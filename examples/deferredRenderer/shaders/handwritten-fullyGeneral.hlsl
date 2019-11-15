// Copyright (c) 2019, The Regents of the University of California,
// Davis campus. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//  * Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
//  * Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//  * Neither the name of The Regents of the University of California,
//    Davis campus nor the names of its contributors may be used to endorse
//    or promote products derived from this software without specific prior
//    written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY
// EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
// PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
// OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

#define NUM_LIGHT_ITERS 1

#define kPI     3.14159265359
#define kINV_PI 0.31830988618379
#define MEDIUMP_FLT_MAX 65504.0f

// Custom structs
struct TileLightList {
  int numPointLight;
  int numShadowedPointLight;
  int idxPointLight[11];
  int idxShadowedPointLight[2];
};

struct DirLight {
  float3 intensity;
  float3 direction;
  float4 cascadeEnds;
  float4x4 vp[3];
};

struct PointLight {
  float3 intensity;
  float3 position;
  float pad;
};

struct ShadowedPointLight {
  float3 intensity;
  float3 position;
  float pad;
};

struct ShadingData {
  float3 diffuse;
  float diffuseWeight;
  float3 normal;
  float3 specular;
  float linearRoughness;
  float3 eyeDir;
  float3 tangent;
  float3 bitangent;
  float3 wPos;
  float3 emissive;
  int materialMask;
};

struct LightSample {
  float3 intensity;
  float3 direction;
  float NdotH;
  float NdotL;
  float LdotH;
};


// Textures and Samplers
uniform Texture2D shadowMapsDirLight[3] : register(t0);
uniform SamplerState shadowMapsDirLight_SAMPLER[3] : register(s0);
uniform TextureCube shadowMapsShadowedPointLight[2] : register(t3);
uniform SamplerState shadowMapsShadowedPointLight_SAMPLER[2] : register(s3);

// Texture Images
RWTexture2D<float4> gbuffers[5] : register(u1);
RWTexture2D<uint4> tileList : register(u6);
RWTexture2D<float4> outImage : register(u7);

// Buffers
RWStructuredBuffer<TileLightList> lightLists : register(u0);

// Constant Buffers 
cbuffer DirLights : register(b0)
{
  int dirLightCount;
  DirLight dirLights[1];
};

cbuffer Lights : register(b1)
{
  float farPlane;
  PointLight allPointLight[11];
  ShadowedPointLight allShadowedPointLight[2];
};

cbuffer Camera : register(b2)
{
  float3 cameraPos;
  int numTilesX;
  float4x4 cameraVP;
};


// Compute Shader Inputs
struct MS_INPUT {
  uint3 MS_SV_GroupID       : SV_GroupID;
  uint3 MS_SV_GroupThreadID : SV_GroupThreadID;
};


///////////////////////////////
// Material Helper Functions //
///////////////////////////////

float3 fresnelSchlick(float3 f0, float3 f90, float u)
{
  return f0 + (f90 - f0) * pow(1.0f - u, 5.0f);
}

float3 evalDiffuseFrostbiteBrdf(float linearRoughness, float NdotV, float3 diffuse, LightSample ls)
{
  float energyBias   = lerp(0.0f, 0.5f, linearRoughness);
  float energyFactor = lerp(1.0f, 1.0f / 1.51f, linearRoughness);

  float3 fd90 = (float3)(energyBias + 2.0f * ls.LdotH * ls.LdotH * linearRoughness);
  float3 fd0  = (float3)(1.0f);

  float lightScatter = fresnelSchlick(fd0, fd90, ls.NdotL).r;
  float viewScatter = fresnelSchlick(fd0, fd90, NdotV).r;

  return (viewScatter * lightScatter * energyFactor * kINV_PI) * diffuse.rgb;
}

float3 evalLambertBrdf(float3 diffuse)
{
  return diffuse.xyz * (1.0f / kPI);
}

float evalGGX(float roughness, float NdotH)
{
  float a2 = roughness * roughness;
  float d = ((NdotH * a2 - NdotH) * NdotH + 1.0f);
  return a2 / (d * d);
}

float evalSmithGGX(float NdotL, float NdotV, float roughness)
{
  float a2 = roughness * roughness;
  float ggxv = NdotL * sqrt((-NdotV * a2 + NdotV) * NdotV + a2);
  float ggxl = NdotV * sqrt((-NdotL * a2 + NdotL) * NdotL + a2);
  return 0.5f / (ggxv + ggxl);
}


//////////////////////////////
// Material Implementations //
//////////////////////////////

float3 evalStandardMaterial(ShadingData sd, LightSample ls)
{
  if(ls.NdotL <= 0.0f)
  {
    return float3(0.0f, 0.0f, 0.0f);
  }

  float NdotV = saturate(dot(sd.normal, sd.eyeDir));

  // diffuse component
  float3 diffuseBrdf = evalDiffuseFrostbiteBrdf(sd.linearRoughness, NdotV, sd.diffuse, ls);
  float3 diffuse = ls.intensity * diffuseBrdf * ls.NdotL;

  // specular component
  float roughness = sd.linearRoughness * sd.linearRoughness;
  float D = evalGGX(roughness, ls.NdotH);
  float G = evalSmithGGX(ls.NdotL, NdotV, roughness);
  float3 F = fresnelSchlick(sd.specular, float3(1.0f, 1.0f, 1.0f), max(0.0f, ls.LdotH));
  float3 specularBrdf = D * G * F * kINV_PI;
  float3 specular = ls.intensity * specularBrdf * ls.NdotL;

  float3 color = diffuse + specular;
  return color;
}


float V_Kelemen(float LoH)
{
  return min(0.25f / (LoH * LoH), MEDIUMP_FLT_MAX);
}

float distributionClearCoat(float roughness, float NdotH)
{
  return evalGGX(roughness, NdotH);
}

float visibilityClearCoat(float roughness, float linearRoughness, float LdotH)
{
  return V_Kelemen(LdotH);
}

float clearCoatLobe(float clearCoatLinearRoughness, float clearCoatRoughness,
                    float NdotH, float LdotH, float Fcc)
{
  float clearCoatNdotH = NdotH;

  float D = distributionClearCoat(clearCoatRoughness, clearCoatNdotH);
  float V = visibilityClearCoat(clearCoatRoughness, clearCoatLinearRoughness, LdotH);
  float F = Fcc;

  return D * V * F;
}

float3 evalStandardMaterialWithClearCoat(ShadingData sd, LightSample ls)
{
  float roughness = (sd.linearRoughness * sd.linearRoughness);
  float Fcc = fresnelSchlick(((float3)(0.04f)), ((float3)(1.0f)), ls.LdotH).x;
  float clearCoat = clearCoatLobe(sd.linearRoughness, roughness, ls.NdotH, ls.LdotH, Fcc);
  float attenuation = 1.0f - Fcc;

  float3 clearCoatColor = clearCoat * ls.intensity * ls.NdotL;

  float3 color = attenuation * evalStandardMaterial(sd, ls);
  color = color + clearCoatColor;

  return color;
}


float distributionCloth(float linearRoughness, float NdotH)
{
  float invAlpha = 1.0f / linearRoughness;
  float cos2h = NdotH * NdotH;
  float sin2h = max(1.0f - cos2h, 0.007812500f);
  return (2.0f + invAlpha) * pow(sin2h, invAlpha * 0.5f) / (2.0f * kPI);
}

float visibilityCloth(float NdotV, float NdotL)
{
  float x = 1.0f / (4.0f * (NdotL + NdotV - NdotL * NdotV));
  return min(x, MEDIUMP_FLT_MAX);
}

float3 evalClothMaterial(ShadingData sd, LightSample ls)
{
  if((ls.NdotL <= 0.0f))
  {
    return float3(0.0f, 0.0f, 0.0f);
  }

  float NdotL = saturate(ls.NdotL);
  float NdotH = saturate(ls.NdotH);
  float LdotH = saturate(ls.LdotH);
  float NdotV = saturate(dot(sd.normal, sd.eyeDir));

  float D = distributionCloth(sd.linearRoughness, NdotH);
  float V = visibilityCloth(NdotV, NdotL);

  float3 sheenColor = sqrt(sd.diffuse);
  float3 f0 = sheenColor;
  float3 f90 = (float3)(saturate(dot(f0, (float3)(50.0f * 0.33f))));
  float3 F = fresnelSchlick(f0, f90, LdotH);

  float3 Fr = (D * V) * F;
  float3 Fd = evalLambertBrdf(sd.diffuse);

  float3 color = Fd + Fr;
  color = color * ls.intensity * NdotL;

  return color;
}

float3 evalSubsurfaceMaterial(ShadingData sd, LightSample ls)
{
  float NdotV = saturate(dot(sd.normal, sd.eyeDir));

  float3 diffuseBrdf = evalDiffuseFrostbiteBrdf(sd.linearRoughness, NdotV, sd.diffuse, ls);

  float3 specularBrdf = (float3)(0.0f);
  if(ls.NdotL > 0.0f)
  {
    float roughness = sd.linearRoughness * sd.linearRoughness;
    float D = evalGGX(roughness, ls.NdotH);
    float G = evalSmithGGX(ls.NdotL, NdotV, roughness);
    float3 F = fresnelSchlick(sd.specular, float3(1.0f, 1.0f, 1.0f), max(0.0f, ls.LdotH));
    specularBrdf = D * G * F * kINV_PI;
  }

  float3 color = (diffuseBrdf + specularBrdf) * ls.NdotL;

  float thickness = 0.5f;
  float3 subsurfaceColor = float3(1.0f, 1.0f, 1.0f);
  float subsurfacePower = 12.234f;

  float scatterVoH = saturate(dot(sd.eyeDir, -ls.direction));
  float forwardScatter = exp2(scatterVoH * subsurfacePower - subsurfacePower);
  float backScatter = saturate(ls.NdotL * thickness + (1.0f - thickness)) * 0.5f;
  float subsurface = lerp(backScatter, 1.0f, forwardScatter) * (1.0f - thickness);
  color = color + (subsurface * evalLambertBrdf(subsurfaceColor));

  color = color * ls.intensity;
  return color;
}


float3 evalMaterial(ShadingData sd, LightSample ls)
{
  if((1 & sd.materialMask) > 0)
  {
    return evalStandardMaterial(sd, ls);
  }
  else if((2 & sd.materialMask) > 0)
  {
    return evalStandardMaterialWithClearCoat(sd, ls);
  }
  else if((4 & sd.materialMask) > 0)
  {
    return evalClothMaterial(sd, ls);
  }
  else if((8 & sd.materialMask) > 0)
  {
    return evalSubsurfaceMaterial(sd, ls);
  }
  else
  {
    return float3(0.0f, 0.0f, 0.0f);
  }
}


/////////////////////
// Light Functions //
/////////////////////

LightSample make_LightSample(float3 intensity, float3 direction, float3 eyeDir, float3 normal)
{
  LightSample ls;
  ls.intensity = intensity;
  ls.direction = direction;

  float3 H = normalize((eyeDir + direction));
  ls.NdotH = dot(normal, H);
  ls.NdotL = dot(normal, direction);
  ls.LdotH = dot(direction, H);

  return ls;
}

float getDistanceFalloff(float distSquared)
{
  return (1.0f / ((0.01f * 0.01f) + distSquared));
}

bool DirLightIsInShadow(DirLight light, Texture2D shadowMap, float3 wPos, float3 normal,
                        float farPlane, int cascadeIdx, SamplerState shadowMap_SAMPLER)
{
  float4 lightSpacePos = mul(light.vp[cascadeIdx], float4(wPos, 1.0f));

  float3 projCoords = lightSpacePos.xyz / lightSpacePos.w;
  projCoords.x = (0.5f *  projCoords.x) + 0.5f;
  projCoords.y = (0.5f * -projCoords.y) + 0.5f;

  float tex = shadowMap.SampleLevel(shadowMap_SAMPLER, projCoords.xy, 0.0f).r;
  float shadowBias = 0.00001f;

  return tex < (projCoords.z - shadowBias);
}

bool ShadowedPointLightIsInShadow(ShadowedPointLight light, TextureCube shadowMap, float3 wPos,
                                  float3 normal, float farPlane, int idx, SamplerState shadowMap_SAMPLER)
{
  float3 fragToLight = wPos - light.position;

  float closestDepth = shadowMap.SampleLevel(shadowMap_SAMPLER, fragToLight, 0.0f).r;
  closestDepth = closestDepth * farPlane;

  float currentDepth = length(fragToLight);
  float bias = 0.05f;

  return currentDepth - bias > closestDepth;
}



float3 illuminateDirLight(DirLight l, ShadingData sd)
{
  LightSample ls = make_LightSample(l.intensity, l.direction, sd.eyeDir, sd.normal);
  return evalMaterial(sd, ls);
}

float3 illuminatePointLight(PointLight l, ShadingData sd)
{
  float3 dir = l.position - sd.wPos;
  float dist = length(dir);

  if(dist >= 25.0f)
  {
    return float3(0.0f, 0.0f, 0.0f);
  }

  float distSquared = dot(dir, dir);

  if(distSquared > 0.00001f)
  {
    dir = normalize(dir);
  }
  else
  {
    dir = float3(0.0f, 0.0f, 0.0f);
  }

  float falloff = getDistanceFalloff(distSquared);
  float3 intensity = l.intensity * falloff;
  LightSample ls = make_LightSample(intensity, dir, sd.eyeDir, sd.normal);

  return evalMaterial(sd, ls);
}

float3 illuminateShadowedPointLight(ShadowedPointLight l, ShadingData sd)
{
  float3 dir = l.position - sd.wPos;
  float dist = length(dir);

  if(dist >= 25.0f)
  {
    return float3(0.0f, 0.0f, 0.0f);
  }

  float distSquared = dot(dir, dir);

  if(distSquared > 0.00001f)
  {
    dir = normalize(dir);
  }
  else
  {
    dir = float3(0.0f, 0.0f, 0.0f);
  }

  float falloff = getDistanceFalloff(distSquared);
  float3 intensity = l.intensity * falloff;
  LightSample ls = make_LightSample(intensity, dir, sd.eyeDir, sd.normal);

  return evalMaterial(sd, ls);
}

float3 illuminate(ShadingData sd, float clipZ, int tileID)
{
  float3 colorAcc = float3(0.0f, 0.0f, 0.0f);

  for(int lightIters = 0; lightIters < NUM_LIGHT_ITERS; lightIters += 1)
  {
    // Dir Lights
    for(int i = 0; i < dirLightCount; i += 1)
    {
      DirLight dirLight = dirLights[i];

      int cascadeIdx = 2;
      for(int j = 2; j > -1; j -= 1)
      {
        if(clipZ <= dirLight.cascadeEnds[j])
        {
          cascadeIdx = j;
        }
      }

      bool isInShadow = false;
      if(cascadeIdx == 2)
      {
        isInShadow = DirLightIsInShadow(dirLight, shadowMapsDirLight[2], sd.wPos, sd.normal,
          farPlane, cascadeIdx, shadowMapsDirLight_SAMPLER[2]);
      }
      else if(cascadeIdx == 1)
      {
        isInShadow = DirLightIsInShadow(dirLight, shadowMapsDirLight[1], sd.wPos, sd.normal,
          farPlane, cascadeIdx, shadowMapsDirLight_SAMPLER[1]);
      }
      else if(cascadeIdx == 0)
      {
        isInShadow = DirLightIsInShadow(dirLight, shadowMapsDirLight[0], sd.wPos, sd.normal,
          farPlane, cascadeIdx, shadowMapsDirLight_SAMPLER[0]);
      }

      if(!isInShadow)
      {
        colorAcc = colorAcc + illuminateDirLight(dirLight, sd);
      }
    }

    // Point Lights
    for(int idx = 0; idx < lightLists[tileID].numPointLight; idx += 1)
    {
      int i = lightLists[tileID].idxPointLight[idx];
      PointLight pointLight = allPointLight[i];
      colorAcc = colorAcc + illuminatePointLight(pointLight, sd);
    }

    // Shadowed Point Lights
    for(int idx = 0; idx < lightLists[tileID].numShadowedPointLight; idx += 1)
    {
      int i = lightLists[tileID].idxShadowedPointLight[idx];
      ShadowedPointLight shadowedPointLight = allShadowedPointLight[i];

      bool isInShadow = false;
      if(i == 1)
      {
        isInShadow = ShadowedPointLightIsInShadow(shadowedPointLight, shadowMapsShadowedPointLight[1],
          sd.wPos, sd.normal, farPlane, i, shadowMapsShadowedPointLight_SAMPLER[1]);
      }
      else if(i == 0)
      {
        isInShadow = ShadowedPointLightIsInShadow(shadowedPointLight, shadowMapsShadowedPointLight[0],
          sd.wPos, sd.normal, farPlane, i, shadowMapsShadowedPointLight_SAMPLER[0]);
      }

      if(!isInShadow)
      {
        colorAcc = colorAcc + illuminateShadowedPointLight(shadowedPointLight, sd);
      }
    }
  }

  return colorAcc;
}


///////////////////
// Main Function //
///////////////////

[numthreads(16,16,1)]
void main(MS_INPUT MS_input)
{
  uint3 MS_SV_GroupID = MS_input.MS_SV_GroupID;
  uint3 MS_SV_GroupThreadID = MS_input.MS_SV_GroupThreadID;

  uint4 tileIDxyzw  = tileList[int2(int(MS_SV_GroupID.x / uint(4)), 63)];
  uint tileID       = tileIDxyzw[int(MS_SV_GroupID.x % uint(4))];

  int2 tileCoords   = int2(int(tileID % uint(numTilesX)), int(tileID / uint(numTilesX)));

  int2 locID = int2(MS_SV_GroupThreadID.xy);

  int2 pixelCoords = (tileCoords * 16) + locID;

  float4 gBuffer0 = gbuffers[0][pixelCoords];
  float4 gBuffer1 = gbuffers[1][pixelCoords];
  float4 gBuffer2 = gbuffers[2][pixelCoords];
  float4 gBuffer3 = gbuffers[3][pixelCoords];
  float4 gBuffer4 = gbuffers[4][pixelCoords];

  ShadingData sd;
  sd.materialMask     = int(gBuffer0.w);
  sd.wPos             = gBuffer0.xyz;
  sd.normal           = normalize(gBuffer1.xyz);
  sd.diffuse          = gBuffer2.xyz;
  sd.diffuseWeight    = gBuffer2.w;
  sd.specular         = gBuffer3.xyz;
  sd.linearRoughness  = max(0.08f, 1.0f - gBuffer3.w);
  sd.emissive         = gBuffer4.xyz;
  sd.eyeDir           = normalize(cameraPos - sd.wPos);

  float3 bt;
  if(abs(sd.normal.x) > abs(sd.normal.y))
  {
    bt = float3(sd.normal.z, 0.0f, -sd.normal.x) / length(sd.normal.xz);
  }
  else
  {
    bt = float3(0.0f, sd.normal.z, -sd.normal.y) / length(sd.normal.yz);
  }

  bt = normalize(bt);

  sd.bitangent = bt;
  sd.tangent = normalize(cross(sd.bitangent, sd.normal));

  float clipZ = mul(cameraVP, float4(sd.wPos, 1.0f)).z;

  float3 colorAcc = illuminate(sd, clipZ, int(tileID));

  colorAcc = colorAcc + sd.emissive;

  float4 color = float4(colorAcc, 1.0f);
  outImage[pixelCoords] = color;
}
