-- Copyright (c) 2019, The Regents of the University of California,
-- Davis campus. All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--  * Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--  * Neither the name of The Regents of the University of California,
--    Davis campus nor the names of its contributors may be used to endorse
--    or promote products derived from this software without specific prior
--    written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
-- OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

local SDP = require("ShadingDataProvider")
local SDE = SDP.Elements

local LightSample = require("LightSystem").LightSample

local Utils = require("utilities")


-- Constants
local kPI = terralib.constant(float, 3.14159265359)
local kINV_PI = terralib.constant(float, 0.31830988618379)
local MEDIUMP_FLT_MAX = terralib.constant(float, 65504.0)


-- Material system

local FEATURE_STATUS = {
  DISABLED  = 0,
  ENABLED   = 1,
}

local MaterialTypes = {
  nextBitflag = 1,
  asList = terralib.newlist(),
  byName = {},
}

-- Materials are mutually exclusive (i.e., a pixel can only be of one material type).
-- Thus, we can only statically specialize if only a single material is enabled.
local function evalMaterialImpl(ShadingData, LightSample, statusList, componentMask)
  local numEnabled = 0
  local lastEnabled = -1

  for i, v in ipairs(statusList) do
    if v ~= FEATURE_STATUS.DISABLED then
      numEnabled = numEnabled + 1
      lastEnabled = i
    end
  end

  local terra evalMaterial(sd : ShadingData, ls : LightSample)
    [(function()

      -- Statically specialize if only one material is enabled
      if numEnabled == 1 then
        local matl = MaterialTypes.asList[lastEnabled]
        return quote
          return [matl.eval(ShadingData, LightSample)](sd, ls)
        end
      end

      local ifStatements = quote return make_vec3(0,0,0) end

      for i = #MaterialTypes.asList, 1, -1 do
        local matl = MaterialTypes.asList[i]
        if statusList[i] == FEATURE_STATUS.ENABLED then
          ifStatements = quote
            if ([matl.bitflag] and sd.materialMask) > 0 then
              return [matl.eval(ShadingData, LightSample)](sd, ls)
            else
              [ifStatements]
            end
          end
        end
      end

      return ifStatements
    end)()]
  end

  return evalMaterial
end

evalMaterialImpl = terralib.memoize(evalMaterialImpl)


local cachedShadingDataTypes = {}

local function getShadingDataType(statusList)
  local entries = {}
  local entriesList = terralib.newlist()

  for i, m in ipairs(MaterialTypes.asList) do
    if statusList[i] ~= FEATURE_STATUS.DISABLED then
      for _, e in ipairs(m.requiredShadingData) do
        if entries[e.field] == nil then
          entries[e.field] = true
          entriesList:insert(e)
        end
      end
    end
  end

  -- TODO: Hardcoding normal and wPos into all ShadingData types for now.
  --       Need to remove this hardcoding when Lights are implemented
  --       as a component type.
  if entries.wPos == nil then
    entries.wPos = true
    entriesList:insert(SDE.wPos)
  end

  if entries.normal == nil then
    entries.normal = true
    entriesList:insert(SDE.normal)
  end

  if entries.eyeDir == nil then
    entries.eyeDir = true
    entriesList:insert(SDE.eyeDir)
  end

  -- TODO: Hardcoding emissive into all ShadingData types for now.
  --       Need to rework how emissive layer works before removing this.
  if entries.emissive == nil then
    entries.emissive = true
    entriesList:insert(SDE.emissive)
  end

  if entries.specular == nil then
    entries.specular = true
    entriesList:insert(SDE.specular)
  end

  if entries.diffuse == nil then
    entries.diffuse = true
    entriesList:insert(SDE.diffuse)
  end

  if entries.linearRoughness == nil then
    entries.linearRoughness = true
    entriesList:insert(SDE.linearRoughness)
  end

  if entries.materialMask == nil then
    entries.materialMask = true
    entriesList:insert(SDE.materialMask)
  end

  -- TODO maybe sort here before getting the cacheKey???
  local cacheKey = entriesList:map("field"):concat()
  local cachedType = cachedShadingDataTypes[cacheKey]

  if cachedType ~= nil then
    return cachedType
  else
    local sdType = terralib.types.newstruct("ShadingData")

    for _, v in ipairs(entriesList) do
      sdType.entries:insert(v)
    end

    cachedShadingDataTypes[cacheKey] = sdType

    return sdType
  end
end


local function registerMaterial(matl)
  if matl.name == nil then
    error("Material Type must have a name", 2)
  end

  if MaterialTypes.byName[matl.name] ~= nil then
    error("Material Type with name '" .. matl.name .. "' already exists.", 2)
  end

  matl.eval = terralib.memoize(matl.eval)
  matl.bitflag = MaterialTypes.nextBitflag
  MaterialTypes.nextBitflag = bit.lshift(MaterialTypes.nextBitflag, 1)
  MaterialTypes.asList:insert(matl)
  MaterialTypes.byName[matl.name] = matl
end


-- Material Evaluation helper functions

local terra fresnelSchlick(f0 : vec3, f90 : vec3, u : float)
  return f0 + (f90 - f0) * pow(1 - u, 5);
end

-- Frostbites's diffuse term. Based on https://seblagarde.files.wordpress.com/2015/07/course_notes_moving_frostbite_to_pbr_v32.pdf
local terra evalDiffuseFrostbiteBrdf(linearRoughness : float, NdotV : float, diffuse : vec3, ls : LightSample)
  var energyBias = lerp(0, 0.5, linearRoughness);
  var energyFactor = lerp(1, 1.0 / 1.51, linearRoughness);

  var fd90 = make_vec3(energyBias + 2 * ls.LdotH * ls.LdotH * linearRoughness);
  var fd0 = make_vec3(1f);
  var lightScatter = fresnelSchlick(fd0, fd90, ls.NdotL).r;
  var viewScatter = fresnelSchlick(fd0, fd90, NdotV).r;
  return (viewScatter * lightScatter * energyFactor * kINV_PI) * diffuse.rgb;
end

local terra evalLambertBrdf(diffuse : vec3)
  return diffuse.rgb * (1 / kPI);
end

local terra evalGGX(roughness : float, NdotH : float)
  var a2 = roughness * roughness;
  var d = ((NdotH * a2 - NdotH) * NdotH + 1);
  return a2 / (d * d);
end

local terra evalSmithGGX(NdotL : float, NdotV : float, roughness : float)
  -- Optimized version of Smith, already taking into account the division by (4 * NdotV)
  var a2 = roughness * roughness;
  -- `NdotV *` and `NdotL *` are inversed. It's not a mistake.
  var ggxv = NdotL * sqrt((-NdotV * a2 + NdotV) * NdotV + a2);
  var ggxl = NdotV * sqrt((-NdotL * a2 + NdotL) * NdotL + a2);
  return 0.5f / (ggxv + ggxl);
end


-- Material Implementations

-- Standard Material
local StandardMaterial = {
  name = "StandardMaterial",
  requiredShadingData   = { SDE.diffuse, SDE.diffuseWeight, SDE.normal, SDE.specular, SDE.linearRoughness, SDE.normal, SDE.eyeDir, SDE.tangent, SDE.bitangent },
  eval =
    function(ShadingData, LightSample)
      local terra evalStandardMaterial(sd : ShadingData, ls : LightSample)
        if ls.NdotL <= 0 then
          return make_vec3(0,0,0)
        end

        var NdotV = saturate(dot(sd.normal, sd.eyeDir))

        -- diffuse component
        -- var diffuseBrdf = evalLambertBrdf(sd.diffuse);
        var diffuseBrdf = evalDiffuseFrostbiteBrdf(sd.linearRoughness, NdotV, sd.diffuse, ls);
        var diffuse = ls.intensity * diffuseBrdf * ls.NdotL;

        -- specular component
        var roughness = sd.linearRoughness * sd.linearRoughness;
        var D = evalGGX(roughness, ls.NdotH);
        var G = evalSmithGGX(ls.NdotL, NdotV, roughness);
        var F = fresnelSchlick(sd.specular, make_vec3(1,1,1), max(0, ls.LdotH));
        var specularBrdf = D * G * F * kINV_PI;
        var specular = ls.intensity * specularBrdf * ls.NdotL;

        var color = diffuse + specular
        return color
      end
      return evalStandardMaterial
    end,
}
registerMaterial(StandardMaterial)


-- From https://github.com/google/filament/blob/master/shaders/src/brdf.fs
--   Kelemen 2001, "A Microfacet Based Coupled Specular-Matte BRDF Model with Importance Sampling"
local terra V_Kelemen(LoH : float)
    return min(0.25f / (LoH * LoH), MEDIUMP_FLT_MAX);
end

local terra distributionClearCoat(roughness : float, NdotH : float)
  return evalGGX(roughness, NdotH)
end

local terra visibilityClearCoat(roughness : float, linearRoughness : float, LdotH : float)
  return V_Kelemen(LdotH)
end

-- From https://github.com/google/filament/blob/master/shaders/src/shading_model_standard.fs
--   Function name: clearCoatLobe
local terra clearCoatLobe(clearCoatLinearRoughness : float, clearCoatRoughness : float,
                          NdotH : float, LdotH : float, Fcc : float)
  var clearCoatNdotH : float = NdotH;

  -- clear coat specular lobe
  var D : float = distributionClearCoat(clearCoatRoughness, clearCoatNdotH);
  var V : float = visibilityClearCoat(clearCoatRoughness, clearCoatLinearRoughness, LdotH);
  var F : float = Fcc

  return (D * V * F)
end

-- Standard Material (from above) with an added Clear Coat layer
--   Clear Coat code based on https://github.com/google/filament/blob/master/shaders/src/shading_model_standard.fs
-- TODO Have separate clear coat normal?
local StandardMaterialWithClearCoat = {
  name = "StandardMaterialWithClearCoat",
  requiredShadingData   = { SDE.diffuse, SDE.diffuseWeight, SDE.normal, SDE.specular, SDE.linearRoughness, SDE.normal, SDE.eyeDir, SDE.tangent, SDE.bitangent },
  eval =
    function(ShadingData, LightSample)
      local terra evalStandardMaterialWithClearCoat(sd : ShadingData, ls : LightSample)
        var roughness = sd.linearRoughness * sd.linearRoughness
        var Fcc = (fresnelSchlick(make_vec3(0.04f), make_vec3(1.0f), ls.LdotH)).x -- fix IOR to 1.5
        var clearCoat = clearCoatLobe(sd.linearRoughness, roughness, ls.NdotH, ls.LdotH, Fcc)
        var attenuation = 1.0f - Fcc

        var clearCoatColor = clearCoat * ls.intensity * ls.NdotL

        var color = attenuation * [StandardMaterial.eval(ShadingData, LightSample)](sd, ls)
        color = color + clearCoatColor

        return color
      end
      return evalStandardMaterialWithClearCoat
    end,
}
registerMaterial(StandardMaterialWithClearCoat)


-- From https://github.com/google/filament/blob/master/shaders/src/brdf.fs
--   Function name: D_Charlie
--   Estevez and Kulla 2017, "Production Friendly Microfacet Sheen BRDF"
local terra distributionCloth(linearRoughness : float, NdotH : float)
  var invAlpha  = 1.0f / linearRoughness
  var cos2h = NdotH * NdotH
  var sin2h = max(1.0f - cos2h, 0.0078125f)
  return (2.0f + invAlpha) * pow(sin2h, invAlpha * 0.5f) / (2.0f * kPI)
end

-- From https://github.com/google/filament/blob/master/shaders/src/brdf.fs
--   Function name: V_Neubelt
--   Neubelt and Pettineo 2013, "Crafting a Next-gen Material Pipeline for The Order: 1886"
local terra visibilityCloth(NdotV : float, NdotL : float)
  var x = 1.0f / (4.0f * (NdotL + NdotV - NdotL * NdotV))
  return min(x, MEDIUMP_FLT_MAX)
end

-- Cloth Material
--   Based on https://github.com/google/filament/blob/master/shaders/src/shading_model_cloth.fs
-- TODO Add subsurface scattering portion
local ClothMaterial = {
  name = "ClothMaterial",
  requiredShadingData   = { SDE.diffuse, SDE.diffuseWeight, SDE.normal, SDE.specular, SDE.linearRoughness, SDE.normal, SDE.eyeDir, SDE.tangent, SDE.bitangent },
  eval =
    function(ShadingData, LightSample)
      local terra evalClothMaterial(sd : ShadingData, ls : LightSample)
        if ls.NdotL <= 0 then
          return make_vec3(0,0,0)
        end

        var NdotL = saturate(ls.NdotL)
        var NdotH = saturate(ls.NdotH)
        var LdotH = saturate(ls.LdotH)
        var NdotV = saturate(dot(sd.normal, sd.eyeDir))

        var D : float = distributionCloth(sd.linearRoughness, NdotH)
        var V : float = visibilityCloth(NdotV, NdotL)

        -- TODO Allow sheenColor to be set separately. Using sqrt(baseColor) as the default for now.
        var sheenColor: vec3 = sqrt(sd.diffuse)
        var f0  : vec3 = sheenColor
        var f90 : vec3 = make_vec3(saturate(dot(f0, make_vec3(50.0f * 0.33f))))
        var F   : vec3 = fresnelSchlick(f0, f90, LdotH)

        -- Ignore pixel.energyCompensation since we use a different BRDF here
        var Fr : vec3 = (D * V) * F;
        var Fd = evalLambertBrdf(sd.diffuse)

        var color : vec3 = Fd + Fr
        color = color * ls.intensity * NdotL

        return color
      end
      return evalClothMaterial
    end,
}
registerMaterial(ClothMaterial)


-- Subsurface Material
--   Subsurface scattering part based on: https://github.com/google/filament/blob/master/shaders/src/shading_model_subsurface.fs
--   Diffuse and Specular BRDFs are the same as used for the StandardMaterial above
local SubsurfaceMaterial = {
  name = "SubsurfaceMaterial",
  requiredShadingData   = { SDE.diffuse, SDE.diffuseWeight, SDE.normal, SDE.specular, SDE.linearRoughness, SDE.normal, SDE.eyeDir, SDE.tangent, SDE.bitangent },
  eval =
    function(ShadingData, LightSample)
      local terra evalSubsurfaceMaterial(sd : ShadingData, ls : LightSample)
        var NdotV = saturate(dot(sd.normal, sd.eyeDir))

        -- diffuse component
        -- var diffuseBrdf = evalLambertBrdf(sd.diffuse);
        var diffuseBrdf = evalDiffuseFrostbiteBrdf(sd.linearRoughness, NdotV, sd.diffuse, ls);

        -- specular component
        var specularBrdf = make_vec3(0.0f)
        if (ls.NdotL > 0.0f) then
          var roughness = sd.linearRoughness * sd.linearRoughness;
          var D = evalGGX(roughness, ls.NdotH);
          var G = evalSmithGGX(ls.NdotL, NdotV, roughness);
          var F = fresnelSchlick(sd.specular, make_vec3(1,1,1), max(0, ls.LdotH));
          specularBrdf = D * G * F * kINV_PI;
        end

        var color = (diffuseBrdf + specularBrdf) * (ls.NdotL)


        -- subsurface scattering
        -- Use a spherical gaussian approximation of pow() for forwardScattering
        -- We could include distortion by adding shading_normal * distortion to light.l

        -- TODO Don't use hardcoded values for the following:
        var thickness       : float = 0.5f -- default taken from Filament
        var subsurfaceColor : vec3  = make_vec3(1f, 1.0f, 1f)
        var subsurfacePower : float = 12.234f -- default taken from Filament

        var scatterVoH      : float = saturate(dot(sd.eyeDir, -ls.direction));
        var forwardScatter  : float = exp2(scatterVoH * subsurfacePower - subsurfacePower);
        var backScatter     : float = saturate(ls.NdotL * thickness + (1.0f - thickness)) * 0.5f;
        var subsurface      : float = mix(backScatter, 1.0f, forwardScatter) * (1.0f - thickness);
        color = color + (subsurface * evalLambertBrdf(subsurfaceColor));

        color = color * ls.intensity
        return color
      end
      return evalSubsurfaceMaterial
    end,
}
registerMaterial(SubsurfaceMaterial)


local TiledDeferredMaterialType = {
  environmentFunctions = {},
  configurationFunctions = {}, -- This will be empty because TiledDeferredMaterialType
                               -- doesn't have any configurationFunctions.
}

function TiledDeferredMaterialType.new()
  local materialType = {
    members = {}, -- This will be empty because TiledDeferredMaterialType
                  -- doesn't have any members.
  }

  setmetatable(materialType, {
    __index = function(table, key)
      if TiledDeferredMaterialType.environmentFunctions[key] ~= nil then
        return TiledDeferredMaterialType.environmentFunctions[key]
      else
        return rawget(table, key)
      end
    end
  })

  return materialType
end

function TiledDeferredMaterialType.environmentFunctions:getDefaultConfiguration()
  local MaterialConfiguration = {}

  MaterialConfiguration.statusList = terralib.newlist()

  for _, v in ipairs(MaterialTypes.asList) do
    MaterialConfiguration.statusList:insert(FEATURE_STATUS.ENABLED)
  end

  function MaterialConfiguration:getConfigurationID()
    local id = 0
    local count = #self.statusList
    for i, v in ipairs(self.statusList) do
      id = id + (math.pow(2, count-i) * self.statusList[i])
    end

    return id, count
  end

  function MaterialConfiguration:ShadingDataType()
    return getShadingDataType(self.statusList)
  end

  function MaterialConfiguration:eval(LightSample)
    local componentMask = Utils.toBitString(self:getConfigurationID())
    return evalMaterialImpl(MaterialConfiguration:ShadingDataType(), LightSample, self.statusList, componentMask)
  end

  -- At least one material type must be enabled for the configuration to be valid
  function MaterialConfiguration:isValid()
    for _, v in ipairs(self.statusList) do
      if v == FEATURE_STATUS.ENABLED then
        return true
      end
    end
    return false
  end

  return MaterialConfiguration
end

function TiledDeferredMaterialType.environmentFunctions:addMembers(pb)
  -- Do nothing
end

return {
  MaterialTypes   = MaterialTypes,
  FEATURE_STATUS  = FEATURE_STATUS,
  TiledDeferredMaterialType = TiledDeferredMaterialType,
}
