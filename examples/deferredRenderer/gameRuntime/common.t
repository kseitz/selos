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

local Array = require "array"
local Camera = require "camera"
local DDSLoader = require "ddsLoader"
local ImGui = require "imgui"
local IO    = require "io-utilities"
local Map   = require "map"
local Gfx   = require "gfx"
local ObjLoader   = require "assimp"
local SceneLoader = require "scene-loader"
local ShaderCacheLoader = require "shaderCache-loader"
local Quat = require "quat"
local Utils = require "utilities"

local STB = terralib.includec(externalPath .. "stb/stb_image.h")
local stbir = terralib.includecstring([[
#pragma clang diagnostic ignored "-Wunused-value"
#define STB_IMAGE_RESIZE_IMPLEMENTATION 1
#include "]] .. externalPath .. [[stb/stb_image_resize.h"
]])


local C = terralib.includecstring([[
  #include <math.h>
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
]])

local CONSTANTS = require("constants")
local FWD_BIT_FLAGS = CONSTANTS.FWD_BIT_FLAGS

local LightSystem = require("LightSystem")
local DirLight            = LightSystem.LightTypes.byName.DirLight
local PointLight          = LightSystem.LightTypes.byName.PointLight
local ShadowedPointLight  = LightSystem.LightTypes.byName.ShadowedPointLight

local MaterialShader = require "materialShader"
local ShadowShader   = require "shadows"
local LightCulling = require "lightCulling"

local compiledShadersDir = "../build/compiledShaders/"
if gCurrentGraphicsAPIName == "gfx-d3d11" then
  compiledShadersDir = compiledShadersDir .. "D3D11/"
else
  compiledShadersDir = compiledShadersDir .. "OpenGL/"
end

local struct CachedShaderFwd {
  materialID    : int
  program       : Gfx.ShaderProgram
}

local struct CachedShaderDef {
  shaderID      : int
  filename      : rawstring
  program       : Gfx.ComputeShaderProgram
}

local struct RenderState {
  windowHeight        : uint32,
  windowWidth         : uint32,
  clearColor          : vec4,
  wireframe           : bool,

  shadowResolutionDir   : vec2,
  shadowResolutionPoint : vec2,
  shadowSampler       : Gfx.SamplerState,
  shadowFramebuffer   : Gfx.Framebuffer,

  dirShadowShader     : &ShadowShader.dirLightShadow.metadata.Type,
  dirShadowVertexLayout : Gfx.VertexLayout,
  dirShadowProgram    : Gfx.ShaderProgram,
  pointShadowShader     : &ShadowShader.pointLightShadow.metadata.Type,
  pointShadowVertexLayout : Gfx.VertexLayout,
  pointShadowProgram    : Gfx.ShaderProgram,

  projection          : mat4,
  defaultFramebuffer  : Gfx.Framebuffer,
  gbufferFramebuffer  : Gfx.Framebuffer,
  lumFramebuffer      : Gfx.Framebuffer,
  depthStencilState   : Gfx.DepthStencilState,
  defaultRasterizerState    : Gfx.RasterizerState,
  wireframeRasterizerState  : Gfx.RasterizerState,
  shadowRasterizerState     : Gfx.RasterizerState,
  blendState          : Gfx.BlendState,

  fwdShaderCache      : Array(CachedShaderFwd)
  defShaderCache      : Array(CachedShaderDef)
  materialMaskMap     : Array(int)
  maskBuffer          : &vec4

  gbuffers            : Array(Gfx.Texture)
  lumTexture          : Gfx.Texture
  frameTexture        : Gfx.Texture
  deferredShader      : &MaterialShader.deferred.metadata.Type
  quadVB              : Gfx.Buffer
  quadIB              : Gfx.Buffer
  quadShader          : &PipelineInstance
  quadVertexLayout    : Gfx.VertexLayout
  quadProgram         : Gfx.ShaderProgram

  lightCullingShader  : &LightCulling.metadata.Type
  lightCullingProgram : Gfx.ComputeShaderProgram

  lumShader           : &PipelineInstance
  lumProgram          : Gfx.ShaderProgram
}

terra RenderState:init()
  self.windowWidth  = CONSTANTS.DEFAULT_WINDOW_WIDTH
  self.windowHeight = CONSTANTS.DEFAULT_WINDOW_HEIGHT
  self.clearColor    = make_vec4(0, 0, 0, 1)
  self.wireframe     = false

  self.shadowResolutionDir    = make_vec2(1024*4, 1024*4)
  self.shadowResolutionPoint  = make_vec2(1024*2, 1024*2)

  self.maskBuffer = [&vec4](C.malloc(self.windowWidth * self.windowHeight * terralib.sizeof(vec4)))

  self.fwdShaderCache:init()
  self.defShaderCache:init()
  self.materialMaskMap:init()
  self.gbuffers:init()
end


local struct Vertex {
  position  : vec3
  normal    : vec3
  bitangent : vec3
  texCoord  : vec2[CONSTANTS.MAX_TEX_COORDS]
}

local struct Mesh {
  name          : rawstring
  modelID       : int
  materialID    : int
  numVerts      : int
  vertexBuffer  : Gfx.Buffer
  indexBuffer   : Gfx.Buffer
}

local struct Model {
  name            : rawstring
  toWorld         : mat4
  meshes          : Array(Mesh)
}

local struct Material {
  name            : rawstring
  materialShader  : &MaterialShader.forward.metadata.Type
  vertexLayout    : Gfx.VertexLayout
  fwdBitflag      : int
  defBitflag      : int
  meshes          : Array(Mesh)
}

terra Model:init()
  self.name = ""
  self.toWorld = make_mat4(1.0f)
  self.meshes:init()
end


local getTranslation = terralib.overloadedfunction("getTranslation")
getTranslation:adddefinition(terra(x: float, y: float, z: float): mat4
  var mat = make_mat4(1.0f)

  mat.rows[0].w = x
  mat.rows[1].w = y
  mat.rows[2].w = z

  return mat
end)

getTranslation:adddefinition(terra(v: vec3): mat4
  return getTranslation(v.x, v.y, v.z)
end)

local terra perspectiveProjection(left: float, right: float,
                                  bottom: float, top: float,
                                  near: float, far: float)
  var mat = make_mat4(0.0f)

  mat.rows[0].x = (2 * near) / (right - left)
  mat.rows[1].y = (2 * near) / (top - bottom)

  mat.rows[0].z = (right + left) / (right - left)
  mat.rows[1].z = (top + bottom) / (top - bottom)
  mat.rows[2].z = -(far + near) / (far - near)
  mat.rows[3].z = -1.0f

  mat.rows[2].w = -(2 * far * near) / (far - near)

  mat = Gfx.FixupMatrix * mat

  return mat
end

local terra perspectiveProjectionFOV( rad : float, aspect : float, zNear : float, zFar : float)
  var tanHalfFovy = tan(rad / 2.0f);

  var mat = make_mat4(0.0f)
  mat(0)(0) = 1.0f / (aspect * tanHalfFovy);
  mat(1)(1) = 1.0f / (tanHalfFovy);
  mat(2)(2) = - (zFar + zNear) / (zFar - zNear);
  mat(3)(2) = - 1.0f;
  mat(2)(3) = - (2.0f * zFar * zNear) / (zFar - zNear);

  mat = Gfx.FixupMatrix * mat

  return mat
end

local terra orthoProjection(left: float, right: float, bottom: float, top: float, near: float, far: float)
  var mat = make_mat4(1.0f)

  mat.rows[0].x = 2 / (right - left);
  mat.rows[1].y = 2 / (top - bottom);
  mat.rows[2].z = -2 / (far - near);

  mat.rows[0].w = -(right + left) / (right - left);
  mat.rows[1].w = -(top + bottom) / (top - bottom);
  mat.rows[2].w = -(far + near) / (far - near);
  --mat.rows[3].w = 1;

  mat = Gfx.FixupMatrix * mat

  return mat
end

local terra lookAt(eye : vec3, center : vec3, up : vec3)
  var f = normalize(center - eye);
  var u = normalize(up)
  var s = normalize(cross(f, u));
  u = cross(s, f);

  var ret = make_mat4(1.0f)
  ret(0)(0) = s.x;
  ret(0)(1) = s.y;
  ret(0)(2) = s.z;
  ret(1)(0) = u.x;
  ret(1)(1) = u.y;
  ret(1)(2) = u.z;
  ret(2)(0) =-f.x;
  ret(2)(1) =-f.y;
  ret(2)(2) =-f.z;
  ret(0)(3) =-dot(s, eye);
  ret(1)(3) =-dot(u, eye);
  ret(2)(3) = dot(f, eye);

  return ret;
end

local terra loadTextureFromImage( device : &Gfx.Device, width : int, height : int, channels : int, data : &uint8, useSRGB : bool ) : Gfx.Texture

  -- TODO: user might want to override the number of mip levels.
  var mipLevels = [int](C.ceil(C.log2(max(width, height))));

  var format = Gfx.TextureFormat.RGBA8;
  if channels == 1 then
    format = Gfx.TextureFormat.R8;
  elseif channels == 4 then
    format = Gfx.TextureFormat.RGBA8;
  else
    C.printf("error: expected 1 or 4 channels for texture (got %d)\n", channels);
    C.exit(1);
    return Gfx.NilTexture;
  end

  if useSRGB then
    format = Gfx.linearToSrgbFormat(format)
  end

  var builder = device:beginBuildTexture2D(width, height)
    :setFormat(format)
    :setMipLevelCount(mipLevels);

  var levelData : (&uint8)[16];
  if data ~= nil then

    builder:setInitDataForLevel(0, data, width*channels);
    levelData[0] = data;

    var levelWidth = width;
    var levelHeight = height;

    for m = 1,mipLevels do

      var prevWidth = levelWidth;
      var prevHeight = levelHeight;

      levelWidth = levelWidth / 2;
      levelHeight = levelHeight / 2;

      if( levelWidth <= 0 ) then levelWidth = 1; end
      if( levelHeight <= 0 ) then levelHeight = 1; end

      var prevData = levelData[m-1];
      var newData = [&uint8](C.malloc(levelWidth * levelHeight * channels));

      -- resize data from previous level to create next level
      stbir.stbir_resize_uint8(
        prevData, prevWidth,  prevHeight,  0,
        newData,  levelWidth, levelHeight, 0,
        channels);

      builder:setInitDataForLevel(m, newData, levelWidth*channels);
      levelData[m] = newData;
    end
  end

  var texture = builder:endBuild();

  if data ~= nil then
    for m = 1,mipLevels do
      C.free(levelData[m])
    end
  end

  return texture;
end

local terra loadTextureFromImageFile( device : &Gfx.Device, path : rawstring, useSRGB : bool )
  var fileExtension = C.strrchr(path, IO.charLit'.')
  var isDDS = (C.strcmp(fileExtension, ".dds") == 0)

  var texture : Gfx.Texture
  if isDDS then
    texture = DDSLoader.loadDDSTexture(path, device, useSRGB)
    if texture == Gfx.NilTexture then
      C.printf("[Error] Could not load dds texture %s\n", path)
    end
  else
    var texWidth: int = 0
    var texHeight: int = 0
    var n: int = 0

    var texData = STB.stbi_load(path, &texWidth, &texHeight, &n, 4)
    if texData == nil then
      C.printf("[Error] Could not load file %s\n", path)
    end

    texture = loadTextureFromImage(device, texWidth, texHeight, 4, texData, useSRGB);
    STB.stbi_image_free(texData);
  end

  return texture;
end


local terra constructVertexLayout( gfx : &Gfx.Device, p : &PipelineData ) : Gfx.VertexLayout
  var streamIndex = 0; -- TODO: allow attributes to come from different streams!
  var streamOffsets : int[Gfx.MaxVertexStreams];
  for i = 0, Gfx.MaxVertexStreams do
    streamOffsets[i] = 0;
  end

  var builder = gfx:beginBuildVertexLayout();
  for i = 0,p.vertexAttributes.count do
    var info = p.vertexAttributes:get(i);
    var format = info.format;
    var size = info.size;
    var alignment = info.alignment;

    -- TODO: allow individual attributes to specify an offset!
    var offset = streamOffsets[streamIndex];
    offset = (offset + alignment-1) and not (alignment - 1);
    streamOffsets[streamIndex] = offset + size;

    builder:addAttribute(format, streamIndex, offset);
  end

  var layout = builder:endBuild();
  return layout;
end

local terra createMesh( gfx : &Gfx.Device, name: rawstring, modelID : int, materialID : int,
                        vertData : &Array(Vertex), indexData : &Array(uint32),
                        mesh : &Mesh)
  C.memset(mesh, 0, terralib.sizeof(Mesh))

  mesh.name = name
  mesh.modelID = modelID
  mesh.materialID = materialID

  var vertexBuffer = gfx:beginBuildBuffer()
    :setSize(vertData.count * sizeof(Vertex))
    :setInitData(vertData:getraw())
    :setUsage(Gfx.Usage.Immutable)
    :setBindVertexBuffer()
    :endBuild();

  var indexBuffer = gfx:beginBuildBuffer()
    :setSize(indexData.count * sizeof(uint32))
    :setInitData(indexData:getraw())
    :setUsage(Gfx.Usage.Immutable)
    :setBindIndexBuffer()
    :endBuild();

  mesh.numVerts  = indexData.count
  mesh.vertexBuffer = vertexBuffer;
  mesh.indexBuffer = indexBuffer;
end

local isFloatZero = terralib.overloadedfunction("isFloatZero")
isFloatZero:adddefinition(terra(f : float)
  if abs(f) > 0.000000001 then
    return false
  end
  return true
end)
isFloatZero:adddefinition(terra(v : vec3)
  if isFloatZero(v.x) and isFloatZero(v.y) and isFloatZero(v.z) then
    return true
  end
  return false
end)

local terra findMaterialID(materials : &Array(Material), name : rawstring)
  for i=0, materials.count do
    if C.strcmp(materials:get(i).name, name) == 0 then
      return i
    end
  end
  return -1
end

local terra createMaterial(gfx : &Gfx.Device, sampler: Gfx.SamplerState, matl : ObjLoader.Material, materialModel : int)
  var material : Material
  material.name = matl.name
  material.meshes:init()
  material.materialShader  = MaterialShader.forward.new(gfx)
  material.vertexLayout    = constructVertexLayout(gfx, material.materialShader.data)

  var sdr = material.materialShader

  var fwdBitflag : int = 0
  var defBitflag : int = 0

  var emptyTexture = gfx:beginBuildTexture2D(1,1)
    :setFormat(Gfx.TextureFormat.RGBA8)
    :setMipLevelCount(1)
    :setBindFlags(Gfx.BindFlag.SHADER_RESOURCE)
    :endBuild()

  sdr.map_Kd.texture = emptyTexture
  sdr.map_Kd.sampler = sampler
  sdr.map_Ks.texture = emptyTexture
  sdr.map_Ks.sampler = sampler
  sdr.map_Ke.texture = emptyTexture
  sdr.map_Ke.sampler = sampler
  sdr.map_bump.texture = emptyTexture
  sdr.map_bump.sampler = sampler

  defBitflag = defBitflag or materialModel

  if matl.map_Kd ~= nil then
    fwdBitflag = fwdBitflag or FWD_BIT_FLAGS.DIFFUSE_MAP
    sdr.map_Kd.texture = loadTextureFromImageFile(gfx, matl.map_Kd, true)
    sdr.map_Kd.sampler = sampler
  end

  if matl.map_Ks ~= nil then
    fwdBitflag = fwdBitflag or FWD_BIT_FLAGS.SPECULAR_MAP
    -- useSRGB = true  if using SpecGloss shading model
    --         = false if using MetalRough shading model
    var useSRGB = true
    if CONSTANTS.ROUGH_METAL_MODEL then
      useSRGB = false
    end
    sdr.map_Ks.texture = loadTextureFromImageFile(gfx, matl.map_Ks, useSRGB)
    sdr.map_Ks.sampler = sampler
  end

  if matl.map_Ke ~= nil then
    fwdBitflag = fwdBitflag or FWD_BIT_FLAGS.EMISSIVE_MAP
    sdr.map_Ke.texture = loadTextureFromImageFile(gfx, matl.map_Ke, true)
    sdr.map_Ke.sampler = sampler
  end

  if matl.map_bump ~= nil then
    fwdBitflag = fwdBitflag or FWD_BIT_FLAGS.NORMAL_MAP
    sdr.map_bump.texture = loadTextureFromImageFile(gfx, matl.map_bump, false)
    sdr.map_bump.sampler = sampler
  end


  var ctxt = gfx:getImmediateContext()
  var ubo = sdr.MaterialProperties:map(ctxt, Gfx.MapMode.WriteDiscard)
  ubo.Kd = matl.Kd
  ubo.Ks = matl.Ks
  ubo.Ke = matl.Ke
  ubo.kdTexCoordIdx  = matl.kdTexCoordIdx
  ubo.ksTexCoordIdx  = matl.ksTexCoordIdx
  ubo.keTexCoordIdx  = matl.keTexCoordIdx
  ubo.normalTexCoordIdx  = matl.normalTexCoordIdx
  ubo.fwdbitflag = fwdBitflag
  ubo.defbitflag = defBitflag
  sdr.MaterialProperties:unmap(ctxt)

  material.fwdBitflag = fwdBitflag
  material.defBitflag = defBitflag

  return material
end

local terra loadModel(path : rawstring, gfx : &Gfx.Device, materials : &Array(Material),
                      modelID : int, overrideMaterial : rawstring, materialModel : int,
                      materialModelOverrides : &Array(tuple(rawstring, int, bool)))
  var objMesh = ObjLoader.loadMesh(path)
  if objMesh == nil then
    C.printf("Error loading model file '%s'\n", path)
    var model : Model
    model:init()
    return model
  end

  var sampler = gfx:beginBuildSamplerState()
    :setFilter(Gfx.Filter.MinMagMipLinear)
    :endBuild();

  var prevMaterialCount = materials.count
  for matNum = 0, objMesh.materials.count do
    var matl = objMesh.materials:get(matNum)
    var matlModel = materialModel
    for i=0, materialModelOverrides.count do
      var override = materialModelOverrides:get(i)
      if C.strcmp(override._0, matl.name) == 0 then
        matlModel = override._1
        materialModelOverrides:get(i)._2 = true
      end
    end
    materials:add(createMaterial(gfx, sampler, matl, matlModel))
  end

  for i=0, materialModelOverrides.count do
    var override = materialModelOverrides:get(i)
    if override._2 == false then
      C.printf("Warning: Unknown material '%s' specified in material model overrides\n", override._0)
    end
  end

  var overrideMaterialID = -1
  if overrideMaterial ~= nil then
    overrideMaterialID = findMaterialID(materials, overrideMaterial)
  end

  var vertData: Array(Vertex)
  var indexData: Array(uint32)
  vertData:init()
  indexData:init()

  var bbMin = make_vec3(0)
  var bbMax = make_vec3(0)

  var model : Model
  model:init()
  for ff = 0, objMesh.faces.count do
    var face = objMesh.faces:get(ff)
    var firstVert = face.firstVertex
    var startIdx = vertData.count

    for vv = 0, face.vertexCount do
      var vert = objMesh.vertices:get(firstVert + vv)
      var p = objMesh.positions:get(vert.p)
      var n = objMesh.normals:get(vert.n)
      var b = objMesh.bitangents:get(vert.b)

      var v : Vertex
      v.position = p
      v.normal = n
      v.bitangent = b

      if vert.t > 0 and objMesh.texCoords.count > vert.t then
        for i = 0, CONSTANTS.MAX_TEX_COORDS do
          v.texCoord[i] = objMesh.texCoords:get(vert.t)[i]
        end
      end

      vertData:add(v)

      bbMin = min(bbMin, v.position)
      bbMax = max(bbMax, v.position)
    end

    -- Note: This only works correctly for convex polygons
    for vv = 1, face.vertexCount - 1 do
      indexData:add(startIdx)
      indexData:add(startIdx + vv)
      indexData:add(startIdx + vv + 1)
    end

    if ff+1 == objMesh.faces.count or face.material ~= objMesh.faces:get(ff+1).material then

      var mesh : Mesh
      var materialID = face.material + prevMaterialCount
      if overrideMaterialID >= 0 then materialID = overrideMaterialID end
      createMesh(gfx, "", modelID, materialID, &vertData, &indexData, &mesh);
      model.meshes:add(mesh)
      materials:get(materialID).meshes:add(mesh)

      vertData:delete()
      indexData:delete()
      vertData:init()
      indexData:init()
    end
  end


  var bbMidpoint = (bbMax + bbMin) / 2
  model.toWorld = model.toWorld * objMesh.toWorld

  objMesh:delete()
  C.free(objMesh)

  return model
end


local gModelCache = global(Map(rawstring, Model,
  terra(a : rawstring, b : rawstring)
    return C.strcmp(a,b) == 0
  end))


local terra camClipToWorldSpace(camViewProj : mat4)
  var viewFrustum : vec3[8]

  var clipSpace = array(
    make_vec3(-1.0f, 1.0f, 0),
    make_vec3(1.0f, 1.0f, 0),
    make_vec3(1.0f, -1.0f, 0),
    make_vec3(-1.0f, -1.0f, 0),
    make_vec3(-1.0f, 1.0f, 1.0f),
    make_vec3(1.0f, 1.0f, 1.0f),
    make_vec3(1.0f, -1.0f, 1.0f),
    make_vec3(-1.0f, -1.0f, 1.0f)
  )

  var camInv = inverse(camViewProj)
  var center = make_vec3(0f, 0f, 0f)

  for i=0, 8 do
    var crd = camInv * make_vec4(clipSpace[i], 1f)
    viewFrustum[i] = make_vec3(crd) / crd.w
    center = center + viewFrustum[i]
  end

  center = center / 8.0f

  -- Calculate bounding sphere radius
  var radius = 0f
  for i=0, 8 do
    var d : float = length(center - viewFrustum[i])
    radius = max(d, radius)
  end

  return viewFrustum, center, radius
end

local terra getCascadeCropParams(crd : vec3[8], lightVP : mat4)
  -- Transform the frustum into light clip-space and calculate min-max
  var maxCS = make_vec4(-1, -1, 0, 1);
  var minCS = make_vec4(1, 1, 1, 1);
  for i = 0, 8 do
    var c : vec4 = lightVP * make_vec4(crd[i], 1.0f);
    c = c / c.w;
    maxCS = max(maxCS, c);
    minCS = min(minCS, c);
  end

  var delta : vec4 = maxCS - minCS;
  var scale : vec4 = make_vec4(2, 2, 1, 1) / delta;

  var offset : vec4
  offset.x = -0.5f * (maxCS.x + minCS.x) * scale.x;
  offset.y = -0.5f * (maxCS.y + minCS.y) * scale.y;
  offset.z = -minCS.z * scale.z;

  scale.w = 1;
  offset.w = 0;

  return scale, offset
end

local terra calcPssmPartitionEnd(nearPlane : float, camDepthRange : float, distanceRange : vec2,
                                 linearBlend : float, cascade : uint32, cascadeCount : uint32)
  -- Convert to camera space
  var minDepth = nearPlane + distanceRange.x * camDepthRange;
  var maxDepth = nearPlane + distanceRange.y * camDepthRange;

  var depthRange = maxDepth - minDepth;
  var depthScale = maxDepth / minDepth;

  var cascadeScale = float(cascade + 1) / float(cascadeCount);
  var logSplit = pow(depthScale, cascadeScale) * minDepth;
  var uniSplit = minDepth + depthRange * cascadeScale;

  var distance = linearBlend * logSplit + (1 - linearBlend) * uniSplit;

  -- Convert back to clip-space
  distance = (distance - nearPlane) / camDepthRange;
  return distance;
end


local gCurCascadePartitionScheme = global(int32, 2)
local kCascadePartitionSchemes = constant(rawstring[3], `array("Linear", "Logarithmic", "PSSM"))

-- Based on Falcor's CSM implementation (Falcor/Framework/Source/Effects/Shadows/CSM.cpp)
local terra calculateDirLightVPs(light : &DirLight, camera : &Camera, camProj : mat4, update : bool)
  if not update then
    return
  end

  var crd, center, radius = camClipToWorldSpace(camProj * camera:lookAt())

  -- Negate light.direction because it is the direction towards the light, but we want direction of the light
  var view = lookAt(center, center + -light.direction, make_vec3(0, 1, 0))
  var proj = orthoProjection(-radius, radius,-radius, radius,-radius, radius)

  var shadowVP = proj * view

  var nearPlane = camera.zNear
  var farPlane  = camera.zFar
  var depthRange = farPlane - nearPlane

  var distanceRange = make_vec2(0f, 0.02f)
  var pssmLambda    = 0.85f

  var cascadeScale  : vec4[CONSTANTS.NUM_CASCADES]
  var cascadeOffset : vec4[CONSTANTS.NUM_CASCADES]

  var nextCascadeStart = distanceRange.x

  var cascadePartitionScheme : int32 = gCurCascadePartitionScheme
  ImGui.Combo("Cascade Scheme", &cascadePartitionScheme, kCascadePartitionSchemes, 3, -1)
  gCurCascadePartitionScheme = cascadePartitionScheme

  for c=0, CONSTANTS.NUM_CASCADES do
    var cascadeStart = nextCascadeStart

    var cascadeEnd : float
    if cascadePartitionScheme == 0 then
      -- Linear
      nextCascadeStart = cascadeStart + (distanceRange.y - distanceRange.x) / float(CONSTANTS.NUM_CASCADES)
      cascadeEnd = nextCascadeStart
    elseif cascadePartitionScheme == 1 then
      -- Logarithmic
      nextCascadeStart = calcPssmPartitionEnd(nearPlane, depthRange, distanceRange, 1.0f, c, CONSTANTS.NUM_CASCADES)
      cascadeEnd = nextCascadeStart
    elseif cascadePartitionScheme == 2 then
      -- PSSM
      nextCascadeStart = calcPssmPartitionEnd(nearPlane, depthRange, distanceRange, pssmLambda, c, CONSTANTS.NUM_CASCADES)
      cascadeEnd = nextCascadeStart
    else
      Utils.assert(false, "Invalid cascade partitioning scheme")
    end

    -- Calculate the cascade distance in camera-clip space(Where the clip-space range is [0, farPlane])
    var camClipSpaceCascadeStart : float = lerp(nearPlane, farPlane, cascadeStart)
    var camClipSpaceCascadeEnd   : float = lerp(nearPlane, farPlane, cascadeEnd)
    light.cascadeEnds(c) = camClipSpaceCascadeEnd

    -- Calculate the cascade frustum
    var cascadeFrust : vec3[8];
    for i = 0, 4 do
      var edge  : vec3 = crd[i + 4] - crd[i];
      var start : vec3 = edge * cascadeStart;
      var endd  : vec3 = edge * cascadeEnd;
      cascadeFrust[i] = crd[i] + start;
      cascadeFrust[i + 4] = crd[i] + endd;
    end

    cascadeScale[c], cascadeOffset[c] = getCascadeCropParams(cascadeFrust, shadowVP)

    var scaleMat = make_mat4(1.0f)
    scaleMat(0)(0) = cascadeScale[c].x
    scaleMat(1)(1) = cascadeScale[c].y
    scaleMat(2)(2) = cascadeScale[c].z

    var offsetMat = make_mat4(1.0f)
    offsetMat(0)(3) = cascadeOffset[c].x
    offsetMat(1)(3) = cascadeOffset[c].y
    offsetMat(2)(3) = cascadeOffset[c].z

    light.vp[c] = offsetMat * scaleMat * shadowVP
  end
end


local ModelArrayType = Array(Model)
local MaterialArrayType = Array(Material)
local DirLightArrayType = Array(DirLight)
local PointLightArrayType = Array(PointLight)
local ShadowedPointLightArrayType = Array(ShadowedPointLight)
local PointVPArrayType = Array(mat4[6])
local DirectionalShadowArrayType = Array(tuple(Gfx.Texture, Gfx.DepthStencilView))
local PointShadowArrayType = Array(tuple(Gfx.Texture, (Gfx.DepthStencilView)[6]))

local terra loadScene(path : rawstring, gfx : &Gfx.Device, rs : &RenderState)
  : { &Array(Model), &Array(Material), &Array(DirLight), &DirectionalShadowArrayType,
      &Array(PointLight), &Array(ShadowedPointLight), &PointVPArrayType, &PointShadowArrayType, Camera
    }
  gModelCache:delete()
  gModelCache:init()

  var scene = SceneLoader.loadSceneFile(path)

  if scene == nil then
    C.printf("[Error] Could not parse scene file.\n")
    return nil, nil, nil, nil, nil, nil, nil, nil, Camera{}
  end

  var materials : &Array(Material) = [&Array(Material)](C.malloc(terralib.sizeof(MaterialArrayType)))
  materials:init()

  -- TODO override material for entire model
  var models : &Array(Model) = [&Array(Model)](C.malloc(terralib.sizeof(ModelArrayType)))
  models:init()
  for i=0, scene.objects.count do
    var obj = scene.objects:get(i)
    var overrideMaterialID = -1
    if obj.overrideMaterial ~= nil then
      overrideMaterialID = findMaterialID(materials, obj.overrideMaterial)
    end

    var modelLoaded = gModelCache:contains(obj.filename)
    var model : Model
    if modelLoaded then
      model = gModelCache(obj.filename)
      for m = 0, model.meshes.count do
        var mesh = model.meshes:get(m)
        if overrideMaterialID >= 0 then
          mesh.materialID = overrideMaterialID
        end
        mesh.modelID = models.count
        materials:get(mesh.materialID).meshes:add(mesh)
      end
    else
      model = loadModel(obj.filename, gfx, materials, models.count, obj.overrideMaterial, obj.materialModel, &obj.materialModelOverrides)
      gModelCache(obj.filename) = model
    end
    model.name = obj.name
    model.toWorld = obj.transform * model.toWorld
    models:add(model)
  end


  -- Setup the main camera
  var aspect = scene.camera.aspect
  if aspect < 0.01 then
    aspect = float(rs.windowWidth) / float(rs.windowHeight)
  end

  var camera : Camera
  camera:init(scene.camera.position, scene.camera.rotation, scene.camera.sensitivity,
              scene.camera.zNear, scene.camera.zFar, scene.camera.fovY, aspect, scene.path)

  -- Dir Lights
  var dirLights : &Array(DirLight) = [&Array(DirLight)](C.malloc(terralib.sizeof(DirLightArrayType)))
  dirLights:init()

  var dirLightShadows : &DirectionalShadowArrayType = [&DirectionalShadowArrayType](C.malloc(terralib.sizeof(DirectionalShadowArrayType)))
  dirLightShadows:init()

  if scene.dirLights.count ~= 1 then
    C.printf("[Error] Only 1 dir light currently supported\n")
  end

  -- TODO add support for multiple directional lights
  -- for i=0, scene.dirLights.count do
  for i=0, 1 do
    var l = scene.dirLights:get(i)
    var light : DirLight
    light.intensity = l.intensity
    light.direction = -normalize(l.direction)

    dirLights:add(light)

    for j=0, CONSTANTS.NUM_CASCADES do
      var builder = gfx:beginBuildTexture2D(rs.shadowResolutionDir.x, rs.shadowResolutionDir.y)
        :setFormat(Gfx.TextureFormat.DEPTH24_STENCIL8)
        :setMipLevelCount(1)
        :setBindFlags(Gfx.BindFlag.SHADER_RESOURCE or Gfx.BindFlag.DEPTH_STENCIL)
      var tex = builder:endBuild()
      dirLightShadows:add({ tex, gfx:createDepthStencilView(tex) })
    end
  end

  var pointLights : &Array(PointLight) = [&Array(PointLight)](C.malloc(terralib.sizeof(PointLightArrayType)))
  pointLights:init()

  for i=0, scene.pointLights.count do
    var l = scene.pointLights:get(i)
    var light : PointLight
    light.intensity = l.intensity
    light.position = l.position

    pointLights:add(light)
  end

  var shadowedPointLights : &Array(ShadowedPointLight) = [&Array(ShadowedPointLight)](C.malloc(terralib.sizeof(ShadowedPointLightArrayType)))
  shadowedPointLights:init()

  var pointLightVPs : &PointVPArrayType = [&PointVPArrayType](C.malloc(terralib.sizeof(PointVPArrayType)))
  pointLightVPs:init()

  var pointLightShadows : &PointShadowArrayType = [&PointShadowArrayType](C.malloc(terralib.sizeof(PointShadowArrayType)))
  pointLightShadows:init()

  for i=0, scene.shadowedPointLights.count do
    var l = scene.shadowedPointLights:get(i)
    var light : ShadowedPointLight
    light.intensity = l.intensity
    light.position = l.position

    var aspect = [float](rs.shadowResolutionPoint.x) / rs.shadowResolutionPoint.y

    var lightProj = Gfx.CubeFixupMatrix * perspectiveProjectionFOV(radians(90.0f), aspect, 1.0f, CONSTANTS.POINT_LIGHT_FARZ)

    var cubeMap : (tuple(vec3, vec3))[6]
    cubeMap[0] = { make_vec3(1.0,0.0,0.0),  make_vec3(0.0,-1.0,0.0) }
    cubeMap[1] = { make_vec3(-1.0,0.0,0.0), make_vec3(0.0,-1.0,0.0) }
    cubeMap[2] = { make_vec3(0.0,1.0,0.0),  make_vec3(0.0,0.0,1.0)  }
    cubeMap[3] = { make_vec3(0.0,-1.0,0.0), make_vec3(0.0,0.0,-1.0) }
    cubeMap[4] = { make_vec3(0.0,0.0,1.0),  make_vec3(0.0,-1.0,0.0) }
    cubeMap[5] = { make_vec3(0.0,0.0,-1.0), make_vec3(0.0,-1.0,0.0) }

    var vps : mat4[6]
    for i=0, 6 do
      vps[i] = lightProj * lookAt(light.position, light.position + cubeMap[i]._0, cubeMap[i]._1)
    end

    shadowedPointLights:add(light)
    pointLightVPs:add(vps)

    var builder = gfx:beginBuildTextureCubeMap(rs.shadowResolutionPoint.x, rs.shadowResolutionPoint.y)
      :setFormat(Gfx.TextureFormat.DEPTH24_STENCIL8)
      :setMipLevelCount(1)
      :setBindFlags(Gfx.BindFlag.SHADER_RESOURCE or Gfx.BindFlag.DEPTH_STENCIL)
    var tex = builder:endBuild()

    var shadow : tuple(Gfx.Texture, (Gfx.DepthStencilView)[6])
    shadow._0 = tex

    for i=0, 6 do
      shadow._1[i] = gfx:createDepthStencilView(tex, i);
    end

    pointLightShadows:add(shadow)
  end

  scene:delete()
  C.free(scene)

  return models, materials, dirLights, dirLightShadows, pointLights,
    shadowedPointLights, pointLightVPs, pointLightShadows, camera
end


local terra loadShaderCache(path : rawstring, gfx : &Gfx.Device, rs : &RenderState)
  var fwdShaderDescriptors, defShaderDescriptors, matlMap = ShaderCacheLoader.loadShaderCache(path)

  rs.materialMaskMap = matlMap

  if fwdShaderDescriptors == nil then
    C.printf("[Error] Could not load shader cache\n")
    return false
  end

  var e = false
  for i=0, fwdShaderDescriptors.count do
    var desc = fwdShaderDescriptors:get(i)
    var vert = desc.vertexSourceCode
    var frag = desc.fragmentSourceCode
    var program = gfx:createShaderProgram(vert, frag)

    if program == Gfx.NilProgram then
      C.printf("[Error] Could not create shader program for materialID %d.\n", desc.materialID)
      e = true
    else
      var s : CachedShaderFwd
      s.materialID = desc.materialID
      s.program = program
      rs.fwdShaderCache:add(s)
    end
  end
  fwdShaderDescriptors:delete()
  C.free(fwdShaderDescriptors)

  for i=0, defShaderDescriptors.count do
    if i % C.floor(defShaderDescriptors.count * 0.10) == 0 then
      C.printf("Loading material shaders: %.0f%%\n", (i * 100.f) / defShaderDescriptors.count)
    end

    var desc = defShaderDescriptors:get(i)
    var comp = desc.computeSourceCode
    var program = gfx:createComputeProgram(comp)

    if program == Gfx.NilProgram then
      C.printf("[Error] Could not create compute program for shaderID %d (%s).\n", desc.shaderID, desc.filename)
      e = true
    else
      var s : CachedShaderDef
      s.shaderID = desc.shaderID
      s.filename = desc.filename
      s.program = program
      rs.defShaderCache:add(s)
    end
  end

  C.printf("Loading material shaders: 100%%\n")

  defShaderDescriptors:delete()
  C.free(defShaderDescriptors)

  -- shadow shader
  rs.dirShadowVertexLayout = constructVertexLayout(gfx, rs.dirShadowShader.data)
  var dirVert = IO.loadTextFile( [compiledShadersDir ..  "dirShadow.vert"] )
  var dirFrag = IO.loadTextFile( [compiledShadersDir .. "dirShadow.frag"] )
  var dirProgram : Gfx.ShaderProgram = Gfx.NilProgram

  if dirVert == nil or dirFrag == nil then
    -- error already reported
    e = true
  else
    dirProgram = gfx:createShaderProgram(dirVert, dirFrag)
  end

  rs.pointShadowVertexLayout = constructVertexLayout(gfx, rs.pointShadowShader.data)
  var pointVert = IO.loadTextFile( [compiledShadersDir .. "pointShadow.vert"] )
  var pointFrag = IO.loadTextFile( [compiledShadersDir .. "pointShadow.frag"] )
  var pointProgram : Gfx.ShaderProgram = Gfx.NilProgram

  if pointVert == nil or pointFrag == nil then
    -- error already reported
    e = true
  else
    pointProgram = gfx:createShaderProgram(pointVert, pointFrag)
  end

  if dirProgram == Gfx.NilProgram then
    C.printf("[Error] Could not create shader program for directional light shadow shader\n")
    e = true
  else
    rs.dirShadowProgram = dirProgram
  end

  if pointProgram == Gfx.NilProgram then
    C.printf("[Error] Could not create shader program for point light shadow shader\n")
    e = true
  else
    rs.pointShadowProgram = pointProgram
  end

  -- light culling shader
  var cullSrc = IO.loadTextFile( [compiledShadersDir .. "lightCulling.comp"])
  var cullProgram = gfx:createComputeProgram(cullSrc)

  if cullProgram == Gfx.NilProgram then
    C.printf("[Error] Could not create shader program for light culling shader\n")
    e = true
  else
    rs.lightCullingProgram = cullProgram
  end

  -- luminance shader
  var lumVert = IO.loadTextFile( [compiledShadersDir .. "lum.vert"] )
  var lumFrag = IO.loadTextFile( [compiledShadersDir .. "lum.frag"] )
  var lumProgram = gfx:createShaderProgram(lumVert, lumFrag)

  if lumProgram == Gfx.NilProgram then
    C.printf("[Error] Could not create shader program for luminance shader\n")
    e = true
  else
    rs.lumProgram = lumProgram
  end


  -- quad shader
  rs.quadVertexLayout = constructVertexLayout(gfx, rs.quadShader.pipelineData)
  var quadVert = IO.loadTextFile( [compiledShadersDir .. "quad.vert"] )
  var quadFrag = IO.loadTextFile( [compiledShadersDir .. "quad.frag"] )
  var quadProgram = gfx:createShaderProgram(quadVert, quadFrag)

  if quadProgram == Gfx.NilProgram then
    C.printf("[Error] Could not create shader program for quad shader\n")
    e = true
  else
    rs.quadProgram = quadProgram
  end

  return not e
end

return {
  loadScene = loadScene,
  loadShaderCache = loadShaderCache,
  CachedShaderFwd = CachedShaderFwd,
  RenderState = RenderState,
  Model     = Model,
  Material  = Material,
  Mesh      = Mesh,
  Vertex    = Vertex,
  DirLight  = DirLight,
  PointLight = PointLight,
  ShadowedPointLight = ShadowedPointLight,
  perspectiveProjection = perspectiveProjection,
  calculateDirLightVPs = calculateDirLightVPs,
}
