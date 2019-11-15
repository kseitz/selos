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

local C = terralib.includecstring([[
  #include <ctype.h>
  #include <time.h>
  #include <math.h>
  #include <memory.h>
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
]])

-- requires
local LightCulling   = require("lightCulling")
local MaterialShader = require("materialShader")
local ShadowShader   = require("shadows")
local QuadShader     = require("quadShader")

require "selos"

local Array   = require "array"
local CONSTANTS = require "constants"
local Camera  = require "camera"
local Common  = require "common"
local Gfx     = require "gfx"
local ImGui   = require "imgui"
local IO      = require "io-utilities"
local SDL     = require "sdl"
local Quat    = require "quat"

local bit = require("bit")
local getCullableLights = require("LightSystem").getCullableLights

local TILE_SIZE = CONSTANTS.TILE_SIZE
local GBUFFER  = require("ShadingDataProvider").GBUFFER_CONSTANTS
local NUM_MATERIAL_TYPES = #(MaterialShader.deferred:getDefaultConfiguration().MaterialType.statusList)
local NUM_LIGHT_TYPES    = #(MaterialShader.deferred:getDefaultConfiguration().LightEnv.statusList)
local NUM_COMPONENTS = NUM_MATERIAL_TYPES + NUM_LIGHT_TYPES
local NUM_CASCADES = CONSTANTS.NUM_CASCADES

local addLicenseInfoButton = require("licenseInfoGUI")

local EXE_NAME = "runGame-"
if gCurrentGraphicsAPIName == "gfx-d3d11" then
  EXE_NAME = EXE_NAME .. "D3D11"
else
  EXE_NAME = EXE_NAME .. "OpenGL"
end
EXE_NAME = EXE_NAME .. ".exe"

local USAGE = "Usage: " .. EXE_NAME .. " [-a or -sN]\n"
USAGE = USAGE .. "  where 0 <= N <= " .. NUM_COMPONENTS .. "\n"

local Timer = Gfx.Timer
local gNumBatches = global(int)

local customLight = global(float[3], `arrayof(float, 1,0,0))
local usingCustomLight = global(bool, false)

-- local structs
local struct GameState {
  sceneFilename   : rawstring
  curShaderCache  : int
  shaderCacheFilename : rawstring[2]
  camera          : Camera
  models          : &Array(Common.Model)
  materials       : &Array(Common.Material)
  dirLights       : &Array(Common.DirLight)
  dirLightShadows : &Array(tuple(Gfx.Texture, Gfx.DepthStencilView))
  pointLights     : &Array(Common.PointLight)
  shadowedPointLights : &Array(Common.ShadowedPointLight)
  pointVPs        : &Array(mat4[6])
  pointLightShadows : &Array(tuple(Gfx.Texture, (Gfx.DepthStencilView)[6]))
  renderShadows   : bool
}

local struct CPUTimer {
  startTime   : C.clock_t
  endTime     : C.clock_t
  elapsedTime : float
  acc         : float
  average     : float
  count       : int
}

terra CPUTimer:init()
  self.elapsedTime = 0
  self.acc         = 0
  self.average     = 0
  self.count       = 0
end

terra CPUTimer:start()
  self.startTime = C.clock()
end

terra CPUTimer:stop()
  self.endTime = C.clock()
end

terra CPUTimer:accumulate()
  var difference : float = self.endTime - self.startTime
  self.elapsedTime = (difference / C.CLOCKS_PER_SEC) * 1000.0f
  self.acc = self.acc + self.elapsedTime
  self.count = self.count + 1
end

terra CPUTimer:updateAverage(numFramesToAverage : int)
  if self.count < numFramesToAverage then
    return
  end
  self.average = self.acc / self.count
  self.acc = 0
  self.count = 0
end

local struct GameTimers {
  time        : float
  numFrames   : int
  frametime   : Timer
  shadowtime  : Timer
  forwardtime : Timer
  tileMaskTime: Timer
  tileLightListTime : Timer
  computetime : Timer
  computetimeCPU : CPUTimer
}

terra GameTimers:init(device : &Gfx.Device)
  self.time = 0
  self.numFrames = 0
  self.frametime:init(device)
  self.shadowtime:init(device)
  self.forwardtime:init(device)
  self.tileMaskTime:init(device)
  self.tileLightListTime:init(device)
  self.computetime:init(device)
  self.computetimeCPU:init()
end

local NUM_FRAMES_PER_AVERAGE = 10

terra GameTimers:update()
  self.numFrames = self.numFrames + 1
  if self.numFrames % NUM_FRAMES_PER_AVERAGE == 0 then
    self.numFrames = 0
  end

  self.frametime:accumulate()
  self.shadowtime:accumulate()
  self.forwardtime:accumulate()
  self.tileMaskTime:accumulate()
  self.tileLightListTime:accumulate()
  self.computetime:accumulate()
  self.computetimeCPU:accumulate()

  self.frametime:updateAverage(NUM_FRAMES_PER_AVERAGE)
  self.shadowtime:updateAverage(NUM_FRAMES_PER_AVERAGE)
  self.forwardtime:updateAverage(NUM_FRAMES_PER_AVERAGE)
  self.tileMaskTime:updateAverage(NUM_FRAMES_PER_AVERAGE)
  self.tileLightListTime:updateAverage(NUM_FRAMES_PER_AVERAGE)
  self.computetime:updateAverage(NUM_FRAMES_PER_AVERAGE)
  self.computetimeCPU:updateAverage(NUM_FRAMES_PER_AVERAGE)
end

local struct QuadVertex {
  position  : vec3
  texCoord  : vec2
}


local cacheDir = "../build/caches/"
local outputDir = "../build/output/"
if gCurrentGraphicsAPIName == "gfx-d3d11" then
  cacheDir = cacheDir .. "D3D11/"
  outputDir = outputDir .. "D3D11/"
else
  cacheDir = cacheDir .. "OpenGL/"
  outputDir = outputDir .. "OpenGL/"
end
local tableOutputFilename = outputDir .. "latexTable.tex"

-- Global variables
local gGameState = global(GameState,
  `GameState {
    "../../assets/scenes/SunTemple_v1.scn",
    -- "../../assets/scenes/SunTemple_v1-path.scn",
    0,
    array(
          [cacheDir .. "shaderCache-fullyGeneral.dat"],
          [cacheDir .. "shaderCache-fullySpecialized.dat"]
         ),
  }
)

local kCacheConfigs = constant(rawstring[2], `array("Fully General", "Fully Specialized"))

local timers = global(GameTimers, `GameTimers { 0 })

local gRenderState = global(Common.RenderState)
local gRS = gRenderState

local gfx = global(Gfx.Device)

local terra getLowerPowerOf2(x : uint32)
  x = x or (x >> 1);
  x = x or (x >> 2);
  x = x or (x >> 4);
  x = x or (x >> 8);
  x = x or (x >> 16);
  return x - (x >> 1);
end

local gUpdateShadowMap = global(bool, true)

local terra updateDirLightVPs(ctxt : &Gfx.Context)
  -- TODO Hardcoded for exactly 1 dir light. Fix this.

  Common.calculateDirLightVPs(&gGameState.dirLights:get(0), &gGameState.camera, gRS.projection, gUpdateShadowMap)

  var lightsUBO = gRS.deferredShader.DirLights:map(ctxt, Gfx.MapMode.WriteDiscard)
  lightsUBO.dirLightCount = gGameState.dirLights.count + terralib.select(usingCustomLight, 1, 0)

  lightsUBO.dirLights[0] = gGameState.dirLights:get(0)

  if usingCustomLight then
    var light : Common.DirLight
    light.intensity = make_vec3(5,5,5)
    var v = normalize(make_vec3(customLight[0], customLight[1], customLight[2]))
    customLight[0] = v.x
    customLight[1] = v.y
    customLight[2] = v.z
    light.direction = v
    lightsUBO.dirLights[gGameState.dirLights.count] = light
  end

  gRS.deferredShader.DirLights:unmap(ctxt)
end

local terra setupLights(ctxt : &Gfx.Context)
  gGameState.renderShadows = true
  var lightsUBO = gRS.deferredShader.Lights:map(ctxt, Gfx.MapMode.WriteDiscard)

  -- TODO Hardcoded for exactly 1 dir light. Fix this.

  -- for i=0, gGameState.dirLights.count do
    -- lightsUBO.dirLights[i] = gGameState.dirLights:get(i)
  for i=0, NUM_CASCADES do
    gRS.deferredShader.shadowMapsDirLight[i].texture = gGameState.dirLightShadows:get(i)._0
    gRS.deferredShader.shadowMapsDirLight[i].sampler = gRS.shadowSampler
  end

  for i=0, gGameState.pointLights.count do
    lightsUBO.allPointLight[i] = gGameState.pointLights:get(i)
  end

  for i=0, gGameState.shadowedPointLights.count do
    lightsUBO.allShadowedPointLight[i] = gGameState.shadowedPointLights:get(i)

    gRS.deferredShader.shadowMapsShadowedPointLight[i].texture = gGameState.pointLightShadows:get(i)._0
    gRS.deferredShader.shadowMapsShadowedPointLight[i].sampler = gRS.shadowSampler
  end
  lightsUBO.farPlane = CONSTANTS.POINT_LIGHT_FARZ -- TODO hardcoded for now
  gRS.deferredShader.Lights:unmap(ctxt)

  var cullLightsUBO = gRS.lightCullingShader.Lights:map(ctxt, Gfx.MapMode.WriteDiscard)
  cullLightsUBO.pointLightCount = gGameState.pointLights.count
  cullLightsUBO.shadowedPointLightCount = gGameState.shadowedPointLights.count

  for i=0, gGameState.pointLights.count do
    cullLightsUBO.pointLights[i] = gGameState.pointLights:get(i)
  end
  for i=0, gGameState.shadowedPointLights.count do
    cullLightsUBO.shadowedPointLights[i] = gGameState.shadowedPointLights:get(i)
  end

  gRS.lightCullingShader.Lights:unmap(ctxt)
end

local terra setupCamera(ctxt : &Gfx.Context, numTilesX : int)
  var cameraUBO = gRS.deferredShader.Camera:map(ctxt, Gfx.MapMode.WriteDiscard)
  cameraUBO.cameraPos = gGameState.camera.position
  cameraUBO.numTilesX = numTilesX
  cameraUBO.cameraVP  = gRS.projection * gGameState.camera:lookAt()

  gRS.deferredShader.Camera:unmap(ctxt)
end

local terra updateTileLightLists(ctxt : &Gfx.Context, numTiles : int)
  timers.tileLightListTime:start()

  var numDispatches = numTiles / CONSTANTS.LIGHT_CULLING_NUM_THREADS +
    terralib.select(numTiles / CONSTANTS.LIGHT_CULLING_NUM_THREADS == 0, 0, 1)

  ctxt:memoryBarrier(Gfx.BarrierFlags.SHADER_IMAGE_ACCESS)

  gRS.lightCullingShader:bind(ctxt)
  ctxt:setShaderProgram(gRS.lightCullingProgram)
  ctxt:dispatchCompute(numDispatches,1,1)
  ctxt:memoryBarrier(Gfx.BarrierFlags.SHADER_BUFFER)

  var tiles = gRS.lightCullingShader.lightLists:copyToHost(&gfx)

  var lightMask = [&int](C.malloc(numTiles * sizeof(int)))
  for i = 0, numTiles do
    var mask = 0

    [(function()
      local quoteList = terralib.newlist()

      local cullableLights = getCullableLights()
      for k, v in ipairs(cullableLights) do
        quoteList:insert(quote
          if tiles[i].["num" .. v.name] > 0 then
            mask = mask or [bit.lshift(1, k-1)]
          end
        end)
      end

      return quoteList
    end)()]

    lightMask[i] = mask
  end

  C.free(tiles)

  timers.tileLightListTime:stop()

  return lightMask
end

local terra reloadSceneFileUI(ctxt : &Gfx.Context)
  if ImGui.Button("Reload Scene") then
    gGameState.renderShadows = true
    gGameState.models:delete()
    C.free(gGameState.models)
    gGameState.models = nil

    gGameState.materials:delete()
    C.free(gGameState.materials)
    gGameState.materials = nil

    gGameState.dirLights:delete()
    C.free(gGameState.dirLights)
    gGameState.dirLights = nil

    -- TODO Hardcoded for exactly 1 dir light. Fix this.

    -- for i=0, gGameState.dirLightShadows.count do
    for i=0, NUM_CASCADES do
      gfx:deleteTexture(gGameState.dirLightShadows:get(i)._0)
    end
    gGameState.dirLightShadows:delete()
    C.free(gGameState.dirLightShadows)
    gGameState.dirLightShadows = nil

    gGameState.pointLights:delete()
    C.free(gGameState.pointLights)
    gGameState.pointLights = nil

    gGameState.shadowedPointLights:delete()
    C.free(gGameState.shadowedPointLights)
    gGameState.shadowedPointLights = nil

    gGameState.pointVPs:delete()
    C.free(gGameState.pointVPs)
    gGameState.pointVPs = nil

    for i=0, gGameState.pointLightShadows.count do
      gfx:deleteTexture(gGameState.pointLightShadows:get(i)._0)
    end
    gGameState.pointLightShadows:delete()
    C.free(gGameState.pointLightShadows)
    gGameState.pointLightShadows = nil

    gGameState.models, gGameState.materials, gGameState.dirLights, gGameState.dirLightShadows, gGameState.pointLights, gGameState.shadowedPointLights,
      gGameState.pointVPs, gGameState.pointLightShadows, gGameState.camera = Common.loadScene(gGameState.sceneFilename, &gfx, &gRS)
    if gGameState.models == nil then
      C.printf("[Error] Could not reload scene file: %s\n", gGameState.sceneFilename)
    end

    setPerspectiveProjection()

    setupLights(ctxt)
  end
end

local terra reloadShaderCacheUI()
  gGameState.renderShadows = true
  gRS.fwdShaderCache.count = 0
  gRS.defShaderCache.count = 0
  gRS.materialMaskMap:delete()
  Common.loadShaderCache(gGameState.shaderCacheFilename[gGameState.curShaderCache], &gfx, &gRS)
end

local terra initShaders()
  return Common.loadShaderCache(gGameState.shaderCacheFilename[gGameState.curShaderCache], &gfx, &gRS)
end

local terra initQuad()
  var ctxt = gfx:getImmediateContext()

  var luminanceTexture = gfx:beginBuildTexture2D(getLowerPowerOf2(gRS.windowWidth), getLowerPowerOf2(gRS.windowHeight))
    :setFormat(Gfx.TextureFormat.R16F)
    :setMipLevelCount(0)
    :enableMipGeneration()
    :setBindFlags(Gfx.BindFlag.SHADER_RESOURCE or Gfx.BindFlag.RENDER_TARGET)
  :endBuild();

  gRS.lumFramebuffer     = gfx:generateFramebuffer();
  ctxt:setFramebuffer(gRS.lumFramebuffer)
  gfx:bindRenderTargetView(gfx:createRenderTargetView(luminanceTexture), 0)

  var frameTexture = gfx:beginBuildTexture2D(gRS.windowWidth, gRS.windowHeight)
    :setFormat(Gfx.TextureFormat.RGBA16F)
    :setMipLevelCount(1)
    :setBindFlags(Gfx.BindFlag.SHADER_RESOURCE or Gfx.BindFlag.UNORDERED_ACCESS)
  :endBuild();

  var lumShader = QuadShader.LuminanceShader.new(&gfx);
  lumShader.tex.texture = frameTexture;
  lumShader.tex.sampler = gfx:beginBuildSamplerState()
    :setAddressModeU(Gfx.TextureAddressMode.Clamp)
    :setAddressModeV(Gfx.TextureAddressMode.Clamp)
    :setAddressModeW(Gfx.TextureAddressMode.Clamp)
    :setFilter(Gfx.Filter.MinMagLinear_MipPoint)
  :endBuild();

  var quadShader = QuadShader.QuadShader.new(&gfx);
  quadShader.tex.texture = frameTexture;
  quadShader.tex.sampler = gfx:beginBuildSamplerState()
    :setAddressModeU(Gfx.TextureAddressMode.Clamp)
    :setAddressModeV(Gfx.TextureAddressMode.Clamp)
    :setAddressModeW(Gfx.TextureAddressMode.Clamp)
    :setFilter(Gfx.Filter.MinMagMipPoint)
  :endBuild();
  quadShader.lumTex.texture = luminanceTexture;
  quadShader.lumTex.sampler = gfx:beginBuildSamplerState()
    :setAddressModeU(Gfx.TextureAddressMode.Clamp)
    :setAddressModeV(Gfx.TextureAddressMode.Clamp)
    :setAddressModeW(Gfx.TextureAddressMode.Clamp)
    :setFilter(Gfx.Filter.MinMagLinear_MipPoint)
  :endBuild();

  var vertexData  : Array(QuadVertex)
  var indexData   : Array(uint32)
  vertexData:init()
  indexData:init()

  vertexData:add(QuadVertex{vec3{-1,-1,0}, vec2{0,0}})
  vertexData:add(QuadVertex{vec3{ 1,-1,0}, vec2{1,0}})
  vertexData:add(QuadVertex{vec3{-1, 1,0}, vec2{0,1}})
  vertexData:add(QuadVertex{vec3{ 1, 1,0}, vec2{1,1}})

  indexData:add(0)
  indexData:add(1)
  indexData:add(2)

  indexData:add(2)
  indexData:add(1)
  indexData:add(3)

  var vertexBuffer = gfx:beginBuildBuffer()
    :setSize(vertexData.count * sizeof(QuadVertex))
    :setInitData(vertexData:getraw())
    :setUsage(Gfx.Usage.Immutable)
    :setBindVertexBuffer()
  :endBuild()

  var indexBuffer = gfx:beginBuildBuffer()
    :setSize(indexData.count * sizeof(uint32))
    :setInitData(indexData:getraw())
    :setUsage(Gfx.Usage.Immutable)
    :setBindIndexBuffer()
  :endBuild()

  gRS.frameTexture = frameTexture
  gRS.lumTexture = luminanceTexture
  gRS.quadShader = quadShader
  gRS.quadVB = vertexBuffer
  gRS.quadIB = indexBuffer
  gRS.lumShader = lumShader
end

local terra initGame()
  if SDL.SDL_Init(SDL.SDL_INIT_VIDEO) < 0 then
    C.printf("[Error] Could not initialize SDL: %s\n", SDL.SDL_GetError())
    return nil
  end

  gRS:init()

  var windowFlags =
    Gfx.getWindowFlags() or
    SDL.SDL_WINDOW_SHOWN or
    SDL.SDL_WINDOW_RESIZABLE;

  var window: &SDL.SDL_Window = SDL.SDL_CreateWindow("Selos", 100, 100,
    gRenderState.windowWidth, gRenderState.windowHeight,
    windowFlags);
  if window == nil then
    C.printf("[Error] Could not create SDL window: %s\n", SDL.SDL_GetError())
    return nil
  end

  -- TODO: specify properties of default framebuffer!!!
  gfx:init( window );

  gRS.defaultFramebuffer = gfx:getDefaultFramebuffer();
  gRS.gbufferFramebuffer = gfx:generateFramebuffer();
  gRS.shadowFramebuffer  = gfx:generateFramebuffer();

  gRS.dirShadowShader = ShadowShader.dirLightShadow.new(&gfx);
  gRS.pointShadowShader = ShadowShader.pointLightShadow.new(&gfx);

  var deferredShader = MaterialShader.deferred.new(&gfx);

  var ctxt = gfx:getImmediateContext()
  ctxt:setFramebuffer(gRS.gbufferFramebuffer)
  -- TODO Some or all of this code should be provided by GBufferShadingData,
  --      rather than being hardcoded here.
  for i=0,5 do
    var gbuffer = gfx:beginBuildTexture2D(gRS.windowWidth, gRS.windowHeight)
      :setFormat(Gfx.TextureFormat.RGBA32F)
      :setMipLevelCount(1)
      :setBindFlags(Gfx.BindFlag.SHADER_RESOURCE or Gfx.BindFlag.UNORDERED_ACCESS or Gfx.BindFlag.RENDER_TARGET)
    :endBuild()
    gRS.gbuffers:add(gbuffer)
    gfx:bindRenderTargetView(gfx:createRenderTargetView(gbuffer), i)
    deferredShader.gbuffers[i] = gbuffer
  end

  var depthTexture = gfx:beginBuildTexture2D(gRS.windowWidth, gRS.windowHeight)
    :setFormat(Gfx.TextureFormat.DEPTH24_STENCIL8)
    :setMipLevelCount(1)
    :setBindFlags(Gfx.BindFlag.DEPTH_STENCIL)
    :endBuild()
  gfx:bindDepthStencilView(gfx:createDepthStencilView(depthTexture))

  ctxt:setFramebuffer(gRS.defaultFramebuffer)

  gRS.shadowSampler = gfx:beginBuildSamplerState()
    :setAddressModeU(Gfx.TextureAddressMode.Clamp)
    :setAddressModeV(Gfx.TextureAddressMode.Clamp)
    :setAddressModeW(Gfx.TextureAddressMode.Clamp)
  :endBuild();

  initQuad()

  deferredShader.outImage = gRS.frameTexture

  var lightCullingShader = LightCulling.new(&gfx)
  lightCullingShader.wPos = deferredShader.gbuffers[0]

  var tileInfoUBO = lightCullingShader.TileInfo:map(ctxt, Gfx.MapMode.WriteDiscard)
  tileInfoUBO.numTilesX = (gRS.windowWidth / TILE_SIZE)  + terralib.select(gRS.windowWidth  % TILE_SIZE == 0, 0, 1)
  lightCullingShader.TileInfo:unmap(ctxt)

  deferredShader.lightLists:replaceWith(&gfx, &lightCullingShader.lightLists)

  gRS.lightCullingShader = lightCullingShader
  gRS.deferredShader = deferredShader

  if not initShaders() then
    C.printf("[Error] Could not initialize shaders.\n")
    SDL.SDL_DestroyWindow(window)
    return nil
  end

  ImGui.ImGui_ImplSdl_Init(window, &gfx);

  return window
end


local terra setPerspectiveProjection()
  var zNear = gGameState.camera.zNear
  var zFar  = gGameState.camera.zFar
  var aspect = gGameState.camera.aspect
  var fovY = gGameState.camera.fovY

  var fH = C.tanf( (fovY / 2) ) * zNear
  var fW = fH * aspect

  gRS.projection = Common.perspectiveProjection(-fW, fW, -fH, fH, zNear, zFar)
end


local terra powi(base : int, exp : int)
  var ret = base
  for i=1, exp do
    ret = ret * ret
  end
end

local gBenchmarkEnabled = terralib.global(bool, false)
local gFrameNumber = terralib.global(int, 0)
local gShowLicenseInfo = terralib.global(bool, false)

local GBUFFER_CLEAR_COLOR = terralib.constant(vec4, `vec4{0,0,0,1})
local terra renderFrame(ctxt : &Gfx.Context)
  timers.frametime:start()
  ImGui.Begin("Selos", nil, 0)

  addLicenseInfoButton()

  gGameState.camera:animate(timers.time)

  ctxt:setDepthStencilState(gRS.depthStencilState)
  ctxt:setRasterizerState(gRS.shadowRasterizerState)
  ctxt:setBlendState(gRS.blendState)

  -- Dir light shadow pass
  ImGui.Checkbox("gUpdateShadowMap", &gUpdateShadowMap)

  timers.shadowtime:start()
  updateDirLightVPs(ctxt)
  ctxt:setFramebuffer(gRS.shadowFramebuffer)

  ctxt:setVertexLayout(gRS.dirShadowVertexLayout)
  ctxt:setShaderProgram(gRS.dirShadowProgram)
  var dirSdr = gRS.dirShadowShader

  -- TODO Hardcoded for exactly 1 dir light. Fix this.

  -- for i=0, gGameState.dirLights.count do
    -- var l = gGameState.dirLights:get(i)
  for i=0, NUM_CASCADES do
    var l = gGameState.dirLights:get(0)
    ctxt:setViewport(0, 0, gRS.shadowResolutionDir.x, gRS.shadowResolutionDir.y)
    gfx:bindDepthStencilView(gGameState.dirLightShadows:get(i)._1)
    ctxt:clearFramebuffer(gRS.clearColor, 1.0, 0, Gfx.CLEAR_DEPTH)

    for j=0, gGameState.materials.count do
      var matl = &gGameState.materials:get(j)
      var curModelID = -1
      for k=0, matl.meshes.count do
        var mesh = matl.meshes:get(k)

        if mesh.modelID ~= curModelID then
          curModelID = mesh.modelID
          var model = &gGameState.models:get(curModelID)
          var m = model.toWorld
          dirSdr.mvp_param = l.vp[i] * m
          dirSdr:setup(ctxt)
          dirSdr.Uniforms:setup(ctxt)
        end

        ctxt:setVertexBuffer(0, mesh.vertexBuffer, terralib.sizeof(Common.Vertex), 0)
        ctxt:setIndexBuffer(mesh.indexBuffer, Gfx.IndexFormat.UInt32)
        ctxt:drawIndexed(Gfx.PrimitiveType.Triangles, mesh.numVerts)
      end
    end
  end

  -- Point light shadow pass
  if gGameState.renderShadows then
    ctxt:setRasterizerState(gRS.defaultRasterizerState)
    ctxt:setVertexLayout(gRS.pointShadowVertexLayout)
    ctxt:setShaderProgram(gRS.pointShadowProgram)
    var pointSdr = gRS.pointShadowShader
    for i=0, gGameState.shadowedPointLights.count do
      var l = gGameState.shadowedPointLights:get(i)
      var lvp = gGameState.pointVPs:get(i)
      ctxt:setViewport(0, 0, gRS.shadowResolutionPoint.x, gRS.shadowResolutionPoint.y)

      for ii = 0, 6 do
        gfx:bindDepthStencilView(gGameState.pointLightShadows:get(i)._1[ii], ii)
        ctxt:clearFramebuffer(gRS.clearColor, 1.0, 0, Gfx.CLEAR_DEPTH)

        for j=0, gGameState.materials.count do
          var matl = &gGameState.materials:get(j)
          var curModelID = -1
          for k=0, matl.meshes.count do
            var mesh = matl.meshes:get(k)

            if mesh.modelID ~= curModelID then
              curModelID = mesh.modelID
              var model = &gGameState.models:get(curModelID)
              var m = model.toWorld
              pointSdr.mvp_param = lvp[ii] * m
              pointSdr.m_param = m
              pointSdr.lightPos_param = l.position
              pointSdr.farPlane_param = CONSTANTS.POINT_LIGHT_FARZ -- TODO hardcoded for now
              pointSdr:setup(ctxt)
              pointSdr.Uniforms:setup(ctxt)
            end

            ctxt:setVertexBuffer(0, mesh.vertexBuffer, terralib.sizeof(Common.Vertex), 0)
            ctxt:setIndexBuffer(mesh.indexBuffer, Gfx.IndexFormat.UInt32)
            ctxt:drawIndexed(Gfx.PrimitiveType.Triangles, mesh.numVerts)
          end
        end
      end
    end
    gGameState.renderShadows = false
  end
  timers.shadowtime:stop()

  -- Render to GBuffers
  timers.forwardtime:start()
  ctxt:setRasterizerState(gRS.defaultRasterizerState)
  ctxt:setFramebuffer(gRS.gbufferFramebuffer)
  ctxt:setViewport(0,0,gRS.windowWidth, gRS.windowHeight)
  ctxt:clearFramebuffer(GBUFFER_CLEAR_COLOR, 1.0f, 0, Gfx.CLEAR_COLOR or Gfx.CLEAR_DEPTH)

  var v = gGameState.camera:lookAt()
  var p = gRS.projection

  var curProgram : Gfx.ShaderProgram = Gfx.NilProgram

  for i=0, gGameState.materials.count do
    var matl = &gGameState.materials:get(i)
    var sdr = gRS.fwdShaderCache:get(0) -- TODO hardcoded for now

    if curProgram ~= sdr.program then
      curProgram = sdr.program
      ctxt:setShaderProgram(curProgram)
    end

    var curModelID = -1
    for j=0, matl.meshes.count do
      var mesh = matl.meshes:get(j)

      if mesh.modelID ~= curModelID then
        curModelID = mesh.modelID
        var model = &gGameState.models:get(curModelID)
        var m = model.toWorld
        matl.materialShader.p = p
        matl.materialShader.v = v
        matl.materialShader.model_param = m
        matl.materialShader:setup(ctxt)
        matl.materialShader.Uniforms:setup(ctxt)
      end

      ctxt:setVertexLayout(matl.vertexLayout)
      ctxt:setVertexBuffer(0, mesh.vertexBuffer, terralib.sizeof(Common.Vertex), 0)
      ctxt:setIndexBuffer(mesh.indexBuffer, Gfx.IndexFormat.UInt32)
      ctxt:drawIndexed(Gfx.PrimitiveType.Triangles, mesh.numVerts)
    end
  end
  timers.forwardtime:stop()

  var numTilesX = (gRS.windowWidth / TILE_SIZE)  + terralib.select(gRS.windowWidth  % TILE_SIZE == 0, 0, 1)
  var numTilesY = (gRS.windowHeight / TILE_SIZE) + terralib.select(gRS.windowHeight % TILE_SIZE == 0, 0, 1)
  var numTilesReal = numTilesX * numTilesY
  var numTiles = numTilesReal
  if numTilesReal % 4 ~= 0 then
    var tmp = numTilesReal % 4
    numTiles = numTiles + (4 - tmp)
  end

  var tileLightMasks = updateTileLightLists(ctxt, numTilesReal)

  -- calculate material masks for each tile
  timers.tileMaskTime:start()
  var maskBuffer = gRS.maskBuffer
  -- TODO Some or all of this code should be provided by GBufferShadingData,
  --      rather than being hardcoded here.
  gfx:copyTextureToHost(gRS.gbuffers:get([GBUFFER.MATERIAL_MASK.idx]), 0, Gfx.TextureFormat.RGBA32F,
                        gRS.windowWidth, gRS.windowHeight, maskBuffer)

  var tileMasksOR : &int
  var tileMasksAND : &int
  tileMasksOR  = [&int](C.malloc(numTiles * terralib.sizeof(int)))
  tileMasksAND = [&int](C.malloc(numTiles * terralib.sizeof(int)))
  C.memset(tileMasksOR,  0, numTiles * terralib.sizeof(int))
  C.memset(tileMasksAND, -1, numTiles * terralib.sizeof(int))

  for y=0, gRS.windowHeight do
    var tileIDy = y / TILE_SIZE
    for x=0, gRS.windowWidth do
      var tileIDx = x / TILE_SIZE
      var tileID = tileIDy * numTilesX + tileIDx

      var idx = y * gRS.windowWidth + x
      var mask : int = maskBuffer[idx].[GBUFFER.MATERIAL_MASK.value]

      tileMasksOR[tileID] = tileMasksOR[tileID] or mask
      tileMasksAND[tileID] = tileMasksAND[tileID] and mask
    end
  end

  var numShaders : int = C.pow(2, NUM_COMPONENTS)

  var shaderTileLists : &uint16
  shaderTileLists = [&uint16](C.malloc(numShaders * numTiles * terralib.sizeof(uint16)))
  C.memset(shaderTileLists, 0, numShaders * numTiles * terralib.sizeof(uint16))

  var shaderTileCounts : &int
  shaderTileCounts = [&int](C.malloc(numShaders * terralib.sizeof(int)))
  C.memset(shaderTileCounts, 0, numShaders * terralib.sizeof(int))

  var shadersToRun : Array(int)
  shadersToRun:init()

  for i=0, numTilesReal do
    var maskOR = tileMasksOR[i]
    var maskAND = tileMasksAND[i]
    -- var mask = (maskAND << NUM_COMPONENTS) or maskOR
    var mask = maskOR

    mask = mask or (tileLightMasks[i] << NUM_MATERIAL_TYPES)

    var sdrIndex = gRS.materialMaskMap:get(mask)
    var sdrID = gRS.defShaderCache:get(sdrIndex).shaderID
    var loc = shaderTileCounts[sdrIndex]
    if loc == 0 then
      shadersToRun:add(sdrIndex)
    end
    shaderTileCounts[sdrIndex] = loc + 1
    shaderTileLists[sdrID * numTiles + loc] = i
  end

  C.free(tileMasksOR)
  C.free(tileMasksAND)
  C.free(tileLightMasks)

  -- TODO shouldn't create this every frame
  var tileListTexture = gfx:beginBuildTexture2D(numTiles/4, numShaders)
    :setFormat(Gfx.TextureFormat.RGBA16U)
    :setMipLevelCount(1)
    :setBindFlags(Gfx.BindFlag.SHADER_RESOURCE or Gfx.BindFlag.UNORDERED_ACCESS)
    :endBuild()

  gfx:updateTextureData(tileListTexture, Gfx.TextureFormat.RGBA16U, numTiles/4, numShaders, 0, shaderTileLists, 4)

  gRS.deferredShader.tileList = tileListTexture

  C.free(shaderTileLists)

  timers.tileMaskTime:stop()

  -- Compute shader invocation
  var timesToLoop = 1
  if gBenchmarkEnabled and gFrameNumber % CONSTANTS.BENCHMARK_MEASURE_EVERY_X_FRAMES == 0 then
    timesToLoop = CONSTANTS.BENCHMARK_NUM_TIMES_TO_LOOP
  end

  var cache = &gRS.defShaderCache
  timers.computetime:start()
  timers.computetimeCPU:start()
  for i=0, timesToLoop do
    setupCamera(ctxt, numTilesX)
    ctxt:memoryBarrier(Gfx.BarrierFlags.SHADER_IMAGE_ACCESS)
    gRS.deferredShader:bind(ctxt)
    gNumBatches = shadersToRun.count
    for i=0, shadersToRun.count do
      var sdrIndex = shadersToRun:get(i)
      ctxt:setShaderProgram(cache:get(sdrIndex).program)
      ctxt:dispatchCompute(shaderTileCounts[sdrIndex], 1, 1)
    end
    ctxt:memoryBarrier(Gfx.BarrierFlags.TEXTURE_FETCH)
  end
  timers.computetimeCPU:stop()
  timers.computetime:stop()

  C.free(shaderTileCounts)
  gfx:deleteTexture(tileListTexture)


  -- Render to luminance texture
  ctxt:setFramebuffer(gRS.lumFramebuffer)
  ctxt:setViewport(0,0,getLowerPowerOf2(gRS.windowWidth), getLowerPowerOf2(gRS.windowHeight))
  ctxt:clearFramebuffer(gRS.clearColor, 1.0f, 0, Gfx.CLEAR_COLOR)

  ctxt:setDepthStencilState(gRS.depthStencilState)
  ctxt:setRasterizerState(gRS.defaultRasterizerState)
  ctxt:setBlendState(gRS.blendState)

  gRS.lumShader:bind(ctxt)
  ctxt:setVertexLayout(gRS.quadVertexLayout)
  ctxt:setShaderProgram(gRS.lumProgram)

  ctxt:setVertexBuffer(0, gRS.quadVB, terralib.sizeof(QuadVertex), 0)
  ctxt:setIndexBuffer(gRS.quadIB, Gfx.IndexFormat.UInt32)
  ctxt:drawIndexed(Gfx.PrimitiveType.Triangles, 6)

  ctxt:generateMipmap(gRS.lumTexture)


  -- Render image to screen using QuadShader
  ctxt:setFramebuffer(gRS.defaultFramebuffer)
  ctxt:setViewport(0,0,gRS.windowWidth, gRS.windowHeight)
  ctxt:clearFramebuffer(gRS.clearColor, 1.0f, 0, Gfx.CLEAR_COLOR or Gfx.CLEAR_DEPTH)

  ctxt:setDepthStencilState(gRS.depthStencilState)
  ctxt:setRasterizerState(terralib.select(gRS.wireframe,
    gRS.wireframeRasterizerState, gRS.defaultRasterizerState))
  ctxt:setBlendState(gRS.blendState)

  if ImGui.Button("Show Shadow Debug") then
    [&QuadShader.QuadShader.metadata.Type](gRS.quadShader).tex.texture = gGameState.dirLightShadows:get(0)._0
  end
  if ImGui.Button("Show Final Render") then
    [&QuadShader.QuadShader.metadata.Type](gRS.quadShader).tex.texture = gRS.frameTexture
  end

  gRS.quadShader:bind(ctxt)
  ctxt:setVertexLayout(gRS.quadVertexLayout)
  ctxt:setShaderProgram(gRS.quadProgram)

  ctxt:setVertexBuffer(0, gRS.quadVB, terralib.sizeof(QuadVertex), 0)
  ctxt:setIndexBuffer(gRS.quadIB, Gfx.IndexFormat.UInt32)
  ctxt:drawIndexed(Gfx.PrimitiveType.Triangles, 6)


  -- UI stuff
  ImGui.Text("")
  ImGui.Text("Game Runtime")
  if ImGui.Button("Reset Time") then
    timers.time = 0
  end
  reloadSceneFileUI(ctxt)
  if ImGui.Button("Reload Shader Cache") then
    reloadShaderCacheUI()
  end

  ImGui.Text("")
  ImGui.ValueFloat("Time       ", timers.time, "%0.3f")
  ImGui.ValueFloat("FPS        ", 1000.f / timers.frametime.average, "%0.3f")
  ImGui.ValueFloat("frametime  ", timers.frametime.average, "%0.3f")
  ImGui.ValueFloat("shadowtime ", timers.shadowtime.average, "%0.3f")
  ImGui.ValueFloat("tileMasks  ", timers.tileMaskTime.average, "%0.3f")
  ImGui.ValueFloat("tileLightList  ", timers.tileLightListTime.average, "%0.3f")
  ImGui.Text("")
  ImGui.ValueFloat("forwardtime", timers.forwardtime.average, "%0.3f")
  ImGui.ValueFloat("computetime", timers.computetime.average, "%0.3f")
  ImGui.ValueFloat("computetimeCPU", timers.computetimeCPU.average, "%0.3f")
  ImGui.ValueFloat("fwd + def  ", timers.forwardtime.average + timers.computetime.average, "%0.3f")

  ImGui.Text("")
  ImGui.ValueFloat("cameraPos X", gGameState.camera.position.x, "%0.3f")
  ImGui.ValueFloat("cameraPos Y", gGameState.camera.position.y, "%0.3f")
  ImGui.ValueFloat("cameraPos Z", gGameState.camera.position.z, "%0.3f")
  ImGui.Text("")
  ImGui.ValueFloat("cameraQ X", gGameState.camera.rotation.x, "%0.3f")
  ImGui.ValueFloat("cameraQ Y", gGameState.camera.rotation.y, "%0.3f")
  ImGui.ValueFloat("cameraQ Z", gGameState.camera.rotation.z, "%0.3f")
  ImGui.ValueFloat("cameraQ W", gGameState.camera.rotation.w, "%0.3f")

  -- ImGui.Checkbox("using custom light", &usingCustomLight)
  -- ImGui.SliderFloat3("custom light", &customLight[0], -1, 1, "%.3f", 1.0f)
  -- setupLights(ctxt)

  var curCacheConfig : int = gGameState.curShaderCache
  ImGui.Text("")
  ImGui.Text("Shader Specialization Config:")
  ImGui.Combo("", &curCacheConfig, kCacheConfigs, 2, 5)

  if curCacheConfig ~= gGameState.curShaderCache then
    gGameState.curShaderCache = curCacheConfig
    reloadShaderCacheUI()
  end

  ImGui.Text("")
  ImGui.ValueInt("Total Num Shaders  ", cache.count)
  ImGui.ValueInt("Active Num Shaders ", shadersToRun.count)
  ImGui.Text("")

  if ImGui.CollapsingHeader("Active Shaders", nil, true, true) then
    for i=0, shadersToRun.count do
      var sdrIndex = shadersToRun:get(i)
      ImGui.BulletText(cache:get(sdrIndex).filename)
    end
  end
  shadersToRun:delete()

  ImGui.End()
  timers.frametime:stop()
end

terra initialize()
  var window = initGame()
  timers:init(&gfx)

  if window == nil then
    C.printf("[Error] Could not initialize application.\n")
    return nil
  end

  gGameState.models, gGameState.materials, gGameState.dirLights, gGameState.dirLightShadows, gGameState.pointLights, gGameState.shadowedPointLights,
    gGameState.pointVPs, gGameState.pointLightShadows, gGameState.camera = Common.loadScene(gGameState.sceneFilename, &gfx, &gRS)
  if gGameState.models == nil then
    C.printf("[Error] Could not load scene file: %s\n", gGameState.sceneFilename)
    return nil
  end

  setPerspectiveProjection()

  var ctxt = gfx:getImmediateContext()

  var numTilesX = (gRS.windowWidth / TILE_SIZE)  + terralib.select(gRS.windowWidth  % TILE_SIZE == 0, 0, 1)

  setupLights(ctxt)
  setupCamera(ctxt, numTilesX)

  -- TODO: more elaborate construction
  gRS.depthStencilState = gfx:beginBuildDepthStencilState()
    :setDepthFunc(Gfx.DepthFunc.Less)
    :setEnableDepthTest(true)
    :setEnableDepthWrite(true)
    :endBuild();

  gRS.defaultRasterizerState = gfx:beginBuildRasterizerState()
    :endBuild();

  gRS.wireframeRasterizerState = gfx:beginBuildRasterizerState()
    :setFillMode(Gfx.FillMode.WireFrame)
    :endBuild();

  gRS.shadowRasterizerState = gfx:beginBuildRasterizerState()
    :setEnableDepthClip(false)
    :endBuild();

  gRS.blendState = gfx:beginBuildBlendState()
    :endBuild();

  return window
end

local gWindow = global(&SDL.SDL_Window)

terra setupForPerformanceMeasurement()
  var window = initialize()

  if window == nil then
    gWindow = nil
  else
    gWindow = window
  end
end

terra cleanupForPerformanceMeasurement()
  gGameState.models:delete()
  C.free(gGameState.models)
end


terra runFor(numCaches : int, caches : &rawstring)
  var window = gWindow

  if window == nil then
    return nil, nil, nil
  end

  var ctxt = gfx:getImmediateContext()

  var totalNumFrames = CONSTANTS.BENCHMARK_FPS * gGameState.camera.path:get(gGameState.camera.path.count-1).time
  var benchmarkNumFrames = (totalNumFrames / CONSTANTS.BENCHMARK_MEASURE_EVERY_X_FRAMES) +
    terralib.select(totalNumFrames % CONSTANTS.BENCHMARK_MEASURE_EVERY_X_FRAMES == 0, 0, 1)

  var meanVarianceStdev : &float
  var meanVarianceStdevCPU : &float
  meanVarianceStdev = [&float](C.malloc(6 * numCaches * terralib.sizeof(float)))
  meanVarianceStdevCPU = [&float](C.malloc(3 * numCaches * terralib.sizeof(float)))

  var numShaders : &int
  numShaders = [&int](C.malloc(numCaches * terralib.sizeof(int)))

  for i=0, numCaches do
    C.printf("Measuring performance for cache %s\n", caches[i])
    var times : &float
    times = [&float](C.malloc(benchmarkNumFrames * terralib.sizeof(float)))
    var timesCPU : &float
    timesCPU = [&float](C.malloc(benchmarkNumFrames * terralib.sizeof(float)))
    var batches : &int
    batches = [&int](C.malloc(benchmarkNumFrames * terralib.sizeof(int)))

    gRS.fwdShaderCache.count = 0
    gRS.defShaderCache.count = 0
    gRS.materialMaskMap:delete()
    Common.loadShaderCache(caches[i], &gfx, &gRS)

    numShaders[i] = gRS.defShaderCache.count

    timers.time = 0

    -- warm up frames
    gBenchmarkEnabled = false
    for n=0, 60 do
      gFrameNumber = n
      -- The image won't render without this call to SDL_PollEvent()
      -- I'm not sure why...
      var e: SDL.SDL_Event
      while SDL.SDL_PollEvent(&e) ~= 0 do
      end

      ctxt:beginFrame()

      ImGui.ImGui_ImplSdl_NewFrame(window)
      renderFrame(ctxt)

      -- ImGui.Render()

      gfx:swap()
      ctxt:endFrame()
      timers:update()
    end

    gBenchmarkEnabled = true
    var numFramesBenchmarked = 0
    gFrameNumber = 0
    while gFrameNumber < totalNumFrames do
      if CONSTANTS.BENCHMARK_RENDER_ALL_FRAMES
        or gFrameNumber % CONSTANTS.BENCHMARK_MEASURE_EVERY_X_FRAMES == 0
        or gFrameNumber+1 % CONSTANTS.BENCHMARK_MEASURE_EVERY_X_FRAMES == 0
      then
        -- The image won't render without this call to SDL_PollEvent()
        -- I'm not sure why...
        var e: SDL.SDL_Event
        while SDL.SDL_PollEvent(&e) ~= 0 do
        end

        ctxt:beginFrame()

        ImGui.ImGui_ImplSdl_NewFrame(window)
        renderFrame(ctxt)

        -- ImGui.Render()

        gfx:swap()
        ctxt:endFrame()
        timers:update()

        if gFrameNumber % CONSTANTS.BENCHMARK_MEASURE_EVERY_X_FRAMES == 0 then
          times[numFramesBenchmarked] = timers.computetime.elapsedTime / CONSTANTS.BENCHMARK_NUM_TIMES_TO_LOOP
          timesCPU[numFramesBenchmarked] = timers.computetimeCPU.elapsedTime / CONSTANTS.BENCHMARK_NUM_TIMES_TO_LOOP
          batches[numFramesBenchmarked] = gNumBatches
          numFramesBenchmarked = numFramesBenchmarked + 1
        end
      end

      gFrameNumber = gFrameNumber + 1
      timers.time = timers.time + CONSTANTS.BENCHMARK_FRAMETIME
    end

    var mean = 0.f
    var meanCPU = 0.f
    var avgNumBatches = 0.f
    for n=0, numFramesBenchmarked do
      mean = mean + times[n]
      meanCPU = meanCPU + timesCPU[n]
      avgNumBatches = avgNumBatches + batches[n]
    end
    mean = mean / numFramesBenchmarked
    meanCPU = meanCPU / numFramesBenchmarked
    avgNumBatches = avgNumBatches / numFramesBenchmarked

    var variance = 0.f
    var varianceCPU = 0.f
    var varNumBatches = 0.f
    for n=0, numFramesBenchmarked do
      var diff = times[n] - mean
      variance = variance + (diff * diff)

      var diffCPU = timesCPU[n] - meanCPU
      varianceCPU = varianceCPU + (diffCPU * diffCPU)

      var diffBatch = batches[n] - avgNumBatches
      varNumBatches = varNumBatches + (diffBatch * diffBatch)
    end
    variance = variance / numFramesBenchmarked
    varianceCPU = varianceCPU / numFramesBenchmarked
    varNumBatches = varNumBatches / numFramesBenchmarked

    var stdev = C.sqrt(variance)
    var stdevCPU = C.sqrt(varianceCPU)
    var stdevNumBatches = C.sqrt(varNumBatches)

    meanVarianceStdev[i*6+0] = mean
    meanVarianceStdev[i*6+1] = variance
    meanVarianceStdev[i*6+2] = stdev
    meanVarianceStdev[i*6+3] = avgNumBatches
    meanVarianceStdev[i*6+4] = varNumBatches
    meanVarianceStdev[i*6+5] = stdevNumBatches

    meanVarianceStdevCPU[i*3+0] = meanCPU
    meanVarianceStdevCPU[i*3+1] = varianceCPU
    meanVarianceStdevCPU[i*3+2] = stdevCPU
    C.free(batches)
    C.free(timesCPU)
    C.free(times)
  end

  return meanVarianceStdev, numShaders, meanVarianceStdevCPU
end

local SPEC_SETS_LIST = terralib.newlist()
local OUTPUT_FILE_LIST = terralib.newlist()
for i=0, NUM_COMPONENTS do
  SPEC_SETS_LIST:insert(cacheDir .. "specSet-" .. i .. ".dat")
  OUTPUT_FILE_LIST:insert(outputDir .. i .. ".csv")
end
local SPEC_SETS = terralib.constant(`array(SPEC_SETS_LIST))
local OUTPUT_FILES = terralib.constant(`array(OUTPUT_FILE_LIST))

local terra runSpecialization(numToSpecialize : int)
  C.printf("Measuring performance for %d specializations\n", numToSpecialize)

  var p : IO.Parser
  p:init(SPEC_SETS[numToSpecialize])

  var specList : Array(rawstring)
  specList:init()
  var caches : Array(rawstring)
  caches:init()
  while p:peek() ~= 0 do
    caches:add(p:parseLine())
    specList:add(p:parseLine())
  end
  p:delete()

  var meanVarianceStdev, numShaders, meanVarianceStdevCPU = runFor(caches.count, caches:getraw())

  var f = C.fopen(OUTPUT_FILES[numToSpecialize], "w")
  if f == nil then
    C.printf("[ERROR] Unable to open output file '%s'\n", OUTPUT_FILES[numToSpecialize])

    -- Clean up memory
    for i=0, caches.count do
      C.free(caches:get(i))
      C.free(specList:get(i))
    end
    caches:delete()
    specList:delete()
    C.free(meanVarianceStdev)
    C.free(meanVarianceStdevCPU)

    return "INVALID", -1, 100, 100, 100
  end

  C.fprintf(f, "Specialization Set, mean (ms), variance, stdev, numBatches, variance batches, stdev batches, meanCPU (ms), varCPU, stdevCPU\n")

  var curBestTime : float = 10000.0
  var curBestTimeCPU : float = 10000.0
  var curBestSpec = ""
  var curBestNumShaders : int = 0
  var curBestNumBatches : float = 0.0f
  for i=0, caches.count do
    var mean = meanVarianceStdev[i*6+0]
    var meanCPU = meanVarianceStdevCPU[i*3+0]
    var batch = meanVarianceStdev[i*6+3]
    C.fprintf(f, "%s, %f, %f, %f, %f, %f, %f, %f, %f, %f\n", specList:get(i),
      meanVarianceStdev[i*6+0], meanVarianceStdev[i*6+1], meanVarianceStdev[i*6+2],
      meanVarianceStdev[i*6+3], meanVarianceStdev[i*6+4], meanVarianceStdev[i*6+5],
      meanVarianceStdevCPU[i*3+0], meanVarianceStdevCPU[i*3+1], meanVarianceStdevCPU[i*3+2])
    if mean < curBestTime then
      curBestTime = mean
      curBestTimeCPU = meanCPU
      curBestSpec = specList:get(i)
      curBestNumShaders = numShaders[i]
      curBestNumBatches = batch
    end
  end
  C.fprintf(f, "\n")
  C.fprintf(f, "best spec, best mean, num batches, CPU time\n")
  C.fprintf(f, "%s, %f, %f, %f\n", curBestSpec, curBestTime, curBestNumBatches, curBestTimeCPU)

  C.fclose(f)

  -- copy curBestSpec string to bestSpec, because curBestSpec will be implicitly deallocated below
  var bestSpec = [rawstring](C.malloc(terralib.sizeof(int8) * (C.strlen(curBestSpec) + 1)))
  C.strcpy(bestSpec, curBestSpec)

  for i=0, caches.count do
    C.free(caches:get(i))
    C.free(specList:get(i))
  end
  caches:delete()
  specList:delete()
  C.free(meanVarianceStdev)
  C.free(meanVarianceStdevCPU)
  C.free(numShaders)

  C.printf("Finished\n\n")

  return bestSpec, curBestNumShaders, curBestTime, curBestNumBatches, curBestTimeCPU
end

local terra runAllSpecializations()
  var specs : Array(rawstring)
  var numShaders : Array(int)
  var times : Array(float)
  var batches : Array(float)
  var timesCPU : Array(float)
  specs:init()
  numShaders:init()
  times:init()
  batches:init()
  timesCPU:init()

  for i=0, NUM_COMPONENTS+1 do
    var s, n, t, b, cpu = runSpecialization(i)
    specs:add(s)
    numShaders:add(n)
    times:add(t)
    batches:add(b)
    timesCPU:add(cpu)
  end

  var f = C.fopen(tableOutputFilename, "w")
  if f == nil then
    C.printf("[ERROR] Unable to open output file '%s'\n", tableOutputFilename)

    -- Clean up memory
    for i=0, specs.count do
      C.free(specs:get(i))
    end
    specs:delete()
    numShaders:delete()
    times:delete()
    batches:delete()
    timesCPU:delete()
    return
  end

  C.fprintf(f,   "\\begin{center}\n")
  C.fprintf(f,   "\\begin{tabular}{lcccc}\n")
  C.fprintf(f,   "\\toprule\n")
  C.fprintf(f,   "    & Number    & Avg. Num.   & Avg. GPU Time   & Avg. CPU Time   \\\\\n")
  C.fprintf(f,   "    & of        & Dispatches  & ms / frame      & us / frame      \\\\\n")
  C.fprintf(f,   "$k$ & Variants  & Per Frame   & (Relative Perf) & (Relative Perf) \\\\\n")
  C.fprintf(f,   "\\midrule\n")

  var perfFullySpecialized = times:get(NUM_COMPONENTS)
  var perfFullySpecializedCPU = timesCPU:get(NUM_COMPONENTS)
  for i=0, specs.count do
    var relative = (perfFullySpecialized / times:get(i)) * 100
    var relativeCPU = (perfFullySpecializedCPU / timesCPU:get(i)) * 100
    var numVariants = numShaders:get(i)
    C.fprintf(f, "%d  & %d        & %0.1f       & %0.3f (%0.1f\\%%) & %0.3f (%0.1f\\%%)  \\\\\n",
      i, numVariants, batches:get(i), times:get(i), relative, timesCPU:get(i)*1000.f, relativeCPU)
  end

  C.fprintf(f, "\\bottomrule\n")
  C.fprintf(f, "\\end{tabular}\n")
  C.fprintf(f, "\\end{center}\n")

  C.fprintf(f, "\n\n\n\n")

  C.fprintf(f, "Absolute GPU (ms/frame): ")
  for i=0, specs.count do
    C.fprintf(f, "%0.5f ", times:get(i))
  end
  C.fprintf(f, "\n")
  C.fprintf(f, "Relative GPU (vs fully specialized): ")
  for i=0, specs.count do
    var relative = (perfFullySpecialized / times:get(i))
    C.fprintf(f, "%0.5f ", relative)
  end
  C.fprintf(f, "\n")
  C.fprintf(f, "Absolute CPU (us/frame): ")
  for i=0, specs.count do
    C.fprintf(f, "%0.5f ", timesCPU:get(i) * 1000.f)
  end
  C.fprintf(f, "\n")
  C.fprintf(f, "Relative CPU (vs fully specialized): ")
  for i=0, specs.count do
    var relativeCPU = (perfFullySpecializedCPU / timesCPU:get(i))
    C.fprintf(f, "%0.5f ", relativeCPU)
  end
  C.fprintf(f, "\n")

  C.fclose(f)

  -- Clean up memory
  for i=0, specs.count do
    C.free(specs:get(i))
  end
  specs:delete()
  numShaders:delete()
  times:delete()
  batches:delete()
  timesCPU:delete()
end


terra main(argc : int, argv : &rawstring)
  if argc ~= 1 and argc ~= 2 then
    C.printf("%s", USAGE)
    return 1
  end

  var numToSpecialize = -100
  if argc == 2 then
    gGameState.sceneFilename = "../../assets/scenes/SunTemple_v1-path.scn"
    var tmp = argv[1][2]
    argv[1][2] = IO.charLit'\0'
    if C.strcmp(argv[1], "-a") == 0 then
      numToSpecialize = -1
    elseif C.strcmp(argv[1], "-s") == 0 and tmp ~= IO.charLit'\0' then
      argv[1][2] = tmp
      var numString = argv[1]+2
      var c = numString
      while c[0] ~= IO.charLit'\0' do
        if C.isdigit(c[0]) == 0 then
          C.printf("%s", USAGE)
          return 1
        end
        c = c + 1
      end
      numToSpecialize = C.atoi(numString)
    else
      C.printf("%s", USAGE)
      return 1
    end
  end

  if numToSpecialize > NUM_COMPONENTS then
    C.printf("%s", USAGE)
    return 1
  end

  var window = initialize()

  if window == nil then
    return 1
  else
    gWindow = window
  end

  if numToSpecialize == -1 then
    runAllSpecializations()
    C.printf("Finished measuring performance for all possible specializations\n")
    return 0
  elseif numToSpecialize >= 0 then
    runSpecialization(numToSpecialize)
    C.printf("Finished measuring performance when specializing %d features\n", numToSpecialize)
    return 0
  end

  var ctxt = gfx:getImmediateContext()

  var quit = false
  var e: SDL.SDL_Event

  while not quit do
    var startTime = C.clock()
    while SDL.SDL_PollEvent(&e) ~= 0 do
      ImGui.ImGui_ImplSdl_ProcessEvent(&e)
      if e.type == SDL.SDL_QUIT then
        quit = true
      elseif e.type == SDL.SDL_WINDOWEVENT then
        -- if e.window.event == SDL.SDL_WINDOWEVENT_SIZE_CHANGED then
        --   gRenderState.windowWidth = e.window.data1
        --   gRenderState.windowHeight = e.window.data2
        --   setPerspectiveProjection()
        -- end
      elseif e.type == SDL.SDL_MOUSEMOTION and not ImGui.WantCaptureMouse() then
        if SDL.SDL_GetMouseState(nil, nil) == SDL.SDL_BUTTON_LEFT then
          var scale = 0.01f
          var x = e.motion.xrel
          var y = e.motion.yrel
          var rotX = Quat.rotationFromAxisAngle(make_vec3(0,1,0), x*scale)
          var rotY = Quat.rotationFromAxisAngle(make_vec3(1,0,0), y*scale)
          gGameState.camera.rotation = rotX * rotY * gGameState.camera.rotation
        end
      elseif e.type == SDL.SDL_KEYDOWN then
        if e.key.keysym.sym == SDL.SDLK_w then
          gGameState.camera:moveCam(0)
        elseif e.key.keysym.sym == SDL.SDLK_s then
          gGameState.camera:moveCam(1)
        elseif e.key.keysym.sym == SDL.SDLK_a then
          gGameState.camera:moveCam(2)
        elseif e.key.keysym.sym == SDL.SDLK_d then
          gGameState.camera:moveCam(3)
        end
      end
    end

    ctxt:beginFrame()

    ImGui.ImGui_ImplSdl_NewFrame(window)
    renderFrame(ctxt)

    ImGui.Render()

    gfx:swap()
    ctxt:endFrame()
    timers:update()

    var difference = C.clock() - startTime
    var elapsedTime : float = difference * 1000.f / C.CLOCKS_PER_SEC
    timers.time = timers.time + (elapsedTime / 1000.0f)
  end

  gGameState.models:delete()
  C.free(gGameState.models)
end