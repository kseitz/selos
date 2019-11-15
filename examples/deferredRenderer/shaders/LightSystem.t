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

local CONSTANTS = require("constants")
local NUM_CASCADES              = CONSTANTS.NUM_CASCADES
local MAX_DIR_LIGHTS            = CONSTANTS.MAX_DIR_LIGHTS

local Utils = require("utilities")

-- Light Sample
local struct LightSample {
  intensity : vec3
  direction : vec3
  NdotH : float
  NdotL : float
  LdotH : float
}

local terra make_LightSample(intensity : vec3, direction : vec3, eyeDir : vec3, normal : vec3)
  var ls : LightSample
  ls.intensity = intensity
  ls.direction = direction

  var H = normalize(eyeDir + direction);
  ls.NdotH = dot(normal, H);
  ls.NdotL = dot(normal, direction);
  ls.LdotH = dot(direction, H);

  return ls
end


local terra getDistanceFalloff(distSquared : float)
  -- The 0.01 is to avoid infs when the light source is close to the shading point
  return 1f / ((0.01f * 0.01f) + distSquared);
end


-- Lights
local LightTypes = {
  asList = terralib.newlist(),
  byName = {},
}

local function registerLight(light, maxAllowed, isCullable)
  local staticFunctions = {}

  for k, v in pairs(light) do
    if terralib.isfunction(v) then
      staticFunctions[k] = v
    end
  end

  function light.metamethods.__getmethod(self, methodname)
    local func = staticFunctions[methodname]

    if func ~= nil then
      return func
    end

    return self.methods[methodname]
  end

  light.__isCullable = isCullable
  light.__maxAllowed = maxAllowed

  LightTypes.asList:insert(light)
  LightTypes.byName[light.name] = light
end

-- Directional Light
local struct DirLight {
  intensity  : vec3
  direction  : vec3
  cascadeEnds : vec4
  vp         : mat4[NUM_CASCADES]
}

terra DirLight.isInShadow(light : DirLight, shadowMap : sampler2D, wPos : vec3, normal : vec3, farPlane : float, cascadeIdx : int)
  var lightSpacePos = light.vp[cascadeIdx] * make_vec4(wPos, 1)
  var projCoords = lightSpacePos.xyz / lightSpacePos.w
  projCoords.x = 0.5f * projCoords.x + 0.5f
  projCoords.y = 0.5f * -projCoords.y + 0.5f
  var tex = textureLod(shadowMap, projCoords.xy, 0).r
  var shadowBias = max(0.05f * (1.0f - dot(normal, light.direction)), 0.005f);
  shadowBias = 0.00001f

  return tex < (projCoords.z - shadowBias)
end

function DirLight:illuminate(MaterialType)
  local terra illuminate(l : DirLight, sd : MaterialType:ShadingDataType())
    var ls = make_LightSample(l.intensity, l.direction, sd.eyeDir, sd.normal)
    return [MaterialType:eval(LightSample)](sd, ls)
  end

  return illuminate
end

registerLight(DirLight, NUM_CASCADES, false)


-- Point Light
local struct PointLight {
  intensity  : vec3
  position   : vec3
  -- TODO remove manual padding
  -- The padding is needed for now because of how D3D11 handles arrays.
  -- Array elements are padded to 16-byte strides, except for the last element
  -- in the array which is not (necessarily) padded.
  pad        : float
}

function PointLight:illuminate(MaterialType)
  local terra illuminate(l : PointLight, sd : MaterialType:ShadingDataType())
    var dir = l.position - sd.wPos
    var dist = length(dir)

    if dist >= CONSTANTS.POINT_LIGHT_RADIUS then
      return make_vec3(0,0,0)
    end

    var distSquared = dot(dir, dir)

    if distSquared > 0.00001f then
      dir = normalize(dir)
    else
      dir = make_vec3(0,0,0)
    end

    var falloff = getDistanceFalloff(distSquared)
    var intensity = l.intensity * falloff
    var ls = make_LightSample(intensity, dir, sd.eyeDir, sd.normal)

    return [MaterialType:eval(LightSample)](sd, ls)
  end

  return illuminate
end

registerLight(PointLight, CONSTANTS.MAX_POINT_LIGHTS, true)


-- Shadowed Point Light
local struct ShadowedPointLight {
  intensity  : vec3
  position   : vec3
  -- TODO remove manual padding
  -- The padding is needed for now because of how D3D11 handles arrays.
  -- Array elements are padded to 16-byte strides, except for the last element
  -- in the array which is not (necessarily) padded.
  pad        : float
}

terra ShadowedPointLight.isInShadow(light : ShadowedPointLight, shadowMap : samplerCube, wPos : vec3, normal : vec3, farPlane : float, idx : int)
  var fragToLight = wPos - light.position
  var closestDepth = textureLod(shadowMap, fragToLight, 0).r
  closestDepth = closestDepth * farPlane
  var currentDepth = length(fragToLight)
  var bias = 0.05f
  return currentDepth - bias > closestDepth
end

function ShadowedPointLight:illuminate(MaterialType)
  local terra illuminate(l : ShadowedPointLight, sd : MaterialType:ShadingDataType())
    var dir = l.position - sd.wPos
    var dist = length(dir)

    if dist >= CONSTANTS.POINT_LIGHT_RADIUS then
      return make_vec3(0,0,0)
    end

    var distSquared = dot(dir, dir)

    if distSquared > 0.00001f then
      dir = normalize(dir)
    else
      dir = make_vec3(0,0,0)
    end

    var falloff = getDistanceFalloff(distSquared)
    var intensity = l.intensity * falloff
    var ls = make_LightSample(intensity, dir, sd.eyeDir, sd.normal)

    return [MaterialType:eval(LightSample)](sd, ls)
  end

  return illuminate
end

registerLight(ShadowedPointLight, CONSTANTS.MAX_SHADOWED_POINT_LIGHTS, true)


local function getCullableLights()
  local ret = terralib.newlist()

  for _, v in ipairs(LightTypes.asList) do
    if v.__isCullable then
      ret:insert(v)
    end
  end

  return ret
end

local function buildTileLightListStruct()
  local counts  = terralib.newlist()
  local indexes = terralib.newlist()

  local cullableLights = getCullableLights()
  for _, v in ipairs(cullableLights) do
    counts:insert(  {field = "num" .. v.name, type = int} )
    indexes:insert( {field = "idx" .. v.name, type = int[v.__maxAllowed]} )
  end

  -- For packing reasons, insert all counts before the arrays of indexes
  local TileLightList = terralib.types.newstruct("TileLightList")
  TileLightList.entries:insertall(counts)
  TileLightList.entries:insertall(indexes)

  return TileLightList
end

local TileLightList = buildTileLightListStruct()


local TiledLightListEnv = {
  environmentFunctions = {},
  configurationFunctions = {},
}


function TiledLightListEnv.new()
  local lightEnv = {
    members = {},
  }

  setmetatable(lightEnv, {
    __index = function(table, key)
      if TiledLightListEnv.environmentFunctions[key] ~= nil then
        return TiledLightListEnv.environmentFunctions[key]
      else
        return rawget(table, key)
      end
    end
  })

  return lightEnv
end

function TiledLightListEnv.environmentFunctions:addMembers(pb)
  local lightLists = pb:createBuffer(TileLightList[CONSTANTS.MAX_NUM_TILES], "lightLists", nil)
  pb:addMember(lightLists, lightLists.name)
  self.members.lightLists = lightLists.decl

  -- Create shadow maps
  for _, v in ipairs(LightTypes.asList) do
    if v.isInShadow ~= nil then
      local name = "shadowMaps" .. v.name
      local mapType = v.isInShadow.definition.parameters[2].type

      local shadowMaps = pb:createTextureSampler(mapType[v.__maxAllowed], name, nil)
      pb:addMember(shadowMaps, shadowMaps.name)
      self.members[name] = shadowMaps.decl
    end
  end

  -- Separate UBO for DirLights because we update it every frame
  local dirLightUB = pb:createUniformBlock("DirLights", nil)
  dirLightUB:declareUniform(int, "dirLightCount", {}, nil)
  dirLightUB:declareUniform(DirLight[MAX_DIR_LIGHTS], "dirLights", {}, nil)
  pb:addMember(dirLightUB, dirLightUB.name)
  self.members.dirLightUB = dirLightUB:createInstanceRepresentation({})

  -- UBO for the rest of the lights
  local uniformBlock = pb:createUniformBlock("Lights", nil)
  uniformBlock:declareUniform(float, "farPlane", {}, nil)

  local cullableLights = getCullableLights()
  for _, v in ipairs(cullableLights) do
    uniformBlock:declareUniform(v[v.__maxAllowed], "all" .. v.name, {}, nil)
  end

  pb:addMember(uniformBlock, uniformBlock.name)
  self.members.uniformBlock = uniformBlock:createInstanceRepresentation({})
end

function TiledLightListEnv.environmentFunctions:getDefaultConfiguration()
  local config = {}
  config.statusList = terralib.newlist()

  local cullableLights = getCullableLights()
  for _, v in ipairs(cullableLights) do
    config.statusList:insert(true)
  end

  setmetatable(config, {
    __index = function(table, key)
      if TiledLightListEnv.configurationFunctions[key] ~= nil then
        return TiledLightListEnv.configurationFunctions[key]
      elseif self.members[key] ~= nil then
        return self.members[key]
      else
        return rawget(table, key)
      end
    end
  })

  return config
end

function TiledLightListEnv.configurationFunctions:isValid()
  -- Since we are not specializing on Dir Lights, every configuration is valid
  -- (including the configuration where all light types are disabled).
  return true
end

function TiledLightListEnv.configurationFunctions:getConfigurationID()
  local id = 0
  local count = #self.statusList
  for i, v in ipairs(self.statusList) do
    id = id + (math.pow(2, count-i) * Utils.boolToInt(self.statusList[i]))
  end

  return id, count
end

function TiledLightListEnv.configurationFunctions:illuminate(MaterialType)
  local sd            = symbol(MaterialType:ShadingDataType(), "sd")
  local clipZ         = symbol(float, "clipZ")
  local tileID        = symbol(int, "tileID")
  local colorAcc      = symbol(vec3, "colorAcc")

  local function generateShadowCode(lightType, light, idx)
    if lightType.isInShadow == nil then
      return `false
    end

    local isInShadow = symbol(bool)
    local ifStatements = quote end
    for c=0, lightType.__maxAllowed - 1 do
      ifStatements = quote
        if idx == c then
          [isInShadow] = lightType.isInShadow(light, self.["shadowMaps" .. lightType.name][c], sd.wPos, sd.normal, self.uniformBlock.farPlane, idx)
        else
          [ifStatements]
        end
      end
    end

    local ret = quote
      var [isInShadow] = false
      [ifStatements]
    in
      [isInShadow]
    end

    return ret
  end

  local quoteList = terralib.newlist()

  -- Dir Lights
  -- Special-cased because we are not specializing on it and because it has a separate UBO
  quoteList:insert(quote
    for i=0, self.dirLightUB.dirLightCount do
      var l = self.dirLightUB.dirLights[i]

      var cascadeIdx : int = NUM_CASCADES - 1
      for i = NUM_CASCADES-1, -1, -1 do
        if clipZ <= l.cascadeEnds(i) then
          cascadeIdx = i
        end
      end

      var isInShadow = [generateShadowCode(DirLight, l, cascadeIdx)]

      if not isInShadow then
        colorAcc = colorAcc + [DirLight:illuminate(MaterialType)](l, sd)
      end
    end
  end)

  -- The rest of the lights
  local cullableLights = getCullableLights()
  for k, v in ipairs(cullableLights) do
    if self.statusList[k] then
      quoteList:insert(quote
        for idx=0, self.lightLists[tileID].["num" .. v.name] do
          var i = self.lightLists[tileID].["idx" .. v.name][idx]
          var l = self.uniformBlock.["all" .. v.name][i]

          var isInShadow = [generateShadowCode(v, l, i)]

          if not isInShadow then
            colorAcc = colorAcc + [v:illuminate(MaterialType)](l, sd)
          end
        end
      end)
    end
  end

  local terra illuminate([sd], [clipZ], [tileID])
    var [colorAcc] = make_vec3(0,0,0)
    for lightIters = 0, CONSTANTS.BENCHMARK_NUM_LIGHT_ITERS do
      [quoteList]
    end
    return colorAcc
  end

  return illuminate
end

return {
  LightSample         = LightSample,
  make_LightSample    = make_LightSample,
  TileLightList       = TileLightList,
  LightTypes          = LightTypes,
  TiledLightListEnv = TiledLightListEnv,
  getCullableLights = getCullableLights,
}
