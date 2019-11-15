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

local Utils = require("utilities")


local CommonShadingDataElements = {
  eyeDir          = {field = "eyeDir",          type = vec3   },
  tangent         = {field = "tangent",         type = vec3   },
  bitangent       = {field = "bitangent",       type = vec3   },
}

local RequiredShadingDataElements = {
  diffuse         = {field = "diffuse",         type = vec3   },
  diffuseWeight   = {field = "diffuseWeight",   type = float  },
  specular        = {field = "specular",        type = vec3   },
  linearRoughness = {field = "linearRoughness", type = float  },
  emissive        = {field = "emissive",        type = vec3   },
  wPos            = {field = "wPos",            type = vec3   },
  normal          = {field = "normal",          type = vec3   },
  IoR             = {field = "IoR",             type = float  },
  materialMask    = {field = "materialMask",    type = int    },
}

local ShadingDataElements = Utils.combineTables(CommonShadingDataElements, RequiredShadingDataElements)

setmetatable(ShadingDataElements, {
    __index = function(t, k)
      error("No member '" .. k .. "' in ShadingDataElements", 2)
    end,
})

local function prepareCommonShadingData(cameraPos, sd)
  local quoteList = terralib.newlist()

  local function calcBitangent()
    return quote
      var bt : vec3
      if abs(sd.normal.x) > abs(sd.normal.y) then
        bt = make_vec3(sd.normal.z, 0.f, -sd.normal.x) / length(make_vec2(sd.normal.x, sd.normal.z))
      else
        bt = make_vec3(0.f, sd.normal.z, -sd.normal.y) / length(make_vec2(sd.normal.y, sd.normal.z))
      end
      bt = normalize(bt)
    in
      bt
    end
  end

  local sdMembers = sd.type.entries:map("field")

  local hasEyeDir     = Utils.find(sdMembers, "eyeDir")
  local hasBitangent  = Utils.find(sdMembers, "bitangent")
  local hasTangent    = Utils.find(sdMembers, "tangent")

  if hasEyeDir then
    quoteList:insert(quote sd.eyeDir = normalize(cameraPos - sd.wPos) end)
  end

  if hasBitangent then
    quoteList:insert(quote sd.bitangent = [calcBitangent()] end)
  end

  if hasTangent then
    if hasBitangent then
      quoteList:insert(quote sd.tangent = normalize(cross(sd.bitangent, sd.normal)) end)
    else
      quoteList:insert(quote sd.tangent = normalize(cross([calcBitangent()], sd.normal)) end)
    end
  end

  return quoteList
end

local function registerShadingDataProvider(provider)
  local errMsgs = terralib.newlist()

  for k, _ in pairs(RequiredShadingDataElements) do
    if provider[k] == nil then
      errMsgs:insert(k)
    end
  end

  if #errMsgs ~= 0 then
    error("ShadingDataProvider does not implement the following required elements:\n  " .. errMsgs:concat("\n  "), 2)
  end
end


local GBufferShadingData = {}
local GBSD = GBufferShadingData

GBufferShadingData.gBuffers = {}
setmetatable(GBufferShadingData.gBuffers, {
  __index = function(t, k)
    assert(0 <= k and k <= 4, "Invalid GBuffer index")

    local sym = symbol(vec4, "gBuffer" .. k)
    GBufferShadingData.gBuffers[k] = sym

    return sym
  end,
})

-- Gbuffer 0: wPos x y z + materialMask
-- Gbuffer 1: normal x y z + IoR
-- GBuffer 2: diffuse r g b + lightmap
-- GBuffer 3: specular r g b + roughness
-- Gbuffer 4: emissive r g b

local GBUFFER = {}
GBUFFER.W_POS         = { idx = 0, value = "xyz"  }
GBUFFER.MATERIAL_MASK = { idx = 0, value = "w"    }
GBUFFER.NORMAL        = { idx = 1, value = "xyz"  }
GBUFFER.IOR           = { idx = 1, value = "w"    }
GBUFFER.DIFFUSE       = { idx = 2, value = "rgb"  }
GBUFFER.DIFFUSE_W     = { idx = 2, value = "a"    }
GBUFFER.SPECULAR      = { idx = 3, value = "rgb"  }
GBUFFER.ROUGHNESS     = { idx = 3, value = "w"    }
GBUFFER.EMISSIVE      = { idx = 4, value = "rgb"  }

function GBufferShadingData.diffuse(sd, cameraPos, gbufs)         return `[GBSD.gBuffers[GBUFFER.DIFFUSE.idx]].[GBUFFER.DIFFUSE.value] end
function GBufferShadingData.diffuseWeight(sd, cameraPos, gbufs)   return `[GBSD.gBuffers[GBUFFER.DIFFUSE_W.idx]].[GBUFFER.DIFFUSE_W.value] end
function GBufferShadingData.specular(sd, cameraPos, gbufs)        return `[GBSD.gBuffers[GBUFFER.SPECULAR.idx]].[GBUFFER.SPECULAR.value] end
function GBufferShadingData.linearRoughness(sd, cameraPos, gbufs) return `(1 - [GBSD.gBuffers[GBUFFER.ROUGHNESS.idx]].[GBUFFER.ROUGHNESS.value]) end
function GBufferShadingData.emissive(sd, cameraPos, gbufs)        return `[GBSD.gBuffers[GBUFFER.EMISSIVE.idx]].[GBUFFER.EMISSIVE.value] end
function GBufferShadingData.wPos(sd, cameraPos, gbufs)            return `[GBSD.gBuffers[GBUFFER.W_POS.idx]].[GBUFFER.W_POS.value] end
function GBufferShadingData.normal(sd, cameraPos, gbufs)          return `normalize([GBSD.gBuffers[GBUFFER.NORMAL.idx]].[GBUFFER.NORMAL.value]) end
function GBufferShadingData.IoR(sd, cameraPos, gbufs)             return `[GBSD.gBuffers[GBUFFER.IOR.idx]].[GBUFFER.IOR.value] end
function GBufferShadingData.materialMask(sd, cameraPos, gbufs)    return `[GBSD.gBuffers[GBUFFER.MATERIAL_MASK.idx]].[GBUFFER.MATERIAL_MASK.value] end

function GBufferShadingData.prepare(gbuffers, cameraPos, pixelCoords, sd)
  local quoteList = terralib.newlist()

  for k, _ in pairs(GBSD.gBuffers) do
    GBSD.gBuffers[k] = nil
  end

  for _, v in pairs(sd.type.entries) do
    local fieldName = v.field
    if GBufferShadingData[fieldName] ~= nil then
      quoteList:insert(quote sd.[fieldName] = [GBufferShadingData[fieldName](sd, cameraPos)] end)
    end
  end

  quoteList:insertall(prepareCommonShadingData(cameraPos, sd))

  local matMaskGBuffer = GBSD.gBuffers[GBUFFER.MATERIAL_MASK.idx] -- reference this here to ensure it gets loaded below

  local gBufferLoads = terralib.newlist()
  for k, v in pairs(GBSD.gBuffers) do
    gBufferLoads:insert(quote var [v] = imageLoad(gbuffers[k], pixelCoords) end)
  end

  return quote
      [gBufferLoads]
      [quoteList]
    end
end

registerShadingDataProvider(GBufferShadingData)


return {
  Elements = ShadingDataElements,
  GBufferShadingData = GBufferShadingData,
  GBUFFER_CONSTANTS = GBUFFER,
}
