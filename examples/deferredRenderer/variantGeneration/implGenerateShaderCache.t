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

local MaterialShader = require "materialShader"
local MaterialSystem = require "MaterialSystem"
local Utils          = require "utilities"

local bit = require("bit")

local compiledShadersDir = "../build/compiledShaders/"
if gCurrentGraphicsAPIName == "gfx-d3d11" then
  compiledShadersDir = compiledShadersDir .. "D3D11/"
else
  compiledShadersDir = compiledShadersDir .. "OpenGL/"
end

local function w(ln)
  io.write(ln)
  io.write("\n")
end

local FEATURE_STATUS = MaterialSystem.FEATURE_STATUS

local function generateVariants(config, idx, allowSpecList, shaderList)
  if idx > (#config.MaterialType.statusList + #config.LightEnv.statusList) then
    if not config:isValid() then
      return
    end

    local numEnabled = 0
    local featureStatusList = terralib.newlist()

    for _,v in ipairs(config.MaterialType.statusList) do
      featureStatusList:insert(v)
      if v ~= FEATURE_STATUS.DISABLED then
        numEnabled = numEnabled + 1
      end
    end

    for _,v in ipairs(config.LightEnv.statusList) do
      if v then
        numEnabled = numEnabled + 1
        featureStatusList:insert(FEATURE_STATUS.ENABLED)
      else
        featureStatusList:insert(FEATURE_STATUS.DISABLED)
      end
    end

    local maskString = Utils.toBitString(config:getConfigurationID())
    local computeFilename = compiledShadersDir .. maskString .. ".comp"
    local shaderID = config:getConfigurationID()
    w("defshader {")
    w("  shaderIndex = " .. #shaderList)
    w("  shaderID = " .. shaderID)
    w("  computeSourceCodeFile = " .. computeFilename)
    w("}\n")
    shaderList:insert({
      featureStatusList = featureStatusList,
      numEnabled = numEnabled,
    })   
  elseif idx > #config.MaterialType.statusList then
    local newIdx = idx - #config.MaterialType.statusList
    if allowSpecList[idx] then
      config.LightEnv.statusList[newIdx] = false
      generateVariants(config, idx+1, allowSpecList, shaderList)
    end
    config.LightEnv.statusList[newIdx] = true
    generateVariants(config, idx+1, allowSpecList, shaderList)
  else
    if allowSpecList[idx] then
      config.MaterialType.statusList[idx] = FEATURE_STATUS.DISABLED
      generateVariants(config, idx+1, allowSpecList, shaderList)
    end
    config.MaterialType.statusList[idx] = FEATURE_STATUS.ENABLED
    generateVariants(config, idx+1, allowSpecList, shaderList)
  end
end

local function outputCache(allowSpecList)
  -- Forward shaders
  w("fwdshader {")
  -- TODO output for each material combination
  local materialID = 0 -- TODO get from shader
  local vertFilename = compiledShadersDir .. materialID .. ".vert"
  local fragFilename = compiledShadersDir .. materialID .. ".frag"
  w("  materialID = " .. materialID)
  w("  vertexSourceCodeFile = " .. vertFilename)
  w("  fragmentSourceCodeFile = " .. fragFilename)
  w("}\n")

  local shaderList = terralib.newlist()
  local config = MaterialShader.deferred:getDefaultConfiguration()
  generateVariants(config, 1, allowSpecList, shaderList)

  local numComponents = (#config.MaterialType.statusList + #config.LightEnv.statusList)
  local allFlags = math.pow(2, numComponents)-1

  w("materialMap {")
  for orMask=0, allFlags do
    local mustBeEnabled = terralib.newlist()

    for i=0, (numComponents - 1) do
      local b = bit.lshift(1, i)
      if bit.band(orMask, b) > 0 then
        mustBeEnabled:insert(i)
      end
    end

    local candidate = nil
    for k,s in ipairs(shaderList) do
      local canUse = true

      for _,v in ipairs(mustBeEnabled) do
        if s.featureStatusList[v+1] == FEATURE_STATUS.DISABLED then canUse = false end
      end

      if canUse then
        if candidate == nil then
          candidate = {index = k-1, numEnabled = s.numEnabled}
        else
          if s.numEnabled < candidate.numEnabled then
            candidate = {index = k-1, numEnabled = s.numEnabled}
          end
        end
      end
    end

    local mask = orMask
    w("  " .. mask .. " = " .. candidate.index)
  end
  w("}\n")
end

return outputCache
