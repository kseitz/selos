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

print("Compiling Shaders...")

local Components     = require "MaterialSystem"
local LightCullingShader = require "lightCulling"
local MaterialShader = require "materialShader"
local ShadowShader   = require "shadows"
local QuadShader     = require "quadShader"
local Utils          = require "utilities"

local bit = require("bit")

local shadersToOutput = {}

local outputDir = "build/compiledShaders/"
if gCurrentGraphicsAPIName == "gfx-d3d11" then
  outputDir = outputDir .. "D3D11/"
else
  outputDir = outputDir .. "OpenGL/"
end

-- TODO We are currently not specializing the forward shader.
--      When that is implemented, materialID should be fetched
--      from the shader, rather than be hardcoded.
local materialID = 0 -- TODO get from shader
local vertFilename = outputDir .. materialID .. ".vert"
local fragFilename = outputDir .. materialID .. ".frag"
shadersToOutput[vertFilename] = MaterialShader.forward.vertexSourceCode
shadersToOutput[fragFilename] = MaterialShader.forward.fragmentSourceCode

shadersToOutput[outputDir .. "dirShadow.vert"] = ShadowShader.dirLightShadow.vertexSourceCode
shadersToOutput[outputDir .. "dirShadow.frag"] = ShadowShader.dirLightShadow.fragmentSourceCode
shadersToOutput[outputDir .. "pointShadow.vert"] = ShadowShader.pointLightShadow.vertexSourceCode
shadersToOutput[outputDir .. "pointShadow.frag"] = ShadowShader.pointLightShadow.fragmentSourceCode

shadersToOutput[outputDir .. "lightCulling.comp"] = LightCullingShader.computeSourceCode

shadersToOutput[outputDir .. "lum.vert"] = QuadShader.LuminanceShader.vertexSourceCode
shadersToOutput[outputDir .. "lum.frag"] = QuadShader.LuminanceShader.fragmentSourceCode

shadersToOutput[outputDir .. "quad.vert"] = QuadShader.QuadShader.vertexSourceCode
shadersToOutput[outputDir .. "quad.frag"] = QuadShader.QuadShader.fragmentSourceCode


local function addShaderToOutput(config)
  MaterialShader.deferred:setConfiguration(config)

  local maskString = Utils.toBitString(config:getConfigurationID())

  local computeFilename = outputDir .. maskString .. ".comp"
  local code = MaterialShader.deferred:generateShaderSourceCode().computeSourceCode
  shadersToOutput[computeFilename] = code
end

local function recur(config, idx)
  if idx > (#config.MaterialType.statusList + #config.LightEnv.statusList) then
    -- Finished recursing - generate compiled shader

    -- Only generate shaders for valid configurations
    if config:isValid() then
      addShaderToOutput(config)
    end

    return
  elseif idx > #config.MaterialType.statusList then
    local newIdx = idx - #config.MaterialType.statusList
    config.LightEnv.statusList[newIdx] = false
    recur(config, idx + 1)
    config.LightEnv.statusList[newIdx] = true
    recur(config, idx + 1)
  else
    for _, v in pairs(Components.FEATURE_STATUS) do
      config.MaterialType.statusList[idx] = v
      recur(config, idx + 1)
    end
  end
end

local defaultConfig = MaterialShader.deferred:getDefaultConfiguration()
recur(defaultConfig, 1)

for k,v in pairs(shadersToOutput) do
  local file = io.open(k, "w")
  io.output(file)
  io.write(v)
  io.close(file)
end


print("Finished\n")
