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
--
-- Copyright (c) 2015, NVIDIA CORPORATION. All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--  * Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--  * Neither the name of NVIDIA CORPORATION nor the names of its
--    contributors may be used to endorse or promote products derived
--    from this software without specific prior written permission.
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

local TerraToShader = require "terraToShader"
local Utils = require "utilities"

-- Remove "saturate" from the list of builtins for GLSL
for _, v in pairs(saturate.definitions) do
  TerraToShader.Builtin.functions[v] = nil
end

local function emitStageShader(builder, stage)

  local ctxt = TerraToShader.EmitContext:new()

  local textureSamplers = ctxt:emitTextureSamplers(builder)
  local textureImages   = ctxt:emitTextureImages(builder)
  local buffers         = ctxt:emitBuffers(builder)
  local uniformBlocks = ctxt:emitUniformBlocks(builder)
  local uniforms      = ctxt:emitUniforms(builder)

  local code          = stage:getCodeList(ctxt, builder)
  local inputs        = stage:getInputsList(ctxt, builder)
  local outputs       = stage:getOutputsList(ctxt, builder)
  local temps         = stage:getTempList(ctxt, builder)

  local inputDecls = ctxt:emitInsOutsUnifs("in", inputs);
  local outputDecls = ctxt:emitInsOutsUnifs("out", outputs);

  local temps = ctxt:emitTemps(temps)

  local terra main()
    [code]
  end
  main:gettype()

  local defn = main.definition

  -- Emit the declaration for the shader entry function
  ctxt:emitFunctionDecl(defn)

  assert(TerraToShader.isTuple(defn.type.returntype) and
    Utils.count(defn.type.returntype.entries) == 0,
    "Error-internal: Shader entry must not return any values.")

  -- Emit all function definitions.
  local funcDefns = ""
  local fn = table.remove(ctxt.gFunctionsToDefine)
  while fn do
    funcDefns = funcDefns .. ctxt:emitFunctionDefn(fn) .. "\n"
    fn = table.remove(ctxt.gFunctionsToDefine)
  end

  local shaderOutputString = "#version 430\n\n"
  -- TODO figure out where "enableBindlessTextures" should live
  if rawget(_G, "enableBindlessTextures") then
    local shaderOutputString = shaderOutputString
      .. "#extension GL_ARB_bindless_texture : require\n"
  end
  local shaderOutputString = shaderOutputString .. "\n"

  shaderOutputString = shaderOutputString .. stage:emitLayoutInfo(ctxt, builder) .. "\n"

  shaderOutputString = shaderOutputString .. "// struct definitions\n"
  for _,v in ipairs(ctxt.gStructDecls) do
    shaderOutputString = shaderOutputString .. v .. "\n"
  end


  shaderOutputString = shaderOutputString .. "\n// textureSamplers\n"
  shaderOutputString = shaderOutputString .. textureSamplers
  shaderOutputString = shaderOutputString .. "\n// textureImages\n"
  shaderOutputString = shaderOutputString .. textureImages
  shaderOutputString = shaderOutputString .. "\n// buffers\n"
  shaderOutputString = shaderOutputString .. buffers
  shaderOutputString = shaderOutputString .. "\n// uniform blocks\n"
  shaderOutputString = shaderOutputString .. uniformBlocks
  shaderOutputString = shaderOutputString .. "\n// uniforms\n"
  shaderOutputString = shaderOutputString .. uniforms
  shaderOutputString = shaderOutputString .. "\n// in variables\n"
  shaderOutputString = shaderOutputString .. inputDecls
  shaderOutputString = shaderOutputString .. "\n// out variables\n"
  shaderOutputString = shaderOutputString .. outputDecls
  shaderOutputString = shaderOutputString .. "\n// temp variables\n"
  shaderOutputString = shaderOutputString .. temps

  shaderOutputString = shaderOutputString .. "\n// function forward declarations\n"
  shaderOutputString = shaderOutputString .. ctxt.gShaderFunctionDecls .. "\n"
  shaderOutputString = shaderOutputString .. "\n// function definitions\n\n"
  shaderOutputString = shaderOutputString .. funcDefns .. "\n"

  return shaderOutputString
end

local function terraToGLSL(builder)
  local vertexCode = emitStageShader(builder, TerraToShader.VertexStage)
  local fragmentCode = emitStageShader(builder, TerraToShader.FragmentStage)
  local computeCode = emitStageShader(builder, TerraToShader.ComputeStage)
  return { vertexSourceCode = vertexCode, fragmentSourceCode = fragmentCode,
           computeSourceCode = computeCode,
         }
end

return terraToGLSL
