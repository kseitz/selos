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

local EmitContext = TerraToShader.EmitContext;
local EmitContextHLSL = TerraToShader.newClass(EmitContext)

-- TODO: refactor to try to share more code

function EmitContextHLSL:init(...)
	EmitContext.init(self, ...);
	self.builtinVariablesUsed = {}
end

function EmitContextHLSL:noteBuiltinVariableUsed(name)
	self.builtinVariablesUsed[name] = true;
end

function EmitContextHLSL:emitInnerProduct(left, right)
  return "mul(" .. left .. ", " .. right .. ")"
end

function EmitContextHLSL:emitDeclList(list, fn, sep)
  local ret = ""
  for i,v in ipairs(list) do

  	if sep and i ~= 0 then
  		ret = ret .. sep
  	end

    local origName = v.name
    local decl = v.decl

    local loc = v.location

    local newName = self:getName(decl, origName)

    ret = ret .. fn(decl, loc, v);
  end

  return ret
end

function EmitContextHLSL:emitAttrDecls(builderData, semanticPrefix)
  if not semanticPrefix then
    semanticPrefix = "A";
  end
  return self:emitDeclList(builderData, function(decl, loc)
    local ret = self:emitDecl(decl)

    if loc then
      ret = ret .. " : " .. semanticPrefix .. loc
    end
    ret = ret .. ";\n"
    return ret;
  end);
end

function EmitContextHLSL:emitTemps(list)
  return self:emitDeclList(list, function(decl, loc, v)
    local ret = ""
    if v.isShared then
      ret = ret .. "groupshared "
    else
      ret = ret .. "static "
    end
    return ret .. self:emitDecl(decl) .. ";\n"
  end);
end

function EmitContextHLSL:emitAssign(list, dstPrefix, srcPrefix)
  return self:emitDeclList(list, function(decl, loc)
  	return dstPrefix .. self:getName(decl) .. " = " .. srcPrefix .. self:getName(decl) .. ";\n"
  end);
end

function EmitContextHLSL:emitUniforms(builder)
  return self:emitInsOutsUnifs("uniform", builder.uniformsList)
end

function EmitContextHLSL:emitTextureSamplers(builder)
  local ret = ""
  for _,v in ipairs(builder.textureSamplersList) do
    local origName = v.name
    local decl = v.decl

    local binding = v.binding

    local newName = self:getName(decl, origName)

    -- texture
    ret = ret .. "uniform " .. self:emitDecl(decl);
    if binding then
      ret = ret .. " : register(t" .. binding .. ")"
    end
    ret = ret .. ";\n"

    -- and sampler
    ret = ret .. "uniform SamplerState " .. self:getName(decl) .. "_SAMPLER";
    if decl.type:isarray() then
      ret = ret .. "[" .. decl.type.N .. "]"
    end
    if binding then
      ret = ret .. " : register(s" .. binding .. ")"
    end
    ret = ret .. ";\n"
  end

  return ret
end

function EmitContextHLSL:emitTextureImages(builder)
  local ret = ""
  for _,v in ipairs(builder.textureImagesList) do
    local origName = v.name
    local decl = v.decl

    local binding = v.binding

    local newName = self:getName(decl, origName)

    EmitContextHLSL.builtinTypeMap.image2D = "RWTexture2D<" .. v.shaderTextureFormat .. ">"
    EmitContextHLSL.builtinTypeMap.uimage2D = "RWTexture2D<" .. v.shaderTextureFormat .. ">"

    ret = ret .. self:emitDecl(decl)
    if binding then
      ret = ret .. " : register(u" .. binding .. ")"
    end
    ret = ret .. ";\n"

    EmitContextHLSL.builtinTypeMap.image2D  = nil
    EmitContextHLSL.builtinTypeMap.uimage2D = nil
  end

  return ret
end

function EmitContextHLSL:emitBuffers(builder)
  local ret = ""
  for _, v in ipairs(builder.buffersList) do
    local origName = v.name
    local decl = v.decl

    local binding = v.binding

    local newName = self:getName(decl, origName)

    ret = ret .. "RWStructuredBuffer<" .. self:getTypeName(v.type.type) .. "> "
    ret = ret .. self:getName(decl)
    if binding then
      ret = ret .. " : register(u" .. binding .. ")"
    end
    ret = ret .. ";\n"
  end

  return ret
end

function EmitContextHLSL:emitUniformBlocks(builder)
  local ret = ""
  for _, block in ipairs(builder.uniformBlocksList) do
    local origBlockName = block.name
    local newBlockName = self:createUniqueName(origBlockName)

    local binding = block.binding

    ret = ret .. "cbuffer " .. newBlockName

    if binding then
      ret = ret .. " : register(b" .. binding .. ")"
    end


    ret = ret .. "\n{\n"

    for _, uniform in ipairs(block.uniformsList) do
      local newName = self:getName(uniform.decl, uniform.name)
      ret = ret .. "  " .. self:emitDecl(uniform.decl) .. ";\n"
    end

    ret = ret .. "};\n\n"
  end

  return ret
end

local builtinVarDir_input = { key = "input", copyKey = "inputCopy", dstPrefix = "", srcPrefix = "MS_input." }
local builtinVarDir_output = { key = "output", copyKey = "outputCopy", dstPrefix = "MS_output.", srcPrefix = "" }

local builtinVarInfos = {
  gl_Position = { name = "MS_RS_Position", dir = builtinVarDir_output, type = "float4", semantic = "SV_Position" },
  gl_GlobalInvocationID = { name = "MS_SV_DispatchThreadID", dir = builtinVarDir_input, type = "uint3", semantic = "SV_DispatchThreadID"},
  gl_LocalInvocationID  = { name = "MS_SV_GroupThreadID", dir = builtinVarDir_input, type = "uint3", semantic = "SV_GroupThreadID"},
  gl_LocalInvocationIndex  = { name = "MS_SV_GroupIndex", dir = builtinVarDir_input, type = "uint", semantic = "SV_GroupIndex"},
  gl_WorkGroupID        = { name = "MS_SV_GroupID", dir = builtinVarDir_input, type = "uint3", semantic = "SV_GroupID"},
  gl_FragDepth        = { name = "MS_SV_Depth", dir = builtinVarDir_output, type = "float", semantic = "SV_Depth"},
}

local builtinVariableRemap = {}
for k,v in pairs(builtinVarInfos) do
  builtinVariableRemap[k] = v.name;
end
EmitContextHLSL.builtinVariableRemap = builtinVariableRemap

function EmitContextHLSL:emitBuiltins()
  local data = { input = "", output = "", temp = "", inputCopy = "", outputCopy = "" };

  for k,_ in pairs(self.builtinVariablesUsed) do
    local info = builtinVarInfos[k];
    local dir = info.dir;

    local key = dir.key;
    local copyKey = dir.copyKey;

    data[key] = data[key] .. info.type .. " " .. info.name .. " : " .. info.semantic .. ";\n"
    data.temp = data.temp .. "static " .. info.type .. " " .. info.name .. ";\n"
    data[copyKey] = data[copyKey] .. dir.dstPrefix .. info.name .. " = " .. dir.srcPrefix .. info.name .. ";\n"
  end

  return data;
end

function EmitContextHLSL:emitTextureCall(argStrings, args)
  local samplerExpr = argStrings[1] .. "_SAMPLER"
  if args[1].kind == "index" then
    local expr = args[1]
    samplerExpr = "(" .. self:emitExpr(expr.value.expression) .. "_SAMPLER)[" .. self:emitExpr(expr.index) .. "]"
  end

  return "(" .. argStrings[1] .. ").Sample(" .. samplerExpr .. ", " .. argStrings[2] .. ")";
end

function EmitContextHLSL:emitTextureLodCall(argStrings, args)
  local samplerExpr = argStrings[1] .. "_SAMPLER"
  if args[1].kind == "index" then
    local expr = args[1]
    samplerExpr = "(" .. self:emitExpr(expr.value.expression) .. "_SAMPLER)[" .. self:emitExpr(expr.index) .. "]"
  end

  return "(" .. argStrings[1] .. ").SampleLevel(" .. samplerExpr .. ", " .. argStrings[2] .. "," .. argStrings[3] .. ")";
end

function EmitContextHLSL:emitImageLoad(argStrings, args)
  return "(" .. argStrings[1] .. "[" .. argStrings[2] .. "])"
end

function EmitContextHLSL:emitImageStore(argStrings, args)
  return "(" .. argStrings[1] .. "[" .. argStrings[2] .. "] = " .. argStrings[3] .. ")"
end

function EmitContextHLSL:emitSamplerFunctionArg(texture, shouldIncludeType)
  local ret = ", "

  if shouldIncludeType then
    ret = ret .. "SamplerState "
  end

  if texture.kind == "index" then
    ret = ret .. "(" .. self:emitExpr(texture.value.expression) .. "_SAMPLER)[" .. self:emitExpr(texture.index) .. "]"
  else
    ret = ret .. self:getName(texture.symbol, texture.name) .. "_SAMPLER"
  end

  return ret
end

local function constructorEmitHelper(name)
  local function emit(ctxt, args)
    if #args == 1 then
      return "((" .. name .. ")(" .. args[1] .. "))";
    else
      local result = name .. "(";
      for i,v in ipairs(args) do
        if i ~= 1 then
          result =  result .. ", ";
        end
        result = result .. v;
      end
      result = result .. ")";
      return result;
    end
  end
  return emit;
end


EmitContextHLSL.builtinTypeMap = {
  sampler2D = "Texture2D",
  samplerCube = "TextureCube",
}

EmitContextHLSL.builtinFunctionMap = {
  inverse = "transpose", -- HACK HACK HACK!!! HLSL doesn't support matrix inverse by default

  mix = "lerp",

  texture = EmitContextHLSL.emitTextureCall,
  textureLod = EmitContextHLSL.emitTextureLodCall,

  imageLoad   = EmitContextHLSL.emitImageLoad,
  imageStore  = EmitContextHLSL.emitImageStore,

  groupMemoryBarrier = "GroupMemoryBarrierWithGroupSync",

  dFdx = "ddx",
  dFdy = "ddy",  
}

-- Vector types and constructors
local vecTypes = { {"vec", "float"} , {"ivec", "int"},
                   {"uvec", "uint"}, {"bvec", "bool"}
                 }
for _, vecTypeTuple in ipairs(vecTypes) do
  for i=2,4 do
    local typeName = vecTypeTuple[1] .. i
    local hlslName = vecTypeTuple[2] .. i
    EmitContextHLSL.builtinTypeMap[typeName] = hlslName
    EmitContextHLSL.builtinFunctionMap[typeName] = constructorEmitHelper(hlslName)
  end
end

-- Matrix types and constructors
local matTypes = { {"mat", "float"} , {"dmat", "double"}}
for _, matTypeTuple in ipairs(matTypes) do
  for i=2,4 do
    for j=2,4 do
      local typeName = matTypeTuple[1] .. i .. "x" .. j
      local hlslName = matTypeTuple[2] .. i .. "x" .. j
      EmitContextHLSL.builtinTypeMap[typeName] = hlslName
      EmitContextHLSL.builtinFunctionMap[typeName] = constructorEmitHelper(hlslName)

      if i==j then
        local typeName = matTypeTuple[1] .. i
        EmitContextHLSL.builtinTypeMap[typeName] = hlslName
        EmitContextHLSL.builtinFunctionMap[typeName] = constructorEmitHelper(hlslName)
      end
    end
  end
end


--


local function emitStageShader(builder, stage)

  local ctxt = EmitContextHLSL:new()

  local textureSamplers = ctxt:emitTextureSamplers(builder)
  local textureImages   = ctxt:emitTextureImages(builder)
  local buffers         = ctxt:emitBuffers(builder)
  local uniformBlocks = ctxt:emitUniformBlocks(builder)
  local uniforms      = ctxt:emitUniforms(builder)

  local code          = stage:getCodeList(ctxt, builder)
  local inputs        = stage:getInputsList(ctxt, builder)
  local outputs       = stage:getOutputsList(ctxt, builder)
  local temps         = stage:getTempList(ctxt, builder)

  local inputDecls = ctxt:emitAttrDecls(inputs);
  local inputTemps = ctxt:emitTemps(inputs);

  local copyInputToTemps = ctxt:emitAssign(inputs, "", "MS_input.");

  local outputSemanticPrefix = nil;
  if stage == TerraToShader.FragmentStage then
    outputSemanticPrefix = "SV_Target";
  end

  local outputDecls = ctxt:emitAttrDecls(outputs, outputSemanticPrefix);
  local outputTemps = ctxt:emitTemps(outputs);

  local copyTempsToOutput = ctxt:emitAssign(outputs, "MS_output.", "");

  local temps = ctxt:emitTemps(temps)

  local terra MS_main()
    [code]
  end
  MS_main:gettype()

  local defn = MS_main.definition

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

  local shaderOutputString = ""

  shaderOutputString = shaderOutputString .. "// struct definitions\n"
  for _,v in ipairs(ctxt.gStructDecls) do
    shaderOutputString = shaderOutputString .. v .. "\n"
  end

  -- Emit the info for builtin variables used.
  -- Note that we do this *after* we have emitted all the function definitions,
  -- so that we can be sure we caught any references to the builtins.
  local builtinVarInfo = ctxt:emitBuiltins();

  -- Now assemble the actual string of code

  shaderOutputString = shaderOutputString .. "\n// textureSamplers\n"
  shaderOutputString = shaderOutputString .. textureSamplers
  shaderOutputString = shaderOutputString .. "\n// texureImages\n"
  shaderOutputString = shaderOutputString .. textureImages
  shaderOutputString = shaderOutputString .. "\n// buffers\n"
  shaderOutputString = shaderOutputString .. buffers
  shaderOutputString = shaderOutputString .. "\n// uniform blocks\n"
  shaderOutputString = shaderOutputString .. uniformBlocks
  shaderOutputString = shaderOutputString .. "\n// uniforms\n"
  shaderOutputString = shaderOutputString .. uniforms

  local hasInputs = not (inputDecls == "" and builtinVarInfo.input == "")
  if hasInputs then
    shaderOutputString = shaderOutputString .. "\n// in variables\nstruct MS_INPUT {\n"
    shaderOutputString = shaderOutputString .. inputDecls .. builtinVarInfo.input
    shaderOutputString = shaderOutputString .. "};\n"
  end

  local hasOutputs = not (outputDecls == "" and builtinVarInfo.output == "")
  if hasOutputs then
    shaderOutputString = shaderOutputString .. "// out variables\nstruct MS_OUTPUT {\n"
    shaderOutputString = shaderOutputString .. outputDecls .. builtinVarInfo.output
    shaderOutputString = shaderOutputString .. "};\n"
  end

  shaderOutputString = shaderOutputString .. "// temp variables\n"
  shaderOutputString = shaderOutputString .. inputTemps
  shaderOutputString = shaderOutputString .. outputTemps
  shaderOutputString = shaderOutputString .. builtinVarInfo.temp
  shaderOutputString = shaderOutputString .. temps

  shaderOutputString = shaderOutputString .. "\n// function forward declarations\n"
  shaderOutputString = shaderOutputString .. ctxt.gShaderFunctionDecls .. "\n"
  shaderOutputString = shaderOutputString .. "\n// function definitions\n\n"
  shaderOutputString = shaderOutputString .. funcDefns .. "\n"

  shaderOutputString = shaderOutputString .. stage:emitLayoutInfo(ctxt, builder) .. "\n"
  shaderOutputString = shaderOutputString .. "void main("
  if hasInputs then
    shaderOutputString = shaderOutputString .. "MS_INPUT MS_input"
  end
  if hasInputs and hasOutputs then
    shaderOutputString = shaderOutputString .. ", "
  end
  if hasOutputs then
    shaderOutputString = shaderOutputString .. "out MS_OUTPUT MS_output"
  end
  shaderOutputString = shaderOutputString .. ")\n"

  shaderOutputString = shaderOutputString .. "{\n"
  shaderOutputString = shaderOutputString .. "  " .. copyInputToTemps .. builtinVarInfo.inputCopy .. "\n"
  shaderOutputString = shaderOutputString .. "  MS_main();\n"
  shaderOutputString = shaderOutputString .. "  " .. copyTempsToOutput .. builtinVarInfo.outputCopy .. "\n"
  shaderOutputString = shaderOutputString .. "}\n"

  return shaderOutputString
end

function TerraToShader.ComputeStage:emitLayoutInfo(ctxt, builder)
  if builder.numthreads == nil then
    return ""
  end
  return "[numthreads(" .. builder.numthreads.x ..
                    "," .. builder.numthreads.y ..
                    "," .. builder.numthreads.z .. ")]"
end

local function terraToHLSL(builder)
  local vertexCode = emitStageShader(builder, TerraToShader.VertexStage)
  local fragmentCode = emitStageShader(builder, TerraToShader.FragmentStage)
  local computeCode = emitStageShader(builder, TerraToShader.ComputeStage)

  return { vertexSourceCode = vertexCode, fragmentSourceCode = fragmentCode,
           computeSourceCode = computeCode,
         }
end

return terraToHLSL
