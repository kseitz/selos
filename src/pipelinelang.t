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

local PipelineBuilder = require "pipelineBuilder"
local Utils = require "utilities"

local Gfx = require "gfx"

local function parseLocation(lex)
  if not lex:matches("(") then
    return function(env) return nil end
  end

  local openparen = lex:next()
  local loc = lex:luaexpr()
  lex:expectmatch(")", "(", openparen.linenumber)
  return loc
end

local function parseVariableDecl(lex)
  local varName = lex:expect(lex.name).value
  lex:expect(":")
  local ty = lex:luaexpr()
  return varName, ty
end

local function parseTemp(lex)
  local temp = {}
  temp.name, temp.ty = parseVariableDecl(lex)

  if lex:matches("=") then
    lex:next()
    temp.init = lex:terraexpr()
  end

  return temp
end

local function tryParseAttribute(lex)
  if not lex:matches("@") then
    return nil
  end

  lex:next()
  local n = lex:expect(lex.name).value
  lex:expect("(")
  local v = lex:luaexpr()
  lex:expect(")")

  return { name = n, value = v }
end

local function newParser()
  local parser = {}

  function parser:parse(lex, ctorList, parserCallback)
    if lex:matches("uniform") then
      lex:next()
      local block = { uniforms = terralib.newlist() }
      block.binding = parseLocation(lex)
      block.name = lex:expect(lex.name).value

      local openBracket = lex:expect("{")

      repeat
        local uniform = { attributes = terralib.newlist() }

        local attrib = tryParseAttribute(lex)
        if attrib then
          uniform.attributes:insert(attrib)
        end

        uniform.name, uniform.ty = parseVariableDecl(lex)
        if lex:matches("=") then
          lex:next()
          uniform.init = lex:terraexpr()
        end
        block.uniforms:insert(uniform)
      until not lex:matches(lex.name) and not lex:matches("@")

      lex:expectmatch("}","{", openBracket.linenumber)

      ctorList:insert(function(pb, parent, env)
        local b = pb:createUniformBlock(block.name, block.binding(env))
        for _, u in ipairs(block.uniforms) do
          local attrs = {}
          for _, a in ipairs(u.attributes) do
            attrs[a.name] = a.value(env)
          end
          if u.init then
            u.init = u.init(env)
          else
            local paramTy = u.ty(env)
            if not paramTy:isarray() then
              local param = pb:createParam(u.name .. "_param", u.ty(env))
              u.init = param.decl
              parent:addMember(param, param.name)
            end
          end
          b:declareUniform(u.ty(env), u.name, attrs, u.init)
        end
        parent:addMember(b, block.name)
      end)

      return true
    elseif lex:matches("textureSampler") then
      lex:next()
      local texSamp = {}
      texSamp.binding = parseLocation(lex)
      texSamp.name, texSamp.ty = parseVariableDecl(lex)

      ctorList:insert(function(pb, parent, env)
        local member =
          pb:createTextureSampler(texSamp.ty(env), texSamp.name, texSamp.binding(env))
        parent:addMember(member, member.name)
      end)

      return true
    elseif lex:matches("textureImage") then
      lex:next()
      local texImg = {}
      texImg.binding = (function(env) return nil end)

      if not lex:matches("(") then
        lex:error("Expected '(' to begin specifying format")
      end

      local openParen = lex:expect("(")
      -- TODO verify that this texture format works with textureImages
      local textureFormat = lex:luaexpr()

      local memoryMode = nil
      if lex:matches(",") then
        lex:expect(",")
        memoryMode = lex:luaexpr()
      else
        memoryMode = (function(env) return nil end)
      end

      lex:expectmatch(")", "(", openParen.linenumber)

      texImg.name, texImg.ty = parseVariableDecl(lex)

      ctorList:insert(function(pb, parent, env)
        local member =
          pb:createTextureImage(texImg.ty(env), texImg.name, texImg.binding(env),
                                textureFormat(env), memoryMode(env))
        parent:addMember(member, member.name)
      end)

      return true
    elseif lex:matches("buffer") then
      local linenumber = lex:next().linenumber
      if Gfx.isOpenGL then
        print(lex.source .. ":" .. linenumber .. ": " ..
          "[Warning] The OpenGL backend does not automatically pad/align the host-side structs used with buffers. " ..
          "You should manually pad them, and use buffers with caution.\n")
      end
      local buf = {}
      buf.binding = (function(env) return nil end)

      buf.name, buf.ty = parseVariableDecl(lex)

      ctorList:insert(function(pb, parent, env)
        local member = pb:createBuffer(buf.ty(env), buf.name, buf.binding(env))
        parent:addMember(member, member.name)
      end)

      return true
    elseif lex:matches("input") then
      lex:next()
      local input = {}
      input.location = parseLocation(lex)
      input.name, input.ty = parseVariableDecl(lex)

      ctorList:insert(function(pb, parent, env)
        local member = pb:createInput(input.ty(env), input.name, input.location(env))
        parent:addMember(member, member.name)
      end)

      return true
    elseif lex:matches("out") then
      lex:next()
      local out = {}
      out.location = parseLocation(lex)
      out.name, out.ty = parseVariableDecl(lex)

      ctorList:insert(function(pb, parent, env)
        local member = pb:createOutput(out.ty(env), out.name, out.location(env))
        parent:addMember(member, member.name)
      end)

      return true
    elseif lex:matches("varying") then
      lex:next()
      local varying = {}
      varying.location = parseLocation(lex)
      varying.name, varying.ty = parseVariableDecl(lex)

      ctorList:insert(function(pb, parent, env)
        local member = pb:createVarying(varying.ty(env), varying.name, varying.location(env))
        parent:addMember(member, member.name)
      end)

      return true
    elseif lex:matches("numthreads") then
      lex:next()

      if not lex:matches("(") then
        lex:error("Expected '(' when specifying numthreads")
      end

      local openParen = lex:expect("(")
      local x = lex:luaexpr()
      lex:expect(",")
      local y = lex:luaexpr()
      lex:expect(",")
      local z = lex:luaexpr()
      lex:expectmatch(")", "(", openParen.linenumber)

      ctorList:insert(function(pb, parent, env)
        local x = x(env)
        local y = y(env)
        local z = z(env)
        if x <= 0 or y <= 0 or z <= 0 then
          lex:error(lex.source .. ":" .. openParen.linenumber .. ": " ..
            "arguments to numthreads(x,y,z) must all be > 0")
        end
        pb:setNumthreads(x,y,z)
      end)

      return true
    elseif lex:matches("ConfigurationOptions") then
      lex:next()

      if not lex:matches("{") then
        lex:error("Expected '{' when specifying ConfigurationOptions")
      end
      local openBracket = lex:expect("{")

      local optionsList = terralib.newlist()
      while not lex:matches("}") do
        local optionName = lex:expect(lex.name).value
        lex:expect("=")
        local optionValue = lex:luaexpr()

        optionsList:insert({name = optionName, value = optionValue})
      end
      lex:expectmatch("}", "{", openBracket.linenumber)

      ctorList:insert(function(pb, parent, env)
        local options = {}
        local newOptionsList = terralib.newlist()
        for _, v in ipairs(optionsList) do
          local value = v.value(env)
          if value == nil then
            lex:error("[Error]: Found a nil value when specifying Configuration Option '" .. v.name .. "'")
          end
          options[v.name] = value
          newOptionsList:insert( {name = v.name, value = value} )
        end
        pb:setConfigurationOptions(options, newOptionsList, env)
      end)

      return true
    elseif lex:matches("vertex") or lex:matches("fragment") or lex:matches("compute") then
      local isVertex = lex:matches("vertex")
      local isFragment = lex:matches("fragment")
      local isCompute = lex:matches("compute")
      local function getVertOrFrag(pb)
        if isVertex then
          return { createTemp = pb.createVertexTemp,   createCode = pb.createVertexCode   }
        elseif isFragment then
          return { createTemp = pb.createFragmentTemp, createCode = pb.createFragmentCode }
        elseif isCompute then
          return { createTemp = pb.createComputeTemp, createCode = pb.createComputeCode }
        end
      end

      if lex:lookaheadmatches("var") then
        lex:next() -- consume "vertex" or "fragment"
        lex:expect("var")
        local isShared = false
        if lex:matches("(") then
          local openParen = lex:next()
          if lex:matches("shared") then
            lex:next()
            isShared = true
          end
          lex:expectmatch(")", openParen)
        end
        local t = parseTemp(lex)
        ctorList:insert(function(pb, parent, env)
          local vertOrFrag = getVertOrFrag(pb)
          local member = vertOrFrag.createTemp(pb, t.ty(env), t.name, isShared)
          parent:addMember(member, member.name)

          -- TODO Temp initializers should not have to be quotes,
          --      but the do in the current implementation. Fix this.
          if t.init then
            local tempFn = function(newEnv) return quote
              [env[t.name]] = [t.init(newEnv)]
            end end
            local initCode = vertOrFrag.createCode(pb, tempFn, env)
            parent:addMember(initCode)
          end
        end)
        return true
      elseif lex:lookaheadmatches("code") then
        lex:next() -- consume "vertex" or "fragment"
        lex:next()
        local code = lex:terrastats()
        lex:expect("end")

        ctorList:insert(function(pb, parent, env)
          local vertOrFrag = getVertOrFrag(pb)
          local member = vertOrFrag.createCode(pb, code, env)
          parent:addMember(member)
        end)

        return true
      elseif lex:lookaheadmatches(lex.name) then
        lex:next() -- consume "vertex" or "fragment"
        local fn = {}
        fn.name = lex:expect(lex.name).value

        lex:expect("=")
        if not lex:matches("terra") then
          lex:error("Expected terra function declaration")
        end
        fn.body = lex:luaexpr()

        ctorList:insert(function(pb, parent, env)
          local vertOrFrag = getVertOrFrag(pb)
          local body = fn.body(env)
          body.name = fn.name
          body.anchor.name = fn.name
          env[fn.name] = body
        end)

        return true
      end
    elseif lex:matches("local") then
      lex:next()

      if lex:matches("terra") then
        lex:error("Syntax unsupported.  Use 'local <fnName> = terra(...)' syntax instead")
      end

      local fn = {}
      fn.name = lex:expect(lex.name).value

      lex:expect("=")
      fn.body = lex:luaexpr()

      ctorList:insert(function(pb, parent, env)
        local body = fn.body(env)
        body.name = fn.name
        body.anchor.name = fn.name
        env[fn.name] = body
      end)

      return true
    elseif lex:matches("terra") then
      lex:error("Syntax unsupported.  Use 'local <fnName> = terra(...)' syntax instead")
      return false
    end

    return false
  end

  return parser
end


local function createShader(self, lex)
  lex:expect("pipeline")

  local parsers = terralib.newlist()
  for i = #self.extensions, 1, -1 do
    parsers:insert(self.extensions[i].newParser())
  end
  parsers:insert(newParser())

  local ctorList = terralib.newlist()

  local sdrName = lex:expect(lex.name).value

  while not lex:matches("{") do
    local parsed = false;
    for _, parser in ipairs(parsers) do
      if parser.parseHeader then
        if parser:parseHeader(lex, ctorList) then
          parsed = true;
          break
        end
      end
    end

    if not parsed then
      lex:error("Unexpected symbol")
    end
  end

  local openBracket = lex:expect("{")

  local function parserCallback(l, cList, endtoken)
    repeat
      local parsed = false
      for _, parser in ipairs(parsers) do
        if parser:parse(l, cList, parserCallback) then
          parsed = true
          break
        end
      end

      if not parsed then
        lex:error("Unexpected symbol")
      end
    until lex:matches(endtoken)
  end

  parserCallback(lex, ctorList, "}")

  lex:expectmatch("}","{", openBracket.linenumber)

  local ctor = function(environment_function)
    local builderExtensions = terralib.newlist()
    for _, ext in ipairs(self.extensions) do
      for _, v in ipairs(ext.builderExtensions) do
        if not Utils.find(builderExtensions, v) then
          builderExtensions:insert(v)
        end
      end
    end

    local builder = PipelineBuilder.newPipelineBuilder(sdrName, unpack(builderExtensions))

    local rep = builder:getInstanceRepresentation();

    local outerEnv = environment_function();
    local env = {}
    setmetatable(env, { __index = function(t,k)
      local v = rep[k];
      if v then return v end;
      v = outerEnv[k];
      return v;
    end});

    -- add system inputs and outputs
    for k, v in pairs(PipelineBuilder.SystemInputs) do
      env[k] = v
    end
    for k, v in pairs(PipelineBuilder.SystemOutputs) do
      env[k] = v
    end

    for _, fn in ipairs(ctorList) do
      fn(builder, builder, env)
    end

    builder:finalize()

    if table.getn(builder.desugaredPipeline.computeCode) ~= 0 then
      if builder.desugaredPipeline.numthreads == nil then
        lex:error(lex.source .. ":" .. openBracket.linenumber .. ": " ..
          "Must specify numthreads(x,y,z) when using a compute shader")
      end
    end

    return builder
  end

  return ctor, { sdrName }
end


local pipelineLanguage = {
  name = "pipelinelang";
  entrypoints = terralib.newlist({ "pipeline" });
  keywords = terralib.newlist(
  { "uniform", "textureSampler", "textureImage",
    "buffer",
    "input", "varying", "out",
    "numthreads",
    "ConfigurationOptions",
    "vertex", "fragment", "compute", "code",
    "shared",
  });

  statement = createShader;
  localstatement = createShader;

  extensions = terralib.newlist();
}

function pipelineLanguage:addExtension(ext)
  self.name = self.name .. "+" .. ext.name
  if ext.entrypoints then
    self.entrypoints:insertall(ext.entrypoints)
  end
  if ext.keywords then
    self.keywords:insertall(ext.keywords)
  end
  self.extensions:insert(ext)
end

return pipelineLanguage
