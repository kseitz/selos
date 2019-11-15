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

local Builtin = require "builtin"
local Gfx = require "gfx"
local Utils = require "utilities"

-- TODO: find a home
function generateCodeBlock(codeFunc, baseEnv, remapEnv)

  local env = baseEnv;
  if remapEnv then
    env = {}
    setmetatable(env, {__index = function(t,k)
      local v = remapEnv[k];
      if v then
        return v
      end;
      v = baseEnv[k];
      return v;
    end});
  end

  return codeFunc(env);
end

local function isVectorType(ty)
  local result = false
  pcall(function()
    if ty.metamethods.isShaderVectorType then
      result = true
    end
  end)
  return result
end

local function isMatrixType(ty)
  local result = false
  pcall(function()
    if ty.metamethods.isShaderMatrixType then
      result = true
    end
  end)
  return result
end

local function roundUpToPowerOftwo(val)
  local result = 1;
  while( result < val ) do
    result = result * 2;
  end
  return result;
end

local function getUniformBlockArrayMemberSizeAlign(ty, count)
  if ty:isprimitive() or isVectorType(ty) then
    local elemSize, elemAlign = getUniformBlockMemberSizeAlign(ty)

    -- base alignment must be at least 16
    local align = elemAlign
    if align < 16 then align = 16 end

    -- stride of element is element size, rounded up to alignment
    local stride = alignTo(elemSize, align)

    -- size of array is count times stride (so there may be
    -- padding at the end)
    local size = count * stride

    return size, align
  elseif isMatrixType(ty) then
    local colType = ty.metamethods.colType
    local colCount = ty.metamethods.colCount
    return getUniformBlockMemberSizeAlign(colType, colCount*count)
  else
    -- struct or array of arrays...
    local elemSize, elemAlign = getUniformBlockMemberSizeAlign(ty)
    local size = elemSize * count
    local align = elemAlign
    return size, align
  end
end

local function getFieldName(f)
  local result = nil
  pcall(function()
    result = "" .. f.field
  end)
  if result ~= nil then return result end
  error("foo")
end


local function getFieldType(f)
  local result = nil
  pcall(function()
    result = f.type
  end)
  if result ~= nil then return result end
  error("foo")
end

local function getUniformBlockMemberSizeAlign(ty)
  if ty:isprimitive() then
    local size = terralib.sizeof(ty);
    local align = size;
    return size, align
  elseif ty:isstruct() then
    if isVectorType(ty) then
      local size = terralib.sizeof(ty);
      local align = roundUpToPowerOftwo(size);
      return size, align
    elseif isMatrixType(ty) then
      local colType = ty.metamethods.colType
      local colCount = ty.metamethods.colCount
      return getUniformBlockArrayMemberSizeAlign(colType, colCount)
    else
      local subOffset = 0
      local align = 16 -- minimum alignment is 16
      for _,f in ipairs(ty.entries) do
        local fieldType = getFieldType(f)
        local fieldSize, fieldAlign = getUniformBlockMemberSizeAlign(fieldType)
        subOffset = align(subOffset, fieldAlign)
        subOffset = subOffset + fieldSize
        if fieldAlign > align then align = fieldAlign end
      end
      -- size will always be a multiple of alignment
      -- (this means there may be padding at the end)
      local size = align(subOffset, align)
      return size, align
    end
  elseif ty:isarray() then
    return getUniformBlockArrayMemberSizeAlign(ty.type, ty.N)
  else
    error("unhandled")
  end

end

local function alignTo(val, a)
  return bit.band(val + a-1, bit.bnot(a-1))
end

local primitiveTypeNames = {
  [float] = "float",
  [bool] = "bool",
  [int] = "int",
  [uint] = "uint",
}

local function getPrimitiveTypeName(ty)
  return primitiveTypeNames[ty]
end

local samplerTypeNames = {
  [sampler2D] = "sampler2D",
}

local function isSamplerType(ty)
  return samplerTypeNames[ty] ~= nil
end

local function getSamplerTypeName(ty)
  return samplerTypeNames[ty]
end


-- compute info for a type

local function getPaddedType(rawType, size)
  local rawSize = terralib.sizeof(rawType);
  if rawSize == size then
    return rawType;
  end

  local struct paddedType {
    __raw : rawType;
    __pad : uint8[size - rawSize];
  }

  -- TODO: need to allow implicit conversions...

  if terralib.sizeof(paddedType) ~= size then
    error("unexpected")
  end

  return paddedType;
end
getPaddedType = terralib.memoize(getPaddedType);

local getUniformBlockMemberTypeInfo = nil;

local function getUniformBlockMemberArrayTypeInfo(rawElemType, count)
  local elemTypeInfo = getUniformBlockMemberTypeInfo(rawElemType);

  -- base alignment must be at least 16
  local align = elemTypeInfo.align
  if align < 16 then align = 16 end

  -- stride of element is element size, rounded up to alignment
  local stride = alignTo(elemTypeInfo.size, align)

  -- size of array is count times stride (so there may be padding at the end)
  local size = count * stride

  -- create a padded element type if needed
  local newElemType = getPaddedType(elemTypeInfo.type, stride);

  local newType = newElemType[count];

  return { size = size, align = align, kind = "array", count = count, stride = stride, elementInfo = elemTypeInfo, type = newType }
end
getUniformBlockMemberArrayTypeInfo = terralib.memoize(getUniformBlockMemberArrayTypeInfo);

local function makeUniformBlockAggType(size, align, fields, origType)
  -- TODO: need to deal with alignment here!!!

  local struct Data
  {
    __raw : uint8[size];
  }

  local accessors = {}
  for _,field in ipairs(fields) do
    accessors[field.name] = function( self )
      return `( @[&field.type](&self.__raw[field.offset]) );
    end
  end

  Data.metamethods.__entrymissing = macro(function(name, self)
    local accessor = accessors[name];
    if accessor then
      return accessors[name](self);
    else
      error(string.format("no field named %s", name), 2);
      return nil
    end
  end)

  if origType then
    local function fromOrigType(oldVar, newVar)
      local ret = terralib.newlist()
      for _, entry in ipairs(origType.entries) do
        ret:insert(quote newVar.[entry.field] = oldVar.[entry.field] end)
      end
      return ret
    end

    Data.metamethods.__cast = function(from, to, expr)
      if from == origType and to == Data then
        return quote
          var ret : Data
          [fromOrigType(expr, ret)]
        in
          ret
        end
      end
      error(("unknown conversion %s to %s"):format(tostring(from),tostring(to)))
    end
  end

  return Data;
end


getUniformBlockMemberTypeInfo = function(ty)
  if ty == bool then
    return { size = 4, align = 4, type = uint32 }
  elseif ty:isprimitive() then
    local size = terralib.sizeof(ty);
    local align = size;
    return { size = size, align = align, kind = "scalar", type = ty }
  elseif isSamplerType(ty) then
    local size = 8
    local align = 8
    return { size = size, align = align, kind = "scalar", type = ty }
  elseif ty:isstruct() then
    if isVectorType(ty) then
      -- TODO: this is wrong for `bvec` types
      local elemType = ty.metamethods.elementType
      if (elemType == bool) then
        error("unhandled")
      end
      local elemCount = ty.metamethods.elementCount
      local elemSize = terralib.sizeof(elemType)

      local size = elemCount * elemSize
      local align = size
      if elemCount == 3 then
        align = 4 * elemSize
      end
      return { size = size, align = align, type = ty }
    elseif isMatrixType(ty) then
      -- TODO: this yields bad info...
      local elemType = ty.metamethods.elementType
      if (elemType == bool) then
        error("unhandled")
      end
      local colType = ty.metamethods.colType
      local colCount = ty.metamethods.colCount
      local rowCount = ty.metamethods.rowCount
      local arrInfo = getUniformBlockMemberArrayTypeInfo(colType, colCount)
      return { size = arrInfo.size, align = arrInfo.align, kind = "matrix", type = ty }
    else
      local offset = 0
      local align = 16 -- minimum alignment is 16
      local fields = {}
      for _,f in ipairs(ty.entries) do
        local fieldName = getFieldName(f)
        local fieldType = getFieldType(f)
        local fieldInfo = Utils.shallowCopy(getUniformBlockMemberTypeInfo(fieldType))
        offset = alignTo(offset, fieldInfo.align)
        fieldInfo.name = fieldName
        fieldInfo.offset = offset
        offset = offset + fieldInfo.size
        if fieldInfo.align > align then align = fieldInfo.align end
        table.insert(fields, fieldInfo)
      end
      -- size will always be a multiple of alignment
      -- (this means there may be padding at the end)
      local size = alignTo(offset, align)

      local newType = makeUniformBlockAggType(size, align, fields, ty);

      return { size = size, align = align, kind = "struct", fields = fields, type = newType }
    end
  elseif ty:isarray() then
    return getUniformBlockMemberArrayTypeInfo(ty.type, ty.N)
  else
    error("unhandled")
  end
end
getUniformBlockMemberTypeInfo = terralib.memoize(getUniformBlockMemberTypeInfo)


local function calculateNumberOfLocations(type)
  if type:isprimitive() or isVectorType(type) then
    -- TODO: handle dvec3 and dec4, which each take up 2 locations
    return 1
  elseif type:isarray() then
    return type.N * calculateNumberOfLocations(type.type)
  elseif type:isstruct() then
    assert(false, "Error-internal: Number of locations for struct type is unimplemented")
  else
    assert(false, "Error-internal: Unable to calculate number of locations")
  end
end


-- A `PipelineInfo` represents run-time information about a "class" of pipelines
struct PipelineInfo
{
  next      : &PipelineInfo;
  name      : rawstring;
  vertexSourceCode    : rawstring;
  fragmentSourceCode  : rawstring;
  computeSourceCode   : rawstring;
  program   : Gfx.ShaderProgram;
  vertexLayout  : Gfx.VertexLayout;
}

-- A Terra value to represent a linked list of all available `PipelineInfo`s
gLoadablePipelines = terralib.global(&PipelineInfo, nil);

-- A Lua table mapping the name of a pipeline to the Terra (runtime) representation
local allPipelines = {}

-- A Lua table mapping the name of a pipeline to the Lua representation of the pipeline
local allPipelineBuilders = {}

local M = {
  SystemInputs  = Builtin.systemInputs,
  SystemOutputs = Builtin.systemOutputs,
  initPipelines = terralib.newlist()
}


function M.findPipelineByName(name)
  return allPipelineBuilders[name];
end

function M.newPipelineBuilder(name, ...)
  if not name then
    error("Error: Pipeline must be given a name.")
  end
  -- data used to construct the shader
  local pipelineBuilder = {
    name = name,
    members = {},
    membersList = terralib.newlist(),
    extensionsList = terralib.newlist(),
    numthreads = nil,
    configurationOptions = nil,
    configurationOptionsList = nil,
    configurationEnv = nil,
    currentConfiguration = nil,

    sysInputs = {},
    sysOutputs = {},

    memberMetatables = {
      UniformBlock = {
        __indexTable = {
          __isTransparent = true,
          createInstanceRepresentation = function(self,env)
            if self.__rep ~= nil then
              return self.__rep
            end

            local rep = {};
            setmetatable(rep, {
              __index = function(t,k)
                --print("uniform block rep index", t,k)
                local u = self.uniforms[k];

                -- if mapping name->rep, then recurse for member->rep
                if u then
                  local r = t[u];
                  --print("found uniform", k, u, r);
                  t[k] = r;
                  return r;
                end

                -- otherwise, ensure that we have the member
                for _,ul in ipairs(self.uniformsList) do
                  if ul == k then
                    u = ul;
                    break;
                  end
                end
                -- lookup fails if we have no matching member
                if not u then return nil end;


                local r = u:createInstanceRepresentation(env);
                --print("found UNIFORM", u.name, u, r);
                t[u] = r;
                return r;
              end,
            })

            -- TODO: need to implement this
            self.__rep = rep
            return rep
          end,
          desugar = function(self, desugared, env, rep, meta)
            --print("desugar uniform block", self.name, rep, "meta:", meta, meta.__binding, meta.__size);
            local desugaredBlock = { name = self.name, uniformsList = terralib.newlist(), binding = meta.__binding };
            for _,u in ipairs(self.uniformsList) do
              local uRep = rep[u];
              u:desugar(desugaredBlock, env, uRep, meta[u]);
            end

            desugared.uniformBlocksList:insert(desugaredBlock);
          end,
          generateMetadata = function(self, pipelineMetadata, counts)
            local metadata = {}

            local binding = counts.uniformBlockCount
            --print("generateMeta", pipelineMetadata, binding);
            counts.uniformBlockCount = counts.uniformBlockCount + 1
            self.binding = binding

            local offset = 0
            local fields = terralib.newlist()

            for _, uniform in ipairs(self.uniformsList) do
              -- TODO: compute offsets based on std140
              local typeInfo = getUniformBlockMemberTypeInfo(uniform.type)
              offset = alignTo(offset, typeInfo.align)

              -- TODO: probably want some meta-table magic here...
              local fieldInfo = {}
              local fieldMetaTable = {  name = uniform.name, offset = offset,
                                        info = typeInfo, type = typeInfo.type }
              fieldMetaTable.__index = fieldMetaTable
              setmetatable(fieldInfo, fieldMetaTable)

              offset = offset + typeInfo.size

              fields:insert(fieldInfo)
              metadata[uniform.name] = fieldInfo
            end

            local align = 16
            local size = alignTo(offset, align)

            local newType = makeUniformBlockAggType(size, align, fields)

            local blockMetaTable = {
              __binding = binding,
              __fields  = fields,
              __size    = size,
              __type    = newType,
              astype    = function(self)
                return newType
              end,
            }
            blockMetaTable.__index = blockMetaTable
            setmetatable(metadata, blockMetaTable)
            --print("block metadata", metadata, metadata.__binding)
            pipelineMetadata[self] = metadata
          end,
          __isUniformBlock = true,
        },
        __index = function(table, key)
          local indexTable = getmetatable(table).__indexTable
          if indexTable[key] then
            return indexTable[key]
          else
            return table.uniforms[key]
          end
        end,
      },
      TextureSampler = {
        __indexTable = {
          createInstanceRepresentation = function(self,env)
            -- TODO: should actually *create* the representation here, not just return the pre-made one...
            return self.decl;
          end,
          desugar = function(self, desugared, env, rep, meta)
            desugared.textureSamplersList:insert(self)
          end,
          generateMetadata = function(self, pipelineMetadata, counts)
            local binding = Gfx.Bindings.getNextTextureSamplerBinding(counts)
            if self.type:isarray() then
              counts.textureSamplerCount = counts.textureSamplerCount + self.type.N
            else
              counts.textureSamplerCount = counts.textureSamplerCount + 1
            end
            self.binding = binding
            pipelineMetadata[self] = { __binding = binding, __type = self.type }
          end,
        },
        __index = function(table, key)
          local indexTable = getmetatable(table).__indexTable
          return indexTable[key]
        end,
      },
      TextureImage = {
        __indexTable = {
          createInstanceRepresentation = function(self,env)
            -- TODO: should actually *create* the representation here, not just return the pre-made one...
            return self.decl;
          end,
          desugar = function(self, desugared, env, rep, meta)
            desugared.textureImagesList:insert(self)
          end,
          generateMetadata = function(self, pipelineMetadata, counts)
            local binding = Gfx.Bindings.getNextTextureImageBinding(counts)
            if self.type:isarray() then
              counts.textureImageCount = counts.textureImageCount + self.type.N
            else
              counts.textureImageCount = counts.textureImageCount + 1
            end
            self.binding = binding
            pipelineMetadata[self] = { __binding = binding, __type = self.type,
                                       __textureFormat = self.textureFormat,
                                       __memoryMode = self.memoryMode,
                                     }
          end,
        },
        __index = function(table, key)
          local indexTable = getmetatable(table).__indexTable
          return indexTable[key]
        end,

      },
      Buffer = {
        __indexTable = {
          createInstanceRepresentation = function(self,env)
            -- TODO: should actually *create* the representation here, not just return the pre-made one...
            return self.decl;
          end,
          desugar = function(self, desugared, env, rep, meta)
            desugared.buffersList:insert(self)
          end,
          generateMetadata = function(self, pipelineMetadata, counts)
            local binding = Gfx.Bindings.getNextBufferBinding(counts)
            counts.bufferCount = counts.bufferCount + 1
            self.binding = binding
            pipelineMetadata[self] = { __binding = binding, __type = self.type,
                                     }
          end,
        },
        __index = function(table, key)
          local indexTable = getmetatable(table).__indexTable
          return indexTable[key]
        end,
      },
      Uniform = {
        __indexTable = {
          createInstanceRepresentation = function(self,env)
            -- TODO: should actually *create* the representation here, not just return the pre-made one...
            --print("creating instance rep for uniform", self.name);
            return global(self.type);
          end,
          desugar = function(self, desugared, env, rep, meta)
            --print("desugar uniform", self.name, rep);
            desugared.uniformsList:insert({name = self.name, decl = rep})
          end,
          generateMetadata = function(self, pipelineMetadata, counts)
            local loc = counts.legacyUniformCount
            counts.legacyUniformCount = counts.legacyUniformCount + 1
            self.location = loc
            pipelineMetadata[self] = { __location = loc }
          end,
        },
        __index = function(table, key)
          local indexTable = getmetatable(table).__indexTable
          return indexTable[key]
        end,
      },
      Input   = {
        __indexTable = {
          createInstanceRepresentation = function(self,env)
            -- TODO: should actually *create* the representation here, not just return the pre-made one...
            return self.decl;
          end,
          desugar = function(self, desugared, env, rep, meta)
            desugared.inputsList:insert(self)
          end,
          generateMetadata = function(self, pipelineMetadata, counts)
            local loc = counts.vertexInputCount
            local numLocs = calculateNumberOfLocations(self.type)
            counts.vertexInputCount = counts.vertexInputCount + numLocs
            self.location = loc
            pipelineMetadata[self] = { __location = loc }
          end,
        },
        __index = function(table, key)
          local indexTable = getmetatable(table).__indexTable
          return indexTable[key]
        end
      },
      Varying = {
        __indexTable = {
          createInstanceRepresentation = function(self,env)
            -- TODO: should actually *create* the representation here, not just return the pre-made one...
            return self.decl
          end,
          desugar = function(self, desugared, env, rep, meta)
            desugared.varyingsList:insert(self)
          end,
          generateMetadata = function(self, pipelineMetadata, counts)
            local loc = counts.varyingCount
            local numLocs = calculateNumberOfLocations(self.type)
            counts.varyingCount = counts.varyingCount + numLocs
            self.location = loc
            pipelineMetadata[self] = { __location = loc }
          end,
        },
        __index = function(table, key)
          local indexTable = getmetatable(table).__indexTable
          return indexTable[key]
        end
      },
      Output  = {
        __indexTable = {
          createInstanceRepresentation = function(self,env)
            -- TODO: should actually *create* the representation here, not just return the pre-made one...
            return self.decl;
          end,
          desugar = function(self, desugared, env, rep, meta)
            desugared.outputsList:insert(self)
          end,
          generateMetadata = function(self, pipelineMetadata, counts)
            local loc = counts.fragmentOutputCount
            local numLocs = calculateNumberOfLocations(self.type)
            counts.fragmentOutputCount = counts.fragmentOutputCount + numLocs
            self.location = loc
            pipelineMetadata[self] = { __location = loc }
          end,
        },
        __index = function(table, key)
          local indexTable = getmetatable(table).__indexTable
          return indexTable[key]
        end
      },
      VertexTemp = {
        __indexTable = {
          createInstanceRepresentation = function(self,env)
            -- TODO: should actually *create* the representation here, not just return the pre-made one...
            return self.decl;
          end,
          desugar = function(self, desugared, env, rep, meta)
            desugared.verttempsList:insert(self)
          end,
        },
        __index = function(table, key)
          local indexTable = getmetatable(table).__indexTable
          return indexTable[key]
        end
      },
      FragmentTemp = {
        __indexTable = {
          createInstanceRepresentation = function(self,env)
            -- TODO: should actually *create* the representation here, not just return the pre-made one...
            return self.decl;
          end,
          desugar = function(self, desugared, env, rep, meta)
            desugared.fragtempsList:insert(self)
          end,
        },
        __index = function(table, key)
          local indexTable = getmetatable(table).__indexTable
          return indexTable[key]
        end
      },
      ComputeTemp = {
        __indexTable = {
          createInstanceRepresentation = function(self,env)
            -- TODO: should actually *create* the representation here, not just return the pre-made one...
            return self.decl;
          end,
          desugar = function(self, desugared, env, rep, meta)
            desugared.computetempsList:insert(self)
          end,
        },
        __index = function(table, key)
          local indexTable = getmetatable(table).__indexTable
          return indexTable[key]
        end
      },
      VertexCode = {
        __indexTable = {
          createInstanceRepresentation = function(self,env)
            -- TODO: need to implement this
            return {}
          end,
          desugar = function(self, desugared, env, rep, meta)
            desugared.vertexCode:insert(generateCodeBlock(self.codeFn, self.env, env));
          end,
        },
        __index = function(table, key)
          local indexTable = getmetatable(table).__indexTable
          return indexTable[key]
        end
      },
      FragmentCode = {
        __indexTable = {
          createInstanceRepresentation = function(self,env)
            -- TODO: need to implement this
            return {}
          end,
          desugar = function(self, desugared, env, rep, meta)
            desugared.fragmentCode:insert(generateCodeBlock(self.codeFn, self.env, env));
          end,
        },
        __index = function(table, key)
          local indexTable = getmetatable(table).__indexTable
          return indexTable[key]
        end
      },
      ComputeCode = {
        __indexTable = {
          createInstanceRepresentation = function(self,env)
            -- TODO: need to implement this
            return {}
          end,
          desugar = function(self, desugared, env, rep, meta)
            desugared.computeCode:insert(generateCodeBlock(self.codeFn, self.env, env));
          end,
        },
        __index = function(table, key)
          local indexTable = getmetatable(table).__indexTable
          return indexTable[key]
        end
      }
    },
  }

  setmetatable(pipelineBuilder, {
    __isPipeline = true,
    __index = function(table, key)
      local extensionsList = rawget(table,"extensionsList")
      for i = #extensionsList, 1, -1 do
        local extFn = extensionsList[i][key]
        if extFn then
          return extFn
        end
      end
    end
  })

  -- Add extensions to the builder
  local extensionsArg = {n=select('#',...),...}
  for _, ext in ipairs(extensionsArg) do
    for k,v in pairs(ext.memberMetatables) do
      if pipelineBuilder.memberMetatables[k] and v.__indexTable then
        local m = pipelineBuilder.memberMetatables[k]
        for k2, v2 in pairs(v.__indexTable) do
          m.__indexTable[k2] = v2
        end
      else
        pipelineBuilder.memberMetatables[k] = v
      end
    end

    pipelineBuilder.extensionsList:insert(ext)
  end

  function pipelineBuilder:addMember(listMember, name)
    self.membersList:insert(listMember)

    if name then
      self.members[name] = listMember
    end
  end

  -- Params:
  --  code - lua function that returns a Terra quote to append to the body of the shader's main function
  --  env - the default environment that should be used when evaluating the function
  function pipelineBuilder:createVertexCode(c, env)
    local code = c
    if type(code) ~= "function" then
      code = function(e) return c end
    end
    code(env)
    local member = { codeFn = code, env = env }
    setmetatable(member, self.memberMetatables.VertexCode)
    return member
  end
  function pipelineBuilder:createFragmentCode(c, env)
    local code = c
    if type(code) ~= "function" then
      code = function(e) return c end
    end
    code(env)
    local member = { codeFn = code, env = env }
    setmetatable(member, self.memberMetatables.FragmentCode)
    return member
  end
  function pipelineBuilder:createComputeCode(c, env)
    local code = c
    if type(code) ~= "function" then
      code = function(e) return c end
    end
    code(env)
    local member = { codeFn = code, env = env }
    setmetatable(member, self.memberMetatables.ComputeCode)
    return member
  end

  -- TODO Maybe use this function to specify which system inputs
  --      are actually used in the shader
  function pipelineBuilder:declareSystemInput(name)
    local sysInput = Builtin.systemInputs[name]

    if sysInput == nil then
      error(name .. " is not a valid system input.\n", 2)
    end

    self.sysInputs[name] = sysInput
    return sysInput
  end

  -- TODO Maybe use this function to specify which system outputs
  --      are actually used in the shader
  function pipelineBuilder:declareSystemOutput(name)
    local sysOutput = Builtin.systemOutputs[name]

    if sysOutput == nil then
      error(name .. " is not a valid system output.\n", 2)
    end

    self.sysOutputs[name] = sysOutput
    return sysOutput
  end

  function pipelineBuilder:createUniformBlock(blockName, ...)
    local pb = self;
    if not blockName or blockName == "" then
      error("Uniform blocks must have a name.", 2)
    end

    local args = {n=select('#',...),...}

    local uniformBlock = {
      name = blockName,
      binding = nil,
      uniforms = {},
      uniformsList = terralib.newlist(),

      declareUniform = function(self, ty, name, attrs, init)
        if ty == nil or name == nil then
          local stackLevel = 2
          if debug.getinfo(2).name == "declareUniformBlock" then
            stackLevel = 3
          end

          error("Each uniform block member must be specified with "
            .. "a type and a name.", stackLevel)
        end

        local uniform = { name = name, type = ty, attributes = attrs, init = init };
        setmetatable(uniform, pb.memberMetatables.Uniform);
        self.uniforms[name] = uniform;
        self.uniformsList:insert(uniform);
        return uniform;
      end
    }
    -- TODO consider having uniform block members be at the top level
    -- and everything else in the metatable
    setmetatable(uniformBlock, self.memberMetatables.UniformBlock)

    if args.n == 0 then
      return uniformBlock
    end

    -- if only 1 vararg, then it must be a binding
    if args.n == 1 then
      if args[1] and type(args[1]) ~= "number" then
        error("Uniform block binding must be a number.", 2)
      end

      uniformBlock.binding = args[1]
      return uniformBlock
    end

    local startPos = 1

    -- if first vararg is a number, then it must be a binding
    if type(args[1]) == "number" then
      uniformBlock.binding = args[1]
      startPos = 2
    end

    for i = startPos, args.n, 2 do
      local ty = args[i]
      local name = args[i+1]

      uniformBlock:declareUniform(ty, name)
    end

    return uniformBlock
  end

  local function createInOutUnif(builder, memberType, ty, name, location)
    if location and type(location) ~= "number" then
      error("Location qualifier must be a number", 2)
    end

    local decl = global(ty)
    local member = { name = name, type = ty, decl = decl, location = location }
    setmetatable(member, memberType)

    return member
  end

  function pipelineBuilder:createUniform(ty, name, location)
    return createInOutUnif(self, self.memberMetatables.Uniform, ty, name, location)
  end

  function pipelineBuilder:createInput(ty, name, location)
    return createInOutUnif(self, self.memberMetatables.Input, ty, name, location)
  end

  function pipelineBuilder:createVarying(ty, name, location)
    return createInOutUnif(self, self.memberMetatables.Varying, ty, name, location)
  end

  function pipelineBuilder:createOutput(ty, name, location)
    return createInOutUnif(self, self.memberMetatables.Output, ty, name, location)
  end

  function pipelineBuilder:setNumthreads(x,y,z)
    self.numthreads = {x=x,y=y,z=z}
  end

  function pipelineBuilder:getDefaultConfiguration()
    local config = {}
    for k, v in pairs(self.configurationOptions) do
      local defaultConfig = v:getDefaultConfiguration()
      if defaultConfig == nil then
        error("Unable to get default configuration for Configuration Option '" .. k .. "'")
      end
      config[k] = defaultConfig
    end

    local function getConfigurationID()
      local configID = 0
      local totalNumBits = 0
      for _, v in ipairs(self.configurationOptionsList) do
        local c = config[v.name]
        local id, numBits = c:getConfigurationID()
        configID = bit.lshift(configID, numBits) + id
        totalNumBits = totalNumBits + numBits
      end
      return configID, totalNumBits
    end

    local function isValid()
      local isValid = true
      for _, v in pairs(config) do
        isValid = isValid and v:isValid()
      end
      return isValid
    end

    setmetatable(config, {
      __index = function(t, k)
        if k == "getConfigurationID" then
          return getConfigurationID
        elseif k == "isValid" then
          return isValid
        end
        return t[k]
      end
    })

    return config
  end

  function pipelineBuilder:setConfiguration(config)
    self.currentConfiguration = config
    for k, v in pairs(config) do
      if self.configurationOptions[k] == nil then
        error("Configuration option '" .. k .. "' is invalid for shader '" .. self.name .. "'", 2)
      end
      self.configurationEnv[k] = v
    end

    for _, v in ipairs(self.configurationOptionsList) do
      local c = config[v.name]
      if c == nil then
        error("Configuration option '" .. v.name .. "' missing when setting configuration for shader '" .. self.name .. "'", 2)
      end
    end

    self.configurationEnv.getConfigurationID = function() return config:getConfigurationID() end

    -- If the pipeline is already complete, then we need to re-desugar whenever the configuration is changed
    if self.isComplete then
      self.desugaredPipeline = self:desugar()
    end
  end

  function pipelineBuilder:setConfigurationOptions(options, optionsList, env)
    self.configurationOptions = options
    self.configurationOptionsList = optionsList
    self.configurationEnv = env

    for k, v in pairs(options) do
      v:addMembers(self)
    end

    self:setConfiguration(self:getDefaultConfiguration())
  end

  function pipelineBuilder:createTextureSampler(ty, name, binding)
    if binding and type(binding) ~= "number" then
      error("Location qualifier must be a number", 2)
    end

    local decl = global(ty)
    local member = {name = name, decl = decl, type = ty, binding = binding }
    setmetatable(member, self.memberMetatables.TextureSampler)

    return member
  end

  function pipelineBuilder:createTextureImage(ty, name, binding, textureFormat, memoryMode)
    if binding and type(binding) ~= "number" then
      error("Location qualifier must be a number", 2)
    end

    local memMode = memoryMode and memoryMode or Gfx.MapMode.ReadWrite

    local decl = global(ty)
    local member = {name = name, decl = decl, type = ty, binding = binding,
                    textureFormat = textureFormat,
                    shaderTextureFormat  = Gfx.ShaderTextureFormat[textureFormat],
                    memoryMode = memMode,
                    shaderMemoryMode = Gfx.ShaderMapMode[memMode],
                   }
    setmetatable(member, self.memberMetatables.TextureImage)

    return member
  end

  function pipelineBuilder:createBuffer(ty, name, binding)
    if binding and type(binding) ~= "number" then
      error("Location qualifier must be a number", 2)
    end

    local decl = global(ty)
    local member = {name = name, decl = decl, type = ty, binding = binding}
    setmetatable(member, self.memberMetatables.Buffer)

    return member
  end

  function pipelineBuilder:createVertexTemp(ty, name)
    local decl = global(ty)
    local member = { name = name, decl = decl }
    setmetatable(member, self.memberMetatables.VertexTemp)

    return member
  end

  function pipelineBuilder:createFragmentTemp(ty, name)
    local decl = global(ty)
    local member = { name = name, decl = decl }
    setmetatable(member, self.memberMetatables.FragmentTemp)

    return member
  end

  function pipelineBuilder:createComputeTemp(ty, name, isShared)
    if isShared == nil then
      isShared = false
    end
    local decl = global(ty)
    local member = { name = name, decl = decl, isShared = isShared }
    setmetatable(member, self.memberMetatables.ComputeTemp)

    return member
  end

  local function addUniformBlocksRec(self, sdr, bindingsUsed)
    for _,a in ipairs(sdr.aggs) do
      addUniformBlocksRec(self, a, bindingsUsed);
    end

    for _,b in ipairs(sdr.uniformBlocksList) do
      if b.binding then
        bindingsUsed[b.binding] = true;
      end
    end
  end

  function pipelineBuilder:getAllUniformBlocks()
    local ret = terralib.newlist()
    for _, m in ipairs(self.membersList) do
      if m.__isUniformBlock then
        ret:insert(m)
      end
    end
    return ret
  end

  function pipelineBuilder:findMember(k)
    local v = self.members[k];
    if v then return v end;

    for _,m in ipairs(self.membersList) do
      if k == m then
        return m;
      end
    end

    return nil;
  end

  function pipelineBuilder:createInstanceRepresentation(env, getMemberRepFunc)
    if not env then env = {} end
    local rep = {}
    local mapMembertoRep = {}

    local subEnv = setmetatable({},
      {__index = function(t,k)
        local v = rep[k];
        if v then return v end
        return env[k];
      end});

    if not getMemberRepFunc then
      getMemberRepFunc = function(m, e)
        return m:createInstanceRepresentation(e);
      end
    end

    setmetatable(mapMembertoRep, {
      __index = function(t,m)
        local mRep = getMemberRepFunc(m, subEnv);
        t[m] = mRep;
        return mRep;
      end
    });

    setmetatable(rep, {
      __index = function(t,k)
        --print("pipeline rep __index:", k);
        local m = self:findMember(k);
        if m then
          local mRep = mapMembertoRep[m];
          --print("rep for member:", m, m.name, "->", mRep);
          rep[k] = mRep;
          return mRep;
        end

        -- try to see through transparent members
        for _,t in ipairs(self.membersList) do
          if t.__isTransparent then
            --print("looking for", k, "in", t.name);
            local tRep = rep[t];
            local mRep = tRep[k];
            if mRep then
              --print("found", mRep);
              return mRep;
            end
          end
        end

        return nil;
      end,

      mapMembertoRep= mapMembertoRep,
    })

    return rep;
  end

  function pipelineBuilder:getInstanceRepresentation(env)
    local r = self.__rep;
    if not r then
      r = self:createInstanceRepresentation(env);
      self.__rep = r;
    end
    return r;
  end

  function pipelineBuilder:desugarInner(desugaredPipeline, desugarEnv, pipelineRep, pipelineMeta)
    if not desugaredPipeline then
      desugaredPipeline = {
        uniformBlocksList   = terralib.newlist(),
        textureSamplersList = terralib.newlist(),
        textureImagesList   = terralib.newlist(),
        buffersList         = terralib.newlist(),
        uniformsList        = terralib.newlist(),
        inputsList          = terralib.newlist(),
        varyingsList        = terralib.newlist(),
        outputsList         = terralib.newlist(),
        vertexCode          = terralib.newlist(),
        fragmentCode        = terralib.newlist(),
        computeCode         = terralib.newlist(),
        verttempsList       = terralib.newlist(),
        fragtempsList       = terralib.newlist(),
        computetempsList    = terralib.newlist(),
        numthreads          = self.numthreads,
      }
    end

    if not pipelineRep then
      pipelineRep = self:getInstanceRepresentation();
    end

    if not desugarEnv then
      desugarEnv = pipelineRep;
    end

    if not pipelineMeta then
      pipelineMeta = self.metadata;
    end

    local mapMembertoRep = getmetatable(pipelineRep).mapMembertoRep;
    for _, m in ipairs(self.membersList) do
      m:desugar(desugaredPipeline, desugarEnv, mapMembertoRep[m], pipelineMeta[m])
    end

    return desugaredPipeline;
  end

  function pipelineBuilder:desugar()
    local desugaredPipeline = self:desugarInner();

    for _, e in ipairs(self.extensionsList) do
      if e.onDidDesugarMembers then
        e:onDidDesugarMembers(self, desugaredPipeline);
      end
    end

    return desugaredPipeline;
  end

  function pipelineBuilder:generateMetadataInner(pipelineMetadata, counts)
    for _, m in ipairs(self.membersList) do
      if m.generateMetadata then
        m:generateMetadata(pipelineMetadata, counts)
      end
    end

    return pipelineMetadata;
  end

  function pipelineBuilder:generateMetadata(pipelineMetadata)
    if not pipelineMetadata then
      pipelineMetadata = {};
    end

    local counts = {
      uniformBlockCount   = 0,
      textureSamplerCount = 0,
      textureImageCount   = 0,
      bufferCount         = 0,
      legacyUniformCount  = 0,
      vertexInputCount    = 0,
      fragmentOutputCount = 0,
      varyingCount        = 0,
    }

    self:generateMetadataInner(pipelineMetadata, counts);

    for k,v in pairs(counts) do
      pipelineMetadata[k] = v;
    end

    return pipelineMetadata;
  end


  function pipelineBuilder:complete()

    if self.isComplete then
      return;
    end

    self.isComplete = true;

    local pipelineMetadata = {}

    for _, e in ipairs(self.extensionsList) do
      if e.preComplete then
        e:preComplete(self, pipelineMetadata)
      end
    end

    self:generateMetadata(pipelineMetadata);
    self.metadata = pipelineMetadata;

    local desugaredPipeline = self:desugar();
    self.desugaredPipeline = desugaredPipeline

    for _, e in ipairs(self.extensionsList) do
      e:complete(self, desugaredPipeline, pipelineMetadata)
    end
  end

  function pipelineBuilder:generateShaderSourceCode()
    self:complete()

    -- check if any member/extension should inhibit code generation
    local inhibitCodeGeneration = false;
    for _,e in ipairs(self.extensionsList) do
      if e.shouldInhibitCodeGeneration and e:shouldInhibitCodeGeneration() then
        inhibitCodeGeneration = true;
      end
    end

    if inhibitCodeGeneration then
      return nil;
    end

    local shaderCode = Gfx.codeGenFn(self.desugaredPipeline)

    if (not shaderCode.vertexSourceCode) or (not shaderCode.fragmentSourceCode) then
      error("Unable to generate shader source code for this pipeline.\n", 2)
    end

    return shaderCode
  end

  -- We are going to be a little reckless and just stash a bunch of data inside
  -- the pipeline metaobject itself, to make our lives easier. Eventually we
  -- might want to clean this up so that pipelines provide a well-defined place
  -- to chain dependent info (e.g., GLSL or HLSL-specific representations).
  --
  -- The `loadable` parameter (default: `true`) indicates whether the pipeline should
  -- be directly executable (so we will generate a shader program for it).
  function pipelineBuilder:finalize(loadable, forceLoad)
    if loadable == nil then loadable = true end
    if forceLoad == nil then forceLoad = false end

    -- Allocate runtime info
    -- Note: This is being done before the `generateMetadata` call below,
    -- because a derived pipeline type might want to look at the `runtimeInfo` pointer

    -- If we are doing hot reload, the runtime info will already being in the
    -- allPipelines list, so fetch it and override the data in it.
    local runtimeInfo = allPipelines[self.name]
    local doingReload = true

    -- Otherwise:
    -- We create a Terra global to store the info about this pipeline.
    -- It is a global rather than a constant because we will use the `program`
    -- field to cache the generated GL program object, and also because
    -- shader reload will clobber the pointers to the shader source code strings.
    if not runtimeInfo then
      doingReload = false
      runtimeInfo = terralib.global(PipelineInfo)
      allPipelines[self.name] = runtimeInfo
    end

    allPipelineBuilders[self.name] = self;

    self.runtimeInfo = `( &runtimeInfo )

    self:complete()

    -- Generate compile time information
    local shaderCode = self:generateShaderSourceCode()
    if shaderCode then
      self.vertexSourceCode = shaderCode.vertexSourceCode
      self.fragmentSourceCode = shaderCode.fragmentSourceCode
      self.computeSourceCode = shaderCode.computeSourceCode
    else
      loadable = false;

      -- ugly hack here, where we *quote* nil, just so that it will actually stay in the table,
      -- since Lua has the behavior of just throwing away nil-valued members...
      self.vertexSourceCode = `nil
      self.fragmentSourceCode = `nil
      self.computeSourceCode = `nil
--      self.vertexSourceCode = ""
--      self.fragmentSourceCode = ""
     end


    -- Generate runtime information
    local initList = terralib.newlist()

    if not doingReload then
      initList:insert(quote runtimeInfo.next = gLoadablePipelines end)

      if loadable then
        -- If this pipeline is one we want to load/reload at runtime, we need to
        -- put it into the linked list of all the loadable pipelines.
        initList:insert(quote gLoadablePipelines = &runtimeInfo end)
      end
    end

    initList:insert(quote
      runtimeInfo.name                = [tostring(self.name)]
      runtimeInfo.vertexSourceCode    = self.vertexSourceCode
      runtimeInfo.fragmentSourceCode  = self.fragmentSourceCode
      runtimeInfo.computeSourceCode   = self.computeSourceCode
    end)

    local init = terra()
      [initList]
    end

    if doingReload or forceLoad then
      init()
    else
      M.initPipelines:insert(quote init() end)
    end

  end


  return pipelineBuilder
end


return M
