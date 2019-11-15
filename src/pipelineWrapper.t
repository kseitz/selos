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
  #include <stdio.h>
  #include <stdlib.h>
]])
local Array = require "array"
local Gfx = require "gfx"
require "gfx-utils"
local MS = require "selos"
local Utils = require "utilities"
local ExtensionBuilder = require "extensionBuilder"

struct TextureSampler
{
  texture : Gfx.Texture;
  sampler : Gfx.SamplerState;
}

local TextureImage = Gfx.Texture;

struct VertexStream
{
  buffer : Gfx.Buffer;
  stride : uint32;
  offset : uint32;
}

-- A set of integers, one for each resource type we need to count.
-- This is useful when we want to pass around a bunch of "base offsets"
-- at which to start filling stuff in, or a bunch of counts to be
-- used when advancing those offsets.
struct ResourceCounters
{
  uniformBuffer   : int;
  textureSampler  : int;
  textureImage    : int;
  buffer          : int;
  vertexStream    : int;
}

terra ResourceCounters:init()
  self.uniformBuffer = 0;
  self.textureSampler = 0;
  self.textureImage = 0;
  self.buffer = 0;
  self.vertexStream = 0;
end

-- We want to be able to add instances of `ResourceCounters`, so that client
-- code doesn't need to be updated any time we add a new kind of resource to be counted.
terra ResourceCounters.metamethods.__add( left : ResourceCounters, right : ResourceCounters )
  var result : ResourceCounters;
  result.uniformBuffer = left.uniformBuffer + right.uniformBuffer;
  result.textureSampler = left.textureSampler + right.textureSampler;
  result.textureImage = left.textureImage + right.textureImage;
  result.buffer = left.buffer + right.buffer;
  result.vertexStream = left.vertexStream + right.vertexStream;
  return result;
end


-- We define smaller "info" structs for the various kinds of members that a pipeline can have.
-- The most important feature these share is an `offset` field, which is the byte offset into
-- the corresponding pipeline instance type where the data for the member can be found.

struct UniformBlockInfo
{
  name    : rawstring;  -- original (un-mangled) named of the buffer
  binding : int;        -- index to bind to ("local" index in case of composition)
  offset  :  int;       -- offset of the buffer in the pipeline instance
}

struct TextureSamplerInfo
{
  name    : rawstring;
  binding : int;
  offset  : int;
  type    : int;
}

struct TextureImageInfo
{
  name    : rawstring;
  binding : int;
  access  : Gfx.MapModeT;
  format  : Gfx.TextureFormatT;
  offset  : int;
}

struct BufferInfo {
  name    : rawstring;
  binding : int;
  offset  : int;
}

struct RequirementInfo
{
  name    : rawstring;
  offset  : int;
  base    : ResourceCounters; -- base resource binding offsets to use when binding the pipeline stored at the requirement
}

struct VertexAttributeInfo
{
  name      : rawstring;
  location  : uint32;
  format    : uint32; -- Gfx.AttributeFormat.*
  size      : uint32;
  alignment : uint32;
}

struct VertexStreamInfo
{
  name    : rawstring;
  binding : int;
  offset  : int;
}

struct PipelineData
{
  -- For now, we store a poiner to the `PipelineInfo` here, but really this and `PipelineInfo` should be
  -- the same type (or at least co-allocated).
  --
  -- The sticking point here is that the "composition" pipeline builder currently generates the wrapper types
  -- and the `PipelineData`, and I'm not entirely clear why that needs to be the case.
  pipelineInfo  : &PipelineInfo;

  ubos              : Array(UniformBlockInfo);
  textureSamplers   : Array(TextureSamplerInfo);
  textureImages     : Array(TextureImageInfo);
  buffers           : Array(BufferInfo);
  vertexAttributes  : Array(VertexAttributeInfo);
  vertexStreams     : Array(VertexStreamInfo);
}

-- We define a catch-all type for pipeline instances, which all the generated pipeline-specific types will conform to.

struct PipelineInstance
{
  -- The only field every pipeline needs is a pointer to the
  -- corresponding `PipelineData` that describes its layout at runtime.
  pipelineData : &PipelineData;

  -- The rest of the data will be specific to a given pipeline type.
}

-- Given a pipeline instance, we can bind its state at runtime just using the meta-data in the `PipelineData`.
-- This function takes as input a `ResourceCounters` with base offsets to use when binding (e.g., the start offset
-- to use for uniform buffer bindings).

PipelineInstance.methods.bind = terralib.overloadedfunction("bind")
PipelineInstance.methods.bind:adddefinition(
terra( self : &PipelineInstance, context : &Gfx.Context, base : &ResourceCounters ) : {}
  var data = self.pipelineData;
  var info = data.pipelineInfo;

  -- TODO: should we be binding the shader here?

  -- We loop over the uniform blocks and texture-samplers in the pipeline instance, and bind them.
  for i=0, data.ubos.count do
    var uboInfo = data.ubos:get(i)
    var ubo = @[&Gfx.Buffer]( [&int8](self) + uboInfo.offset );
    --C.printf("UBO[%d] = %s\n", base.uniformBuffer + uboInfo.binding, uboInfo.name);
    context:setUniformBuffer(base.uniformBuffer + uboInfo.binding, ubo);
  end

  for i=0, data.textureSamplers.count do
    var texSampInfo = data.textureSamplers:get(i)
    var texSamp = @[&TextureSampler]( [&int8](self) + texSampInfo.offset );
    --C.printf("TexSamp[%d] = %s\n", base.textureSampler + texSampInfo.binding, texSampInfo.name);
    context:setTextureSampler(base.textureSampler + texSampInfo.binding, texSampInfo.type, texSamp.texture, texSamp.sampler)
  end

  for i=0, data.textureImages.count do
    var texImageInfo = data.textureImages:get(i)
    var texImage = @[&TextureImage]( [&int8](self) + texImageInfo.offset );
    context:setTextureImage(base.textureImage + texImageInfo.binding, texImage, texImageInfo.access, texImageInfo.format)
  end

  for i=0, data.buffers.count do
    var bufInfo = data.buffers:get(i)
    var buf = (@[&Gfx.RWStructuredBufferBase]( [&int8](self) + bufInfo.offset));
    context:setRWStructuredBuffer(base.buffer + bufInfo.binding, buf);
  end

  for i=0, data.vertexStreams.count do
    var info = data.vertexStreams:get(i);
    var stream = @[&VertexStream]( [&int8](self) + info.offset );
    context:setVertexBuffer(base.vertexStream + info.binding, stream.buffer, stream.stride, stream.offset );
  end
end)

-- When binding a "root" pipeline, we just go ahead and binding it with all the resource counters starting at zero.

PipelineInstance.methods.bind:adddefinition(
terra( self : &PipelineInstance, context : &Gfx.Context ) : {}
  var base : ResourceCounters;
  base:init();
  self:bind(context, &base);
end)

local ext = ExtensionBuilder:newExtensionBuilder("WrapperBuilderExtension")

-- Uniform Block
ext:registerMemberType("UniformBlock")
function ext.UniformBlock:getWrapperType(pipelineMetadata)
  local blockInfo = pipelineMetadata[self]

  local sym = symbol(&blockInfo:astype())
  local setupCode = terralib.newlist()

  for _, v in ipairs(self.uniformsList) do
    local name = v.name
    if v.init then
      setupCode:insert(quote
        sym.[name] = [v.init]
      end)
    end
  end

  return Gfx.UniformBuffer(blockInfo, sym, setupCode);
end

function ext.UniformBlock:addToWrapperStorageInit(pipelineMetadata, offset, dataStorage, quoteList)
  local blockInfo = pipelineMetadata[self]
  quoteList:insert(`dataStorage.ubos:add( UniformBlockInfo{
    [self.name],
    blockInfo.__binding,
    [offset]
  }))
end

function ext.UniformBlock:addToWrapperObjectInit(pipelineMetadata, obj, device, quoteList)
  quoteList:insert(`obj.[self.name]:init(device))
end

-- TextureSampler
ext:registerMemberType("TextureSampler")
function ext.TextureSampler:getWrapperType(pipelineMetadata)
  local type = pipelineMetadata[self].__type

  if type:isarray() then
    return TextureSampler[type.N]
  end

  return TextureSampler;
end

function ext.TextureSampler:addToWrapperStorageInit(pipelineMetadata, offset, dataStorage, quoteList)
  local info = pipelineMetadata[self]

  local count = 1
  local ty = info.__type
  if info.__type:isarray() then
    count = info.__type.N
    ty = ty.type
  end

  local tyInt = 0
  if ty == sampler2D then
    tyInt = 0
  elseif ty == samplerCube then
    tyInt = 1
  else
    error("Invalid textureSampler type\n")
  end

  for i = 0, count-1 do
    quoteList:insert(`dataStorage.textureSamplers:add( TextureSamplerInfo{
      [self.name],
      [info.__binding + i],
      [offset] + i * sizeof(TextureSampler),
      tyInt
    }))
  end
end

-- TextureImage
ext:registerMemberType("TextureImage")
function ext.TextureImage:getWrapperType(pipelineMetadata)
  local type = pipelineMetadata[self].__type

  if type:isarray() then
    return TextureImage[type.N]
  end

  return TextureImage
end

function ext.TextureImage:addToWrapperStorageInit(pipelineMetadata, offset, dataStorage, quoteList)
  local info = pipelineMetadata[self]

  local count = 1
  if info.__type:isarray() then
    count = info.__type.N
  end

  for i=0, count-1 do
    quoteList:insert(`dataStorage.textureImages:add( TextureImageInfo{
      [self.name],
      [info.__binding + i],
      [info.__memoryMode],
      [info.__textureFormat],
      [offset] + i * sizeof(TextureImage)
    }))
  end
end

-- Buffers
ext:registerMemberType("Buffer")
function ext.Buffer:getWrapperType(pipelineMetadata)
  local info = pipelineMetadata[self]
  return Gfx.RWStructuredBuffer(info.__type.type, info.__type.N)
end

function ext.Buffer:addToWrapperStorageInit(pipelineMetadata, offset, dataStorage, quoteList)
  local info = pipelineMetadata[self]

  quoteList:insert(`dataStorage.buffers:add( BufferInfo{
    [self.name],
    [info.__binding],
    [offset]
  }))
end

function ext.Buffer:addToWrapperObjectInit(pipelineMetadata, obj, device, quoteList)
  quoteList:insert(`obj.[self.name]:init(device))
end

-- Param
ext:registerMemberType("Param")
function ext.Param:createInstanceRepresentation(env)
  return self.decl
end

function ext.Param:desugar(desugared, env, rep, meta)
end

function ext.Param:generateMetadata(pipelineMetadata, counts)
end

function ext.Param:getWrapperType(pipelineMetadata)
  return self.type
end

function ext.Param:getSetupCode(pipelineMetadata, obj, ctxt)
  local ret = terralib.newlist()
  ret:insert(quote
    [self.decl] = obj.[self.name]
  end)
  return ret
end

function ext:createParam(name, ty)
  local decl = global(ty)
  local param = { name = name,
                  type = ty,
                  decl = decl,
                }
  setmetatable(param, self.memberMetatables.Param)
  return param
end

-- Input
ext:registerMemberType("Input")
function ext.Input:addToWrapperStorageInit(pipelineMetadata, offset, dataStorage, quoteList)
  local info = pipelineMetadata[self]

  if self.type:isarray() then
    for i=0, self.type.N do
      quoteList:insert(`dataStorage.vertexAttributes:add( VertexAttributeInfo{
        [self.name],
        info.__location + i,
        [Gfx.getAttributeFormatForType(self.type.type)],
        terralib.sizeof(self.type.type),
        [Utils.alignof(self.type.type)]
      }))
    end
  else
    quoteList:insert(`dataStorage.vertexAttributes:add( VertexAttributeInfo{
      [self.name],
      info.__location,
      [Gfx.getAttributeFormatForType(self.type)],
      terralib.sizeof(self.type),
      [Utils.alignof(self.type)]
    }))
  end
end

-- VertexStream
ext:registerMemberType("VertexStream")
function ext.VertexStream:getWrapperType(pipelineMetadata, newType)
  return VertexStream;
end

function ext.VertexStream:addToWrapperStorageInit(pipelineMetadata, offset, dataStorage, quoteList)
  local info = pipelineMetadata[self]
  quoteList:insert(`dataStorage.vertexStreams:add( VertexStreamInfo{
    [self.name],
    info.__binding,
    offset
  }))
end

local function generateType(pb, metadata)
  local newType = terralib.types.newstruct(tostring(pb.name) .. "Type")
  metadata.Type = newType
  local typeDataStorage = global(PipelineData);
  local typeData = global(&PipelineData);

  newType.entries:insert( {field = "data", type = &PipelineData })

  local typeStorageInitList = terralib.newlist()
  for _, m in ipairs(pb.membersList) do
    -- TODO: Members that contain other members need to add their members to the wrapper.
    --       The current getWrapperType() and addToWrapperStorageInit() functions are
    --       insufficient for this purpose, so I'm adding an addToWrapperType() function
    --       as an option that effectively replaces both of these calls.
    --       We should consider unifying these three functions in the future
    --       if it makes sense to do so.
    if m.addToWrapperType then
      m:addToWrapperType(metadata, newType, typeDataStorage, typeStorageInitList)
    end
    if m.getWrapperType then
      local wrapperType = m:getWrapperType(metadata, newType)
      if wrapperType then
        newType.entries:insert( {field = m.name, type = wrapperType} );
      end
    end
  end

  -- Second loop over members to run wrapper storage init code
  -- because Terra eagerly type-checks quotes, so we must wait until
  -- newType is complete before we can find the offsets of each member
  for _, m in ipairs(pb.membersList) do
    local offset = `0;
    if m.getWrapperType and m:getWrapperType(metadata, newType) then
      offset = `[terralib.offsetof(newType, m.name)]
    end
    if m.addToWrapperStorageInit then
      m:addToWrapperStorageInit(metadata, offset, typeDataStorage, typeStorageInitList)
    end
  end


  -- all instances of this type will share the same `PipelineData` instance, which we will
  -- lazily initialize when it is needed.
  local terra getTypeData()
    -- TODO: thread-safe initialization?
    if typeData == nil then
      typeDataStorage.pipelineInfo = pb.runtimeInfo; -- TODO: initialize this properly!
      [typeStorageInitList]
      typeData = &typeDataStorage;
    end
    return typeData;
  end

  terra newType:init( device : &Gfx.Device )
    self.data = getTypeData()
    [(function()
      local quoteList = terralib.newlist()
      for _, m in ipairs(pb.membersList) do
        if m.addToWrapperObjectInit then
          m:addToWrapperObjectInit(metadata, self, device, quoteList)
        end
      end
      return quoteList
    end)()]
  end

  -- allow a pointer to an instance of `newType` to be converted to an `&PipelineInstance`,
  -- so that newType is conceptually a "subclass"
  function newType.metamethods.__cast(fromType, toType, fromExp)
    if fromType:ispointer() and toType:ispointer() then
      if toType.type == PipelineInstance then
        return `toType(fromExp);
      end
    end
    error("invalid conversion"); -- TODO: re-use existing conversion and error-message machinery here!
  end

  terra newType:bind(ctxt : &Gfx.Context)
    [&PipelineInstance](self):bind(ctxt)
  end

  newType.__methodmissingTable = {}
  newType.metamethods.__methodmissing = macro(function(methodname, obj, ...)
    local args = terralib.newlist {...}
    local methodImpl = newType.__methodmissingTable[methodname]
    if methodImpl then
      return methodImpl(obj, unpack(args))
    else
      error("no such method " .. methodname .. " defined for type " .. metadata.Type.name)
    end
  end)

  pb.new = terra( device : &Gfx.Device ) : &newType
    var ret : &newType = [&newType](C.malloc(sizeof(newType)))
    ret:init(device)
    return ret
  end

  pb.getPipelineData = getTypeData;
  metadata.Type = newType
end

function ext:complete(pb, desugaredPipeline, metadata)
  generateType(pb, metadata)

  terra pb.metadata.Type:setup( ctxt : &Gfx.Context )
    [(function()
      local ret = terralib.newlist()
      for _, m in ipairs(pb.membersList) do
        if m.getSetupCode then
          ret:insertall(m:getSetupCode(metadata, self, ctxt))
        end
      end
      ret:insert(quote
        self:bind(ctxt)
      end)
      return ret
    end)()]
  end
end


local wrapperBuilderExtension = ext:finishBuild()

return wrapperBuilderExtension
