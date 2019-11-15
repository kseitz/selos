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

-- gfx-opengl.t

local C = terralib.includecstring([[
  #include <math.h>
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
]])

local Array = require "array"
local GL = require "opengl"
local SDL = require "sdl"

local Gfx_OpenGL = {}
Gfx_OpenGL.isOpenGL = true

Gfx_OpenGL.codeGenFn = require "terraToGLSL"

-- TODO this probably need to be divided into different
-- variables for different types of identifiers
Gfx_OpenGL.IdType = GL.GLuint

--

Gfx_OpenGL.NilBuffer = 0;
Gfx_OpenGL.NilTexture = 0;
Gfx_OpenGL.NilProgram = 0;

local fixup0 = terralib.new(vec4, 1.0, 0.0, 0.0, 0.0)
local fixup1 = terralib.new(vec4, 0.0, -1.0, 0.0, 0.0)
local fixup2 = terralib.new(vec4, 0.0, 0.0, 1.0, 0.0)
local fixup3 = terralib.new(vec4, 0.0, 0.0, 0.0, 1.0)
local fixupArray  = terralib.new(vec4[4], {fixup0, fixup1, fixup2, fixup3})
local fixup = terralib.new(mat4, fixupArray)
Gfx_OpenGL.FixupMatrix = terralib.constant(fixup)

local cubefixup0 = terralib.new(vec4, 1.0, 0.0, 0.0, 0.0)
local cubefixup1 = terralib.new(vec4, 0.0, 1.0, 0.0, 0.0)
local cubefixup2 = terralib.new(vec4, 0.0, 0.0, 1.0, 0.0)
local cubefixup3 = terralib.new(vec4, 0.0, 0.0, 0.0, 1.0)
local cubefixupArray  = terralib.new(vec4[4], {cubefixup0, cubefixup1, cubefixup2, cubefixup3})
local cubefixup = terralib.new(mat4, cubefixupArray)
Gfx_OpenGL.CubeFixupMatrix = terralib.constant(cubefixup)


local Gfx_OpenGL_MapMode = {}
Gfx_OpenGL.MapMode = Gfx_OpenGL_MapMode;
Gfx_OpenGL.MapModeT = int;

Gfx_OpenGL_MapMode.ReadOnly     = 0;
Gfx_OpenGL_MapMode.WriteOnly    = 1;
Gfx_OpenGL_MapMode.ReadWrite    = 2;
Gfx_OpenGL_MapMode.WriteDiscard = 3;

local Gfx_OpenGL_ShaderMapMode = {}
Gfx_OpenGL.ShaderMapMode = Gfx_OpenGL_ShaderMapMode;

Gfx_OpenGL_ShaderMapMode[Gfx_OpenGL_MapMode.ReadOnly] = "readonly"
Gfx_OpenGL_ShaderMapMode[Gfx_OpenGL_MapMode.WriteOnly] = "writeonly"

local terra toGLMapMode(m : Gfx_OpenGL.MapModeT) : GL.GLenum
  if m == Gfx_OpenGL.MapMode.ReadOnly then
    return GL.GL_READ_ONLY
  elseif m == Gfx_OpenGL.MapMode.WriteOnly then
    return GL.GL_WRITE_ONLY
  elseif m == Gfx_OpenGL.MapMode.ReadWrite then
    return GL.GL_READ_WRITE
  elseif m == Gfx_OpenGL.MapMode.WriteDiscard then
    return GL.GL_WRITE_ONLY
  end

  return -1;
end

--

local Bindings = {}

function Bindings.getNextTextureSamplerBinding(counts)
  return counts.textureSamplerCount
end

function Bindings.getNextBufferBinding(counts)
  return counts.bufferCount
end

function Bindings.getNextTextureImageBinding(counts)
  return counts.textureImageCount
end

Gfx_OpenGL.Bindings = Bindings


local struct Gfx_OpenGL_AttributeFormatInfo
{
    componentCount  : GL.GLint;
    componentType   : GL.GLenum;
    normalized      : GL.GLboolean;
}

local struct Gfx_OpenGL_VertexAttributeDesc
{
  streamIndex : GL.GLuint;
  format      : Gfx_OpenGL_AttributeFormatInfo;
  offset      : GL.GLsizei;
}

local kMaxVertexStreams = 16;
Gfx_OpenGL.MaxVertexStreams = kMaxVertexStreams;

local struct Gfx_OpenGL_VertexLayoutDesc
{
  attributes : Gfx_OpenGL_VertexAttributeDesc[kMaxVertexStreams]; 
  attributeCount : int;
}

local struct Gfx_OpenGL_VertexLayoutImpl
{
  desc : Gfx_OpenGL_VertexLayoutDesc;
}
local Gfx_OpenGL_VertexLayout = &Gfx_OpenGL_VertexLayoutImpl;
Gfx_OpenGL.VertexLayout = Gfx_OpenGL_VertexLayout;


local struct Gfx_OpenGL_Context
{
  vertexLayout          : Gfx_OpenGL_VertexLayout;
  vertexStreamBuffers   : GL.GLuint[kMaxVertexStreams];
  vertexStreamStrides   : int[kMaxVertexStreams];
  vertexStreamOffsets   : int[kMaxVertexStreams];
  dirtyVertexAttributes : uint32;
  indexFormat           : GL.GLenum;
}
Gfx_OpenGL.Context = Gfx_OpenGL_Context;

local struct Gfx_OpenGL_Device
{
  defaultContext : Gfx_OpenGL_Context;
  window : &SDL.SDL_Window;
}
Gfx_OpenGL.Device = Gfx_OpenGL_Device;

Gfx_OpenGL.CLEAR_COLOR = GL.GL_COLOR_BUFFER_BIT;
Gfx_OpenGL.CLEAR_DEPTH = GL.GL_DEPTH_BUFFER_BIT;
Gfx_OpenGL.CLEAR_STENCIL = GL.GL_STENCIL_BUFFER_BIT;


local Gfx_OpenGL_PrimitiveType = {}
Gfx_OpenGL.PrimitiveType = Gfx_OpenGL_PrimitiveType;
Gfx_OpenGL_PrimitiveType.Triangles = GL.GL_TRIANGLES;

local Gfx_OpenGL_DepthFunc = {}
Gfx_OpenGL.DepthFunc = Gfx_OpenGL_DepthFunc;
Gfx_OpenGL_DepthFunc.Less = GL.GL_LESS;


local Gfx_OpenGL_FillMode = {}
Gfx_OpenGL.FillMode = Gfx_OpenGL_FillMode;
Gfx_OpenGL_FillMode.Point = GL.GL_POINT;
Gfx_OpenGL_FillMode.WireFrame = GL.GL_LINE;
Gfx_OpenGL_FillMode.Solid = GL.GL_FILL;



local Gfx_OpenGL_Usage = {}
Gfx_OpenGL.Usage = Gfx_OpenGL_Usage;
Gfx_OpenGL_Usage.Default = GL.GL_STATIC_DRAW;
Gfx_OpenGL_Usage.Immutable = GL.GL_STATIC_DRAW;
Gfx_OpenGL_Usage.Dynamic = GL.GL_DYNAMIC_DRAW;
Gfx_OpenGL_Usage.Staging = GL.GL_DYNAMIC_DRAW; -- TODO: get this right...

local Gfx_OpenGL_CPUAccess = {}
Gfx_OpenGL.CPUAccess = Gfx_OpenGL_CPUAccess;
Gfx_OpenGL_CPUAccess.None = 0;
Gfx_OpenGL_CPUAccess.Write = 0x1;
Gfx_OpenGL_CPUAccess.Read = 0x2;
Gfx_OpenGL_CPUAccess.ReadWrite = 0x3;

local Gfx_OpenGL_IndexFormat = {}
Gfx_OpenGL.IndexFormat = Gfx_OpenGL_IndexFormat;
Gfx_OpenGL_IndexFormat.UInt32 = GL.GL_UNSIGNED_INT;
Gfx_OpenGL_IndexFormat.UInt16 = GL.GL_UNSIGNED_SHORT;

local Gfx_OpenGL_attributeFormats = {
  { "Vec2", 2, GL.GL_FLOAT, GL.GL_FALSE },
  { "Vec3", 3, GL.GL_FLOAT, GL.GL_FALSE },
  { "Vec4", 4, GL.GL_FLOAT, GL.GL_FALSE },
  { "RGBA8", 4, GL.GL_UNSIGNED_BYTE, GL.GL_TRUE },
}

local Gfx_OpenGL_AttributeFormat = {}
for i,v in ipairs(Gfx_OpenGL_attributeFormats) do
  local name = unpack(v)
  Gfx_OpenGL_AttributeFormat[name] = i
end

local function Gfx_OpenGL_getVertexAttributeFormatInfoImpl( f, info )
    local body = terralib.newlist();
    for i,v in ipairs(Gfx_OpenGL_attributeFormats) do
      local name, count, type, normalized = unpack(v);
      table.insert(body, quote
        if f == i then
          info.componentCount = count;
          info.componentType = type;
          info.normalized = normalized;
          return info;
        end
      end);
    end
    return body;
end

local terra Gfx_OpenGL_getVertexAttributeFormatInfo( f : int )
  var info : Gfx_OpenGL_AttributeFormatInfo;
  [Gfx_OpenGL_getVertexAttributeFormatInfoImpl(f, info)]
  return info;
end

Gfx_OpenGL.AttributeFormat = Gfx_OpenGL_AttributeFormat;

Gfx_OpenGL.MAP_WRITE_ONLY = GL.GL_WRITE_ONLY;

local struct Gfx_OpenGL_DepthStencilStateDesc
{
  depthFunc       : GL.GLenum;
  enableDepthTest : bool;
  enableDepthWrite : bool;
}
Gfx_OpenGL.DepthStencilStateDesc = Gfx_OpenGL_DepthStencilStateDesc

terra Gfx_OpenGL_DepthStencilStateDesc:init()
  self.depthFunc = GL.GL_LESS;
  self.enableDepthTest = true;
  self.enableDepthWrite = true;
end

terra Gfx_OpenGL_Device:beginBuildDepthStencilState()
  var result : Gfx_OpenGL_DepthStencilStateDesc;
  result:init();
  return result;
end

terra Gfx_OpenGL_DepthStencilStateDesc:setDepthFunc( val : GL.GLenum )
  self.depthFunc = val;
  return self;
end

terra Gfx_OpenGL_DepthStencilStateDesc:setEnableDepthTest( val : bool )
  self.enableDepthTest = val;
  return self;
end

terra Gfx_OpenGL_DepthStencilStateDesc:setEnableDepthWrite( val : bool )
  self.enableDepthWrite = val;
  return self;
end

local struct Gfx_OpenGL_DepthStencilState
{
  desc : Gfx_OpenGL_DepthStencilStateDesc;
}
Gfx_OpenGL.DepthStencilState = Gfx_OpenGL_DepthStencilState

terra Gfx_OpenGL_DepthStencilState.methods.beginBuild() : Gfx_OpenGL_DepthStencilStateDesc
  var result : Gfx_OpenGL_DepthStencilStateDesc;
  result:init();
  return result;
end

terra Gfx_OpenGL_DepthStencilStateDesc:endBuild() : Gfx_OpenGL_DepthStencilState
  var result : Gfx_OpenGL_DepthStencilState;
  result.desc = @self;
  return result;
end



local struct Gfx_OpenGL_RasterizerStateDesc
{
  fillMode : GL.GLenum;
  enableScissor : bool;
  enableDepthClip : bool;
}
Gfx_OpenGL.RasterizerStateDesc = Gfx_OpenGL_RasterizerStateDesc;

local struct Gfx_OpenGL_RasterizerState
{
  desc : Gfx_OpenGL_RasterizerStateDesc;
}
Gfx_OpenGL.RasterizerState = Gfx_OpenGL_RasterizerState;


terra Gfx_OpenGL_RasterizerStateDesc:init()
  self.fillMode = GL.GL_FILL;
  self.enableScissor = false;
  self.enableDepthClip = true;
end

terra Gfx_OpenGL_Device:beginBuildRasterizerState()
  var result : Gfx_OpenGL_RasterizerStateDesc;
  result:init();
  return result;
end

terra Gfx_OpenGL_RasterizerStateDesc:setFillMode( val : GL.GLenum )
  self.fillMode = val;
  return self;
end

terra Gfx_OpenGL_RasterizerStateDesc:setEnableScissor( val : bool )
  self.enableScissor = val;
  return self;
end

terra Gfx_OpenGL_RasterizerStateDesc:setEnableDepthClip( val : bool )
  self.enableDepthClip = val
  return self;
end

terra Gfx_OpenGL_RasterizerState.methods.beginBuild() : Gfx_OpenGL_RasterizerStateDesc
  var result : Gfx_OpenGL_RasterizerStateDesc;
  result:init();
  return result;
end

terra Gfx_OpenGL_RasterizerStateDesc:endBuild() : Gfx_OpenGL_RasterizerState
  var result : Gfx_OpenGL_RasterizerState;
  result.desc = @self;
  return result;
end

--
local kFilters = {
  { "MinMagMipPoint",                           GL.GL_NEAREST, GL.GL_NEAREST_MIPMAP_NEAREST,  false, false },
  { "MinMagPoint_MipLinear",                    GL.GL_NEAREST, GL.GL_NEAREST_MIPMAP_LINEAR,   false, false },
  { "MinPoint_MagLinear_MipPoint",              GL.GL_LINEAR,  GL.GL_NEAREST_MIPMAP_NEAREST,  false, false },
  { "MinPoint_MagMipLinear",                    GL.GL_LINEAR,  GL.GL_NEAREST_MIPMAP_LINEAR,   false, false },
  { "MinLinear_MagMipPoint",                    GL.GL_NEAREST, GL.GL_LINEAR_MIPMAP_NEAREST,   false, false },
  { "MinLinear_MagPoint_MipLinear",             GL.GL_NEAREST, GL.GL_LINEAR_MIPMAP_LINEAR,    false, false },
  { "MinMagLinear_MipPoint",                    GL.GL_LINEAR,  GL.GL_LINEAR_MIPMAP_NEAREST,   false, false },
  { "MinMagMipLinear",                          GL.GL_LINEAR,  GL.GL_LINEAR_MIPMAP_LINEAR,    false, false },
  { "Anisotropic",                              GL.GL_LINEAR,  GL.GL_LINEAR_MIPMAP_LINEAR,    true,  false },

  { "Comparison_MinMagMipPoint",                GL.GL_NEAREST, GL.GL_NEAREST_MIPMAP_NEAREST,  false, false },
  { "Comparison_MinMagPoint_MipLinear",         GL.GL_NEAREST, GL.GL_NEAREST_MIPMAP_LINEAR,   false, false },
  { "Comparison_MinPoint_MagLinear_MipPoint",   GL.GL_LINEAR,  GL.GL_NEAREST_MIPMAP_NEAREST,  false, false },
  { "Comparison_MinPoint_MagMipLinear",         GL.GL_LINEAR,  GL.GL_NEAREST_MIPMAP_LINEAR,   false, false },
  { "Comparison_MinLinear_MagMipPoint",         GL.GL_NEAREST, GL.GL_LINEAR_MIPMAP_NEAREST,   false, false },
  { "Comparison_MinLinear_MagPoint_MipLinear",  GL.GL_NEAREST, GL.GL_LINEAR_MIPMAP_LINEAR,    false, false },
  { "Comparison_MinMagLinear_MipPoint",         GL.GL_LINEAR,  GL.GL_LINEAR_MIPMAP_NEAREST,   false, false },
  { "Comparison_MinMagMipLinear",               GL.GL_LINEAR,  GL.GL_LINEAR_MIPMAP_LINEAR,    false, false },
  { "Comparison_Anisotropic",                   GL.GL_LINEAR,  GL.GL_LINEAR_MIPMAP_LINEAR,    true,  false },

  -- TODO: Minimum/Maximum modes
}

local Gfx_OpenGL_Filter = {};
Gfx_OpenGL.Filter = Gfx_OpenGL_Filter;
for i,v in ipairs(kFilters) do
  local name = unpack(v);
  Gfx_OpenGL_Filter[name] = i;
end

local struct FilterInfo
{
  magFilter         : GL.GLenum;
  minFilter         : GL.GLenum;
  enableAniso       : bool;
  enableComparison  : bool;
}

local function getFilterInfoImpl( f, info )
    local body = terralib.newlist();
    for i,v in ipairs(kFilters) do
      local name, magFilter, minFilter, enableAniso, enableComparison = unpack(v);
      table.insert(body, quote
        if f == i then
          info.magFilter        = magFilter;
          info.minFilter        = minFilter;
          info.enableAniso      = enableAniso;
          info.enableComparison = enableComparison;
          return info;
        end
      end);
    end
    return body;
end

local terra getFilterInfo( f : int )
  var info : FilterInfo;
  [getFilterInfoImpl(f, info)]
  return info;
end


local Gfx_OpenGL_TextureAddressMode = {};
Gfx_OpenGL.TextureAddressMode = Gfx_OpenGL_TextureAddressMode;

Gfx_OpenGL_TextureAddressMode.Wrap         = GL.GL_REPEAT
Gfx_OpenGL_TextureAddressMode.Mirror       = GL.GL_MIRRORED_REPEAT
Gfx_OpenGL_TextureAddressMode.Clamp        = GL.GL_CLAMP_TO_EDGE
Gfx_OpenGL_TextureAddressMode.Mirror_Once  = GL.GL_MIRROR_CLAMP_TO_EDGE

--

local struct SamplerStateDesc
{
  filter : FilterInfo;
  maxAnisotropy : float;
  addressModeU : GL.GLenum;
  addressModeV : GL.GLenum;
  addressModeW : GL.GLenum;
  -- TODO: comparison func
}

terra SamplerStateDesc:init()
  self.filter.minFilter = GL.GL_LINEAR;
  self.filter.magFilter = GL.GL_LINEAR; -- TODO: figure out how to get this right in geenral. :(
  self.filter.enableAniso = false;
  self.filter.enableComparison = false;
  self.addressModeU = GL.GL_REPEAT;
  self.addressModeV = GL.GL_REPEAT;
  self.addressModeW = GL.GL_REPEAT;
end

terra SamplerStateDesc:setFilter( f : int )
  self.filter = getFilterInfo(f);
  return self;
end

terra SamplerStateDesc:setMaxAnisotropy( val : float )
  self.maxAnisotropy = val;
  return self;
end

terra SamplerStateDesc:setAddressModeU( m : GL.GLenum )
  self.addressModeU = m
  return self
end

terra SamplerStateDesc:setAddressModeV( m : GL.GLenum )
  self.addressModeV = m
  return self
end

terra SamplerStateDesc:setAddressModeW( m : GL.GLenum )
  self.addressModeW = m
  return self
end

terra SamplerStateDesc:endBuild()
  var samplerID : GL.GLuint;
  GL.glGenSamplers(1, &samplerID);

  GL.glSamplerParameteri(samplerID, GL.GL_TEXTURE_MAG_FILTER, self.filter.magFilter);  
  GL.glSamplerParameteri(samplerID, GL.GL_TEXTURE_MIN_FILTER, self.filter.minFilter);  

  GL.glSamplerParameteri(samplerID, GL.GL_TEXTURE_WRAP_S, self.addressModeU);
  GL.glSamplerParameteri(samplerID, GL.GL_TEXTURE_WRAP_T, self.addressModeV);
  GL.glSamplerParameteri(samplerID, GL.GL_TEXTURE_WRAP_R, self.addressModeW);

  --[[ TODO: glcorearb.h doesn't include support for anisotropic filtering!
  if(self.filter.enableAniso) then
    GL.glSamplerParameterf(samplerID, GL.GL_TEXTURE_MAX_ANISOTROPY, self.maxAnisotropy);
  end
  --]]

  return samplerID;
end

terra Gfx_OpenGL_Device:beginBuildSamplerState()
  var desc : SamplerStateDesc;
  desc:init();
  return desc;
end

--

Gfx_OpenGL.BlendFactor = {}
Gfx_OpenGL.BlendFactor.One = GL.GL_ONE;
Gfx_OpenGL.BlendFactor.Zero = GL.GL_ZERO;
Gfx_OpenGL.BlendFactor.SrcAlpha = GL.GL_SRC_ALPHA;
Gfx_OpenGL.BlendFactor.InvSrcAlpha = GL.GL_ONE_MINUS_SRC_ALPHA;

--

local struct BlendStateImpl
{
  enableBlend : bool;
  srcFactor : GL.GLenum;
  dstFactor : GL.GLenum;
}

local struct BlendStateBuilder
{
  impl : BlendStateImpl;
}

terra BlendStateBuilder:init()
  self.impl.enableBlend = false;
  self.impl.srcFactor = GL.GL_ONE;
  self.impl.dstFactor = GL.GL_ZERO;
end

terra Gfx_OpenGL_Device:beginBuildBlendState()
  var builder : BlendStateBuilder;
  builder:init();
  return builder;
end

terra BlendStateBuilder:setEnableBlend( val : bool )
  self.impl.enableBlend = val;
  return self;
end

terra BlendStateBuilder:setBlendFunc( src : GL.GLenum, dst : GL.GLenum )
  self.impl.srcFactor = src;
  self.impl.dstFactor = dst;
  return self;
end

terra BlendStateBuilder:endBuild()
  var result = [&BlendStateImpl](C.malloc([terralib.sizeof(BlendStateImpl)]));
  @result = self.impl;

  return result;
end

terra Gfx_OpenGL_Device:deleteBlendState( b : &BlendStateImpl )
  C.free(b);
end

Gfx_OpenGL.BlendState = &BlendStateImpl;

--

local kTextureFormats = {
  { "R8",     GL.GL_R8,     GL.GL_RED,    GL.GL_UNSIGNED_BYTE,  "r8ui"},
  { "RGB8",   GL.GL_RGB8,   GL.GL_RGB,    GL.GL_UNSIGNED_BYTE,  "rgb8ui"},
  { "RGBA8",  GL.GL_RGBA8,  GL.GL_RGBA,   GL.GL_UNSIGNED_BYTE,  "rgba8ui"},
  { "R16F",   GL.GL_R16F,   GL.GL_RED,    GL.GL_HALF_FLOAT,     "r16f"},
  { "RGBA16", GL.GL_RGBA16, GL.GL_RGBA,   GL.GL_UNSIGNED_SHORT, "rgba16ui"},
  { "RGBA16U", GL.GL_RGBA16, GL.GL_RGBA,   GL.GL_UNSIGNED_SHORT, "rgba16ui"},
  { "RGBA16F", GL.GL_RGBA16F, GL.GL_RGBA, GL.GL_HALF_FLOAT,     "rgba16f"},
  { "RGBA32F", GL.GL_RGBA32F, GL.GL_RGBA, GL.GL_FLOAT,          "rgba32f"},
  {"DEPTH24_STENCIL8", GL.GL_DEPTH24_STENCIL8, GL.GL_DEPTH_STENCIL, GL.GL_UNSIGNED_INT_24_8, ""},

  {"BC1_UNORM", GL.GL_COMPRESSED_RGBA_S3TC_DXT1_EXT, GL.GL_RGBA, GL.GL_UNSIGNED_BYTE,  ""},
  {"BC2_UNORM", GL.GL_COMPRESSED_RGBA_S3TC_DXT3_EXT, GL.GL_RGBA, GL.GL_UNSIGNED_BYTE,  ""},
  {"BC3_UNORM", GL.GL_COMPRESSED_RGBA_S3TC_DXT5_EXT, GL.GL_RGBA, GL.GL_UNSIGNED_BYTE,  ""},

  {"BC5_UNORM", GL.GL_COMPRESSED_RG_RGTC2,        GL.GL_RG, GL.GL_UNSIGNED_BYTE,  ""},
  {"BC5_SNORM", GL.GL_COMPRESSED_SIGNED_RG_RGTC2, GL.GL_RG, GL.GL_BYTE,           ""},

  -- sRGB
  {"BC1_UNORM_SRGB", GL.GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT, GL.GL_RGBA, GL.GL_UNSIGNED_BYTE, ""},
  {"BC2_UNORM_SRGB", GL.GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT, GL.GL_RGBA, GL.GL_UNSIGNED_BYTE, ""},
  {"BC3_UNORM_SRGB", GL.GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT, GL.GL_RGBA, GL.GL_UNSIGNED_BYTE, ""},
  {"RGBA8_SRGB",     GL.GL_SRGB8_ALPHA8,                        GL.GL_RGBA, GL.GL_UNSIGNED_BYTE, ""},

  {"UNKNOWN",   GL.GL_RGBA8,  GL.GL_RGBA,    GL.GL_UNSIGNED_BYTE,  ""},
}
local Gfx_OpenGL_TextureFormat = {}
Gfx_OpenGL.TextureFormat = Gfx_OpenGL_TextureFormat;
Gfx_OpenGL.TextureFormatT = int;

local Gfx_OpenGL_ShaderTextureFormat = {}
Gfx_OpenGL.ShaderTextureFormat = Gfx_OpenGL_ShaderTextureFormat

for i,v in ipairs(kTextureFormats) do
  local name, internalFormat, componentShape, componentType, shaderImageType = unpack(v)
  Gfx_OpenGL_TextureFormat[name] = i;
  Gfx_OpenGL_ShaderTextureFormat[i] = shaderImageType
end

local struct TextureFormatInfo
{
  internalFormat  : GL.GLenum;
  componentShape  : GL.GLenum;
  componentType   : GL.GLenum;
}

local function getTextureFormatInfoImpl( f, info )
    local body = terralib.newlist();
    for i,v in ipairs(kTextureFormats) do
      local name, internalFormat, componentShape, componentType = unpack(v);
      table.insert(body, quote
        if f == i then
          info.internalFormat   = internalFormat;
          info.componentShape   = componentShape;
          info.componentType    = componentType;
          return info;
        end
      end);
    end
    return body;
end

local terra getTextureFormatInfo( f : int )
  var info : TextureFormatInfo;
  [getTextureFormatInfoImpl(f, info)]
  return info;
end

local terra isCompressedFormat( f : GL.GLenum )
  if   f == GL.GL_COMPRESSED_RGBA_S3TC_DXT1_EXT
    or f == GL.GL_COMPRESSED_RGBA_S3TC_DXT3_EXT
    or f == GL.GL_COMPRESSED_RGBA_S3TC_DXT5_EXT
    or f == GL.GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT
    or f == GL.GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT
    or f == GL.GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT
    or f == GL.GL_COMPRESSED_RG_RGTC2
    or f == GL.GL_COMPRESSED_SIGNED_RG_RGTC2
  then
    return true
  else
    return false
  end
end


local Gfx_OpenGL_BindFlag = {};
Gfx_OpenGL.BindFlag = Gfx_OpenGL_BindFlag;

Gfx_OpenGL_BindFlag.SHADER_RESOURCE = 0;
Gfx_OpenGL_BindFlag.DEPTH_STENCIL   = 0;
Gfx_OpenGL_BindFlag.UNORDERED_ACCESS = 0;
Gfx_OpenGL_BindFlag.RENDER_TARGET = 0;

local MAX_NUM_MIP_LEVELS = 16
local struct Texture2DDesc
{
  format            : TextureFormatInfo;
  extentX           : int;
  extentY           : int;
  mipLevelCount     : int;
  initDataForLevels : (&uint8)[MAX_NUM_MIP_LEVELS];
}

-- Texture2D
terra Texture2DDesc:init()
  self.format.internalFormat  = GL.GL_RGBA8;
  self.format.componentShape  = GL.GL_RGBA;
  self.format.componentType   = GL.GL_UNSIGNED_BYTE;
  self.extentX                = 32;
  self.extentY                = 32;
  self.mipLevelCount          = 0;

  for i = 0, MAX_NUM_MIP_LEVELS do
    self.initDataForLevels[i] = nil
  end
end

terra Texture2DDesc:setFormat( f : int )
  self.format = getTextureFormatInfo(f);
  return self;
end

terra Texture2DDesc:setMipLevelCount( val : int )
  self.mipLevelCount = val;
  return self;
end

terra Texture2DDesc:setInitDataForLevel( level : int, val : &uint8, pitch : int )
  self.initDataForLevels[level] = val;
  return self;
end

terra Texture2DDesc:setBindFlags( b : uint )
  -- Do nothing for OpenGL
  return self;
end

terra Texture2DDesc:setUsage( u : GL.GLenum )
  -- Do nothing for OpenGL
  return self;
end

terra Texture2DDesc:setCPUAccess( a : int )
  -- Do nothing for OpenGL
  return self;
end

terra Texture2DDesc:enableMipGeneration()
  -- Do nothing for OpenGL
  return self
end


local terra nextMipExtent( val : int ) : int
  var v = val / 2;
  if v <= 0 then v = 1 end
  return v;
end

terra Texture2DDesc:endBuild()
  -- figure out the level count, if needed
  var levelCount = self.mipLevelCount;
  if( levelCount == 0 )then
    var x = self.extentX;
    var y = self.extentY;
    levelCount = 1;
    while (x > 1) or (y >1) do
      x = nextMipExtent(x);
      y = nextMipExtent(y);
      levelCount = levelCount + 1;
    end
  end
  self.mipLevelCount = levelCount;

  var textureID : GL.GLuint;
  GL.glCreateTextures(GL.GL_TEXTURE_2D, 1, &textureID);
  GL.glTextureStorage2D(
    textureID,
    self.mipLevelCount,
    self.format.internalFormat,
    self.extentX,
    self.extentY);

  var levelX = self.extentX;
  var levelY = self.extentY;

  for level = 0,self.mipLevelCount do
    if self.initDataForLevels[level] ~= nil then
      if isCompressedFormat(self.format.internalFormat) then
        var blockSize = 16
        if   self.format.internalFormat == GL.GL_COMPRESSED_RGBA_S3TC_DXT1_EXT
          or self.format.internalFormat == GL.GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT
        then
          blockSize = 8
        end

        var imageSize = (max(1, ( (levelX + 3) / 4 ) ) * max(1, ( (levelY + 3) / 4 ) ) * blockSize)

        GL.glCompressedTextureSubImage2D(
          textureID,
          level,
          0,
          0,
          levelX,
          levelY,
          self.format.internalFormat,
          imageSize,
          self.initDataForLevels[level]);
      else
        GL.glTextureSubImage2D(
          textureID,
          level,
          0,
          0,
          levelX,
          levelY,
          self.format.componentShape,
          self.format.componentType,
          self.initDataForLevels[level]);
      end
    end

    levelX = levelX / 2;
    levelY = levelY / 2;
    if levelX == 0 then levelX = 1 end
    if levelY == 0 then levelY = 1 end
  end
--  GL.glGenerateTextureMipmap(textureID);

  -- TODO: shouldn't actually need these!
  -- GL.glTextureParameteri(textureID, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR_MIPMAP_LINEAR);
  -- GL.glTextureParameteri(textureID, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);

  GL.glTextureParameteri(textureID, GL.GL_TEXTURE_MAX_LEVEL, self.mipLevelCount);

  return textureID;
end

terra Gfx_OpenGL_Device:beginBuildTexture2D( extentX : int, extentY : int )
  var builder : Texture2DDesc;
  builder:init();
  builder.extentX = extentX;
  builder.extentY = extentY;
  return builder;
end


-- Texture Cube Map
local struct TextureCubeMapDesc
{
  format            : TextureFormatInfo;
  extentX           : int;
  extentY           : int;
  mipLevelCount     : int;
  initDataForLevels : (&uint8)[6][MAX_NUM_MIP_LEVELS];
}

terra TextureCubeMapDesc:init()
  self.format.internalFormat  = GL.GL_RGBA8;
  self.format.componentShape  = GL.GL_RGBA;
  self.format.componentType   = GL.GL_UNSIGNED_BYTE;
  self.extentX                = 32;
  self.extentY                = 32;
  self.mipLevelCount          = 0;

  for i = 0, 6 do
    for j = 0, MAX_NUM_MIP_LEVELS do
      self.initDataForLevels[i][j] = nil
    end
  end
end

terra TextureCubeMapDesc:setFormat( f : int )
  self.format = getTextureFormatInfo(f);
  return self;
end

terra TextureCubeMapDesc:setMipLevelCount( val : int )
  self.mipLevelCount = val;
  return self;
end

terra TextureCubeMapDesc:setInitDataForLevel( face : int, level : int, val : &uint8, pitch : int )
  self.initDataForLevels[face][level] = val;
  return self;
end

terra TextureCubeMapDesc:setBindFlags( b : uint )
  -- Do nothing for OpenGL
  return self;
end

terra TextureCubeMapDesc:setUsage( u : GL.GLenum )
  -- Do nothing for OpenGL
  return self;
end

terra TextureCubeMapDesc:setCPUAccess( a : int )
  -- Do nothing for OpenGL
  return self;
end

terra TextureCubeMapDesc:endBuild()
  var textureID : GL.GLuint;
  GL.glCreateTextures(GL.GL_TEXTURE_CUBE_MAP, 1, &textureID);
  GL.glTextureStorage2D(
    textureID,
    self.mipLevelCount,
    self.format.internalFormat,
    self.extentX,
    self.extentY);

  -- TODO load initial data
  var hasInitData : bool = false;
  for i = 0, 6 do
    for j = 0, MAX_NUM_MIP_LEVELS do
      if self.initDataForLevels[i][j] ~= nil then
        hasInitData = true;
      end
    end
  end

  if hasInitData then
    C.printf("Warning: Initialization data for Cube Maps is not currently supported\n")
  end

  -- TODO: shouldn't actually need these!
  GL.glTextureParameteri(textureID, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);
  GL.glTextureParameteri(textureID, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);

  GL.glTextureParameteri(textureID, GL.GL_TEXTURE_MAX_LEVEL, self.mipLevelCount);


  return textureID;
end

terra Gfx_OpenGL_Device:beginBuildTextureCubeMap( extentX : int, extentY : int )
  var builder : TextureCubeMapDesc;
  builder:init();
  builder.extentX = extentX;
  builder.extentY = extentY;
  return builder;
end


terra Gfx_OpenGL_Device:deleteTexture( t : GL.GLuint )
  GL.glDeleteTextures(1, &t);
end

terra Gfx_OpenGL_Device:updateTextureData( t : GL.GLuint, f: int, width : int, height : int,
                                           level : int, data : &uint16, numComponents : int)
  var format = getTextureFormatInfo(f)
  GL.glTextureSubImage2D(
    t,
    level,
    0,
    0,
    width,
    height,
    format.componentShape,
    format.componentType,
    data)
end

terra Gfx_OpenGL_Context:generateMipmap(textureID : GL.GLuint)
  GL.glGenerateTextureMipmap(textureID);
end


--

Gfx_OpenGL.ShaderProgram = GL.GLuint;
Gfx_OpenGL.ComputeShaderProgram = GL.GLuint;
Gfx_OpenGL.VertexArray = GL.GLuint;
Gfx_OpenGL.Buffer = GL.GLuint;
Gfx_OpenGL.SamplerState = GL.GLuint;
Gfx_OpenGL.Texture = GL.GLuint;


terra Gfx_OpenGL.getWindowFlags() : int
  SDL.SDL_GL_SetAttribute(SDL.SDL_GL_CONTEXT_MAJOR_VERSION, 3)
  SDL.SDL_GL_SetAttribute(SDL.SDL_GL_CONTEXT_MINOR_VERSION, 1)

  return SDL.SDL_WINDOW_OPENGL;
end

terra Gfx_OpenGL_Device:init( window : &SDL.SDL_Window )
  self.window = window;

  var context = SDL.SDL_GL_CreateContext(window)
  if context == nil then
    C.printf("Error creating OpenGL context: %s\n", SDL.SDL_GetError())
    SDL.SDL_DestroyWindow(window)
    return
  end

  if SDL.SDL_GL_SetSwapInterval(0) < 0 then
    C.printf("Error setting Swap Interval: %s\n", SDL.SDL_GetError())
    SDL.SDL_DestroyWindow(window)
    return
  end

  if not GL.loadFunctions() then
    C.printf("Unable to load GL functions.\n")
    SDL.SDL_DestroyWindow(window)
    return
  end

  GL.glClipControl(GL.GL_UPPER_LEFT, GL.GL_ZERO_TO_ONE)
  GL.glEnable(GL.GL_FRAMEBUFFER_SRGB)
end


local terra printShaderLog(shader: GL.GLuint)
  var logLen = 0
  var maxLen = 0

  GL.glGetShaderiv(shader, GL.GL_INFO_LOG_LENGTH, &maxLen)

  var log = [rawstring](C.malloc(sizeof(int8) * maxLen))

  GL.glGetShaderInfoLog(shader, maxLen, &logLen, log)
  if logLen > 0 then
    C.printf("%s\n", log)
  end

  C.free(log)
end

local terra printProgramLog(program: GL.GLuint)
  var logLen = 0
  var maxLen = 0

  GL.glGetProgramiv(program, GL.GL_INFO_LOG_LENGTH, &maxLen)
  var log = [rawstring](C.malloc(sizeof(int8) * maxLen))

  GL.glGetProgramInfoLog(program, maxLen, &logLen, log)
  if logLen > 0 then
    C.printf("%s\n", log)
  end

  C.free(log)
end

local terra loadShaderFromString( kind : GL.GLenum, source : &int8 ) : GL.GLuint
    var shader = GL.glCreateShader(kind);
    GL.glShaderSource(shader, 1, &source, nil);
    GL.glCompileShader(shader);

    var success = GL.GL_FALSE;
    GL.glGetShaderiv(shader, GL.GL_COMPILE_STATUS, &success)
    if success ~= GL.GL_TRUE then
        C.printf("Error compiling GLSL shader '%d'.\n", shader)
        printShaderLog(shader)
        return 0
    end

    return shader
end

terra Gfx_OpenGL_Device:createShaderProgram(vshader : rawstring, fshader : rawstring) : Gfx_OpenGL.ShaderProgram
  var program = GL.glCreateProgram()

  var vs = loadShaderFromString(GL.GL_VERTEX_SHADER, vshader)
  var fs = loadShaderFromString(GL.GL_FRAGMENT_SHADER, fshader)

  GL.glAttachShader(program, vs)
  GL.glAttachShader(program, fs)
  GL.glLinkProgram(program)
  GL.glDeleteShader(vs)
  GL.glDeleteShader(fs)

  var success = GL.GL_FALSE
  GL.glGetProgramiv(program, GL.GL_LINK_STATUS, &success)
  if success ~= GL.GL_TRUE then

    C.printf("=Vert=\n%s\n", vshader);
    C.printf("=Frag=\n%s\n", fshader);

    C.printf("Error linking program '%d'.\n", program)
    printProgramLog(program)
    return 0
  end

  return program
end

terra Gfx_OpenGL_Device:createComputeProgram(shader : rawstring) : Gfx_OpenGL.ComputeShaderProgram
  var program = GL.glCreateProgram()

  var cs = loadShaderFromString(GL.GL_COMPUTE_SHADER, shader)

  GL.glAttachShader(program, cs)
  GL.glLinkProgram(program)
  GL.glDeleteShader(cs)

  var success = GL.GL_FALSE
  GL.glGetProgramiv(program, GL.GL_LINK_STATUS, &success)
  if success ~= GL.GL_TRUE then

    C.printf("=Compute=\n%s\n", shader);

    C.printf("Error linking program '%d'.\n", program)
    printProgramLog(program)
    return 0
  end

  return program
end

-- Also works for Gfx_OpenGL.ComputeShaderProgram because it is also technically GLuint
terra Gfx_OpenGL_Device:deleteShaderProgram( p : Gfx_OpenGL.ShaderProgram )
  if p ~= 0 then
    GL.glDeleteProgram(p);
   end
end

terra Gfx_OpenGL_Device:getImmediateContext() : &Gfx_OpenGL_Context
  return &self.defaultContext;
end


local struct Gfx_OpenGL_BufferBuilder
{
--  device  : &Gfx_OpenGL_Device;
  target  : GL.GLenum;
  size    : GL.GLsizei;
  data    : &GL.GLvoid;
  usage   : GL.GLenum;
}

terra Gfx_OpenGL_BufferBuilder:init( device : &Gfx_OpenGL_Device )
--  self.device = device;
  self.target = GL.GL_UNIFORM_BUFFER;
  self.size   = 0;
  self.data   = nil;
  self.usage  = Gfx_OpenGL_Usage.Default;
end

terra Gfx_OpenGL_Device:beginBuildBuffer() : Gfx_OpenGL_BufferBuilder
  var builder : Gfx_OpenGL_BufferBuilder;
  builder:init(self);
  return builder;
end

terra Gfx_OpenGL_BufferBuilder:setSize( size : GL.GLsizei )
  self.size = size;
  return self;
end

terra Gfx_OpenGL_BufferBuilder:setInitData( data : &GL.GLvoid )
  self.data = data;
  return self;
end

terra Gfx_OpenGL_BufferBuilder:setUsage( usage : GL.GLenum )
  self.usage = usage;
  return self;
end

terra Gfx_OpenGL_BufferBuilder:setCPUAccess( ignored : int )
  return self
end

terra Gfx_OpenGL_BufferBuilder:setBindVertexBuffer()
  self.target = GL.GL_ARRAY_BUFFER;
  return self;
end

terra Gfx_OpenGL_BufferBuilder:setBindIndexBuffer()
  self.target = GL.GL_ELEMENT_ARRAY_BUFFER;
  return self;
end

terra Gfx_OpenGL_BufferBuilder:setBindUniformBuffer()
  self.target = GL.GL_UNIFORM_BUFFER;
  return self;
end

terra Gfx_OpenGL_BufferBuilder:setBindRWStructuredBuffer()
  self.target = GL.GL_SHADER_STORAGE_BUFFER;
  return self;
end

terra Gfx_OpenGL_BufferBuilder:endBuild() : GL.GLuint
  var bufferID : GL.GLuint;

  -- TODO: use DSA if available (e.g., `glCreateBuffers`)?
  GL.glGenBuffers(1, &bufferID);
  GL.glBindBuffer(self.target, bufferID);
  GL.glBufferData(self.target, self.size, self.data, self.usage);

  return bufferID;
end

terra Gfx_OpenGL_Device:deleteBuffer( bufferID : GL.GLuint )
  GL.glDeleteBuffers(1, &bufferID);
end

--


terra Gfx_OpenGL_VertexLayoutDesc:init( device : &Gfx_OpenGL_Device )
  self.attributeCount   = 0;
end

terra Gfx_OpenGL_Device:beginBuildVertexLayout() : Gfx_OpenGL_VertexLayoutDesc
  var builder : Gfx_OpenGL_VertexLayoutDesc;
  builder:init(self);
  return builder;
end

terra Gfx_OpenGL_VertexLayoutDesc:addAttribute( format : int, streamIndex : int, offset : GL.GLsizei )

  var index = self.attributeCount;
  self.attributeCount = self.attributeCount + 1;

  var f = Gfx_OpenGL_getVertexAttributeFormatInfo(format);

  self.attributes[index].streamIndex = streamIndex;
  self.attributes[index].format = f;
  self.attributes[index].offset = offset;

  return self;
end

terra Gfx_OpenGL_VertexLayoutDesc:endBuild() : Gfx_OpenGL_VertexLayout
  var result : Gfx_OpenGL_VertexLayout;
  result = [Gfx_OpenGL_VertexLayout](C.malloc(sizeof(Gfx_OpenGL_VertexLayoutImpl)));
  result.desc = @self;
  return result;
end

--

terra Gfx_OpenGL_Context:clearFramebuffer( c : vec4, d : float, s : uint32, flags : uint )

  -- TODO: this is a hack!!!
  GL.glDisable(GL.GL_SCISSOR_TEST);
  GL.glDepthMask(GL.GL_TRUE);

  GL.glClearColor(c.x, c.y, c.z, c.w)
  GL.glClearDepth(d)
  GL.glClearStencil(s);
  GL.glClear(flags)
end

terra Gfx_OpenGL_Context:setViewport( x0 : int, y0 : int, xExtent : int, yExtent : int )
  GL.glViewport(x0, y0, xExtent, yExtent)
end

terra Gfx_OpenGL_Context:setDepthStencilState( s : Gfx_OpenGL_DepthStencilState )
  GL.glDepthFunc(s.desc.depthFunc);
  terralib.select(s.desc.enableDepthTest, GL.glEnable, GL.glDisable)(GL.GL_DEPTH_TEST);
  GL.glDepthMask(terralib.select(s.desc.enableDepthWrite, GL.GL_TRUE, GL.GL_FALSE));
end

terra Gfx_OpenGL_Context:setRasterizerState( s : Gfx_OpenGL_RasterizerState )
  GL.glPolygonMode(
    GL.GL_FRONT_AND_BACK,
    s.desc.fillMode);
  terralib.select(s.desc.enableScissor, GL.glEnable, GL.glDisable)(GL.GL_SCISSOR_TEST);  
  terralib.select(s.desc.enableDepthClip, GL.glDisable, GL.glEnable)(GL.GL_DEPTH_CLAMP);
end

terra Gfx_OpenGL_Context:setBlendState( s : &BlendStateImpl )
  if s.enableBlend then
    GL.glEnable(GL.GL_BLEND);
  else
    GL.glDisable(GL.GL_BLEND);
  end
--  terralib.select(s.enableBlend, GL.glEnable, GL.glDisable)(GL.GL_BLEND);
  GL.glBlendFunc(s.srcFactor, s.dstFactor);
end

terra Gfx_OpenGL_Context:setShaderProgram( programID : GL.GLuint )
  GL.glUseProgram(programID)
end

terra Gfx_OpenGL_Context:setVertexInput( vaoID : GL.GLuint )
  GL.glBindVertexArray(vaoID);
end

terra Gfx_OpenGL_Context:setVertexLayout( layout : Gfx_OpenGL_VertexLayout )
  self.vertexLayout = layout;
  self.dirtyVertexAttributes = 0xFFFFFFFF;
end

terra Gfx_OpenGL_Context:setVertexBuffer( index : int, bufferID : GL.GLuint, stride : int, offset : int )
  self.vertexStreamBuffers[index] = bufferID;
  self.vertexStreamStrides[index] = stride;
  self.vertexStreamOffsets[index] = offset;
  self.dirtyVertexAttributes = self.dirtyVertexAttributes or 0x1;
end

terra Gfx_OpenGL_Context:setIndexBuffer( bufferID : GL.GLuint, format : GL.GLenum )
  GL.glBindBuffer(GL.GL_ELEMENT_ARRAY_BUFFER, bufferID);
  self.indexFormat = format;
end

terra Gfx_OpenGL_Context:setTextureSampler( index : int, type : int, textureID : GL.GLuint, samplerID : GL.GLuint )
  var texType = GL.GL_TEXTURE_2D
  if type == 0 then
    texType = GL.GL_TEXTURE_2D
  elseif type == 1 then
    texType = GL.GL_TEXTURE_CUBE_MAP
  end

  GL.glActiveTexture(GL.GL_TEXTURE0 + index);
  GL.glBindTexture(texType, textureID);
  GL.glBindSampler(index, samplerID);
end

terra Gfx_OpenGL_Context:setTextureImage( index : int, textureID : GL.GLuint, access : Gfx_OpenGL.MapModeT, format : int)
  GL.glBindImageTexture(index, textureID, 0, GL.GL_FALSE, 0, toGLMapMode(access), getTextureFormatInfo(format).internalFormat)
end

terra Gfx_OpenGL_Context:setUniformBuffer( index : int, bufferID : GL.GLuint )
  GL.glBindBufferBase(GL.GL_UNIFORM_BUFFER, index, bufferID);
end

terra Gfx_OpenGL_Context:setScissorRect( x0 : int, y0 : int, x1 : int, y1 : int, w : int, h : int)
  -- Need to flip the Y axis here.
  var t0 = h - y1;
  var t1 = h - y0;

  GL.glScissor(x0, t0, x1 - x0, t1 - t0);
end

local terra Gfx_openGL_Context_flushStateForDraw( self : &Gfx_OpenGL_Context )
  if self.dirtyVertexAttributes ~= 0 then
    var mask : uint32 = 1;
    var layout = self.vertexLayout;
    for i = 0,layout.desc.attributeCount do
      if (self.dirtyVertexAttributes and mask) ~= 0 then
        var attr = &layout.desc.attributes[i];
        -- do the thing!!!
        GL.glBindBuffer(GL.GL_ARRAY_BUFFER, self.vertexStreamBuffers[attr.streamIndex]);

        GL.glVertexAttribPointer(
          i,
          attr.format.componentCount,
          attr.format.componentType,
          attr.format.normalized,
          self.vertexStreamStrides[attr.streamIndex],
          [&GL.GLvoid](attr.offset + self.vertexStreamOffsets[attr.streamIndex]));

        GL.glEnableVertexAttribArray(i);

      end
      mask = mask << 1;
    end
    self.dirtyVertexAttributes = 0;
  end
end

Gfx_OpenGL_Context.methods.drawIndexed =
  terralib.overloadedfunction("Gfx_OpenGL_Context:drawIndexed")

Gfx_OpenGL_Context.methods.drawIndexed:adddefinition(
  terra(self : &Gfx_OpenGL_Context, primitiveType : GL.GLenum, vertCount : int )

  Gfx_openGL_Context_flushStateForDraw( self );

  -- TODO: need to be able to pass index format from outside...
  GL.glDrawElements(primitiveType, vertCount, self.indexFormat, nil)
end)

Gfx_OpenGL_Context.methods.drawIndexed:adddefinition(
  terra(self : &Gfx_OpenGL_Context, primitiveType : GL.GLenum, vertCount : int, baseIndex : int, baseVertex : int )

  Gfx_openGL_Context_flushStateForDraw( self );

  var indexOffset = 0;
  if( self.indexFormat == GL.GL_UNSIGNED_SHORT ) then
    indexOffset = 2 * baseIndex;
  elseif( self.indexFormat == GL.GL_UNSIGNED_INT ) then
    indexOffset = 4 * baseIndex;
  elseif( self.indexFormat == GL.GL_UNSIGNED_BYTE ) then
    indexOffset = baseIndex;
  else
  end

  -- TODO: need to be able to pass index format from outside...
  GL.glDrawElementsBaseVertex(primitiveType, vertCount, self.indexFormat, [&int8](nil) + indexOffset, baseVertex)
end)


-- Compute shader dispatch
terra Gfx_OpenGL_Context:dispatchCompute(x : int, y : int, z : int)
  GL.glDispatchCompute(x,y,z)
end

local Gfx_OpenGL_BarrierFlags = {};
Gfx_OpenGL.BarrierFlags = Gfx_OpenGL_BarrierFlags;

Gfx_OpenGL_BarrierFlags.TEXTURE_FETCH = GL.GL_TEXTURE_FETCH_BARRIER_BIT;
Gfx_OpenGL_BarrierFlags.SHADER_IMAGE_ACCESS = GL.GL_SHADER_IMAGE_ACCESS_BARRIER_BIT;
Gfx_OpenGL_BarrierFlags.SHADER_BUFFER = GL.GL_SHADER_STORAGE_BARRIER_BIT;

terra Gfx_OpenGL_Context:memoryBarrier(barrier : GL.GLuint)
  GL.glMemoryBarrier(barrier)
end

-- Debugging utilities
terra Gfx_OpenGL_Device:setDebug(b: bool)
  terralib.select(b, GL.glEnable, GL.glDisable)(GL.GL_DEBUG_OUTPUT);
end

terra Gfx_OpenGL_Device:printDebugMessages(numMsgs: GL.GLuint)
  var maxMsgLen: GL.GLint = 0
  GL.glGetIntegerv(GL.GL_MAX_DEBUG_MESSAGE_LENGTH, &maxMsgLen)

  var msgData     : Array(GL.GLchar); msgData:init(numMsgs * maxMsgLen);
  var sources     : Array(GL.GLenum); sources:init(numMsgs);
  var types       : Array(GL.GLenum); types:init(numMsgs);
  var severities  : Array(GL.GLenum); severities:init(numMsgs);
  var ids         : Array(GL.GLuint); ids:init(numMsgs);
  var lengths     : Array(GL.GLsizei); lengths:init(numMsgs);

  var numFound: GL.GLuint = GL.glGetDebugMessageLog(numMsgs, msgData.capacity, sources:getraw(),
    types:getraw(), ids:getraw(), severities:getraw(), lengths:getraw(), msgData:getraw());

  sources.count     = numFound;
  types.count       = numFound;
  severities.count  = numFound;
  ids.count         = numFound;
  lengths.count     = numFound;

  var msgPtr = msgData:getraw()
  for i = 0, lengths.count do
    var len = lengths:get(i)
    var m = [rawstring](C.malloc(len * sizeof(int8)))
    C.strcpy(m, msgPtr);
    msgPtr = msgPtr + len;
    C.printf("%s\n", m);
    C.free(m)
  end
  C.printf("\n\n");
end

terra Gfx_OpenGL_Context:mapBuffer( bufferID : GL.GLuint, mode : Gfx_OpenGL.MapModeT,
                                    size : int, usage : GL.GLenum) : &int8
  GL.glBindBuffer(GL.GL_UNIFORM_BUFFER, bufferID);

  if mode == Gfx_OpenGL_MapMode.WriteDiscard then
    GL.glBufferData(GL.GL_UNIFORM_BUFFER, size, nil, usage)
  end

  var ptr = GL.glMapBuffer(GL.GL_UNIFORM_BUFFER, toGLMapMode(mode));
  return [&int8](ptr);
end

terra Gfx_OpenGL_Context:unmapBuffer( bufferID : GL.GLuint )
  GL.glBindBuffer(GL.GL_UNIFORM_BUFFER, bufferID);
  var ptr = GL.glUnmapBuffer(GL.GL_UNIFORM_BUFFER);
end

terra Gfx_OpenGL_Device:copyTextureToHost( texture : Gfx_OpenGL.Texture, level : int,
                                           format : Gfx_OpenGL.TextureFormatT,
                                           width : int, height : int, pixels : &vec4)
  var fmt = getTextureFormatInfo(format)
  var size = width * height * terralib.sizeof(vec4)
  GL.glGetTextureImage(texture, level, fmt.componentShape, fmt.componentType, size, pixels)
end

-- RenderTargetView and DepthStencilView

struct Gfx_OpenGL_RenderTargetView {}
struct Gfx_OpenGL_DepthStencilView {}

Gfx_OpenGL.RenderTargetView = &Gfx_OpenGL_RenderTargetView;
Gfx_OpenGL.DepthStencilView = &Gfx_OpenGL_DepthStencilView;

terra Gfx_OpenGL_Device:createRenderTargetView( id : GL.GLuint )
  return [&Gfx_OpenGL_RenderTargetView]([uint64](id) + 1);
end

Gfx_OpenGL_Device.methods.createDepthStencilView = terralib.overloadedfunction("createDepthStencilView",
{
  terra(self : &Gfx_OpenGL_Device, id : GL.GLuint)
    return [&Gfx_OpenGL_DepthStencilView]([uint64](id) + 1);
  end
,
  terra(self : &Gfx_OpenGL_Device, id : GL.GLuint, face : int)
    return [&Gfx_OpenGL_DepthStencilView]([uint64](id) + 1);
  end
})


local Gfx_OpenGL_getTextureID = terralib.overloadedfunction("Gfx_OpenGL_getTextureID")
Gfx_OpenGL_getTextureID:adddefinition(terra( v : &Gfx_OpenGL_RenderTargetView )
  return [GL.GLuint]([uint64](v) - 1);
end)

Gfx_OpenGL_getTextureID:adddefinition(terra( v : &Gfx_OpenGL_DepthStencilView )
  return [GL.GLuint]([uint64](v) - 1);
end)

terra Gfx_OpenGL_Device:bindRenderTargetView( v : &Gfx_OpenGL_RenderTargetView, bindPoint : uint )
  var id = Gfx_OpenGL_getTextureID(v)
  GL.glFramebufferTexture(GL.GL_FRAMEBUFFER, GL.GL_COLOR_ATTACHMENT0 + bindPoint, id, 0)

  var bufs : Array(GL.GLenum)
  bufs:init()
  for i=0,bindPoint+1 do
    bufs:add(GL.GL_COLOR_ATTACHMENT0 + i)
  end
  GL.glDrawBuffers(bindPoint+1, bufs:getraw())

  -- TODO make this check DEBUG-only
  if GL.glCheckFramebufferStatus(GL.GL_FRAMEBUFFER) ~= GL.GL_FRAMEBUFFER_COMPLETE then
    C.printf("Something went wrong with the Framebuffer setup");
  end
end

Gfx_OpenGL_Device.methods.bindDepthStencilView = terralib.overloadedfunction("bindDepthStencilView",
{
  terra ( self : &Gfx_OpenGL_Device, v : &Gfx_OpenGL_DepthStencilView )
    var id = Gfx_OpenGL_getTextureID(v)
    GL.glFramebufferTexture(GL.GL_FRAMEBUFFER, GL.GL_DEPTH_STENCIL_ATTACHMENT, id, 0)

    -- TODO make this check DEBUG-only
    if GL.glCheckFramebufferStatus(GL.GL_FRAMEBUFFER) ~= GL.GL_FRAMEBUFFER_COMPLETE then
      C.printf("Something went wrong with the Framebuffer setup");
    end
  end
,
  terra ( self : &Gfx_OpenGL_Device, v : &Gfx_OpenGL_DepthStencilView, face : int )
    var id = Gfx_OpenGL_getTextureID(v)
    var f = GL.GL_TEXTURE_CUBE_MAP_POSITIVE_X + face

    GL.glFramebufferTexture2D(GL.GL_FRAMEBUFFER, GL.GL_DEPTH_STENCIL_ATTACHMENT, f, id, 0)

    -- TODO make this check DEBUG-only
    if GL.glCheckFramebufferStatus(GL.GL_FRAMEBUFFER) ~= GL.GL_FRAMEBUFFER_COMPLETE then
      C.printf("Something went wrong with the Framebuffer setup: %d\n", GL.glCheckFramebufferStatus(GL.GL_FRAMEBUFFER));
    end
  end
})


-- framebuffer

struct Gfx_OpenGL_Framebuffer {}
Gfx_OpenGL.Framebuffer = &Gfx_OpenGL_Framebuffer;

terra Gfx_OpenGL_toFramebuffer( id : GL.GLuint )
  return [&Gfx_OpenGL_Framebuffer]([uint64](id) + 1);
end

terra Gfx_OpenGL_getFramebufferID( f : &Gfx_OpenGL_Framebuffer )
  return [GL.GLuint]([uint64](f) - 1);
end

terra Gfx_OpenGL_Device:getDefaultFramebuffer()
  return Gfx_OpenGL_toFramebuffer(0);
end

terra Gfx_OpenGL_Device:generateFramebuffer()
  var id : GL.GLuint;
  GL.glGenFramebuffers(1, &id);
  return Gfx_OpenGL_toFramebuffer(id);
end

terra Gfx_OpenGL_Context:setFramebuffer( f : &Gfx_OpenGL_Framebuffer )
  var id = Gfx_OpenGL_getFramebufferID(f);
  GL.glBindFramebuffer(GL.GL_FRAMEBUFFER, id);
end

terra Gfx_OpenGL_Device:swap()
  var window = self.window;
  SDL.SDL_GL_SwapWindow(window)
end

local struct Gfx_OpenGL_Timer {
  queryIDs  : GL.GLuint[2]
  startTime : GL.GLint64
  endTime   : GL.GLint64
  elapsedTime : float
  acc         : float
  average     : float
  count       : int
}
Gfx_OpenGL.Timer = Gfx_OpenGL_Timer

terra Gfx_OpenGL_Timer:init(device : &Gfx_OpenGL_Device)
  self.acc = 0
  self.average = 0
  self.count = 0

  GL.glGenQueries(2, self.queryIDs)
end

terra Gfx_OpenGL_Timer:start()
  GL.glQueryCounter(self.queryIDs[0], GL.GL_TIMESTAMP)
end

terra Gfx_OpenGL_Timer:stop()
  GL.glQueryCounter(self.queryIDs[1], GL.GL_TIMESTAMP)
end

terra Gfx_OpenGL_Timer:accumulate()
  var ready = GL.GL_FALSE
  while ready == GL.GL_FALSE do
    GL.glGetQueryObjectiv(self.queryIDs[1], GL.GL_QUERY_RESULT_AVAILABLE, &ready)
  end
  GL.glGetQueryObjecti64v(self.queryIDs[0], GL.GL_QUERY_RESULT, &self.startTime)
  GL.glGetQueryObjecti64v(self.queryIDs[1], GL.GL_QUERY_RESULT, &self.endTime)

  self.elapsedTime = (self.endTime - self.startTime) / 1000000.0f;
  self.acc = self.acc + self.elapsedTime
  self.count = self.count + 1
end

terra Gfx_OpenGL_Timer:updateAverage(numFramesToAverage : int)
  if self.count < numFramesToAverage then
    return
  end
  self.average = self.acc / self.count
  self.acc = 0
  self.count = 0
end

terra Gfx_OpenGL_Context:beginFrame()
end

terra Gfx_OpenGL_Context:endFrame()
end


-- screenshot
terra Gfx_OpenGL_Context:readFramebufferPixels( w : int, h : int, buffer : &opaque )
  GL.glReadPixels(0, 0, w, h, GL.GL_RGBA, GL.GL_UNSIGNED_BYTE, buffer);
end

-- Since Terra now type-checks eagerly, the definition of this function
-- must come at the end of this file, after all necessary GL.* functions
-- have been used at least once.
terra GL.loadFunctions()
  return GL.loadFunctionsMacro()
end


local struct Gfx_OpenGL_RWStructuredBufferBase
{
  raw : Gfx_OpenGL.Buffer;
}
Gfx_OpenGL.RWStructuredBufferBase = Gfx_OpenGL_RWStructuredBufferBase

terra Gfx_OpenGL_Context:setRWStructuredBuffer( index : int, buffer : Gfx_OpenGL_RWStructuredBufferBase )
  GL.glBindBufferBase(GL.GL_SHADER_STORAGE_BUFFER, index, buffer.raw);
end


function Gfx_OpenGL_RWStructuredBuffer(T, numElems)
  local size = terralib.sizeof(T) * numElems

  local struct SB
  {
    raw : Gfx_OpenGL.Buffer;
  }

  terra SB:init( device : &Gfx_OpenGL.Device )
    self.raw = device:beginBuildBuffer()
      :setSize(size)
      :setBindRWStructuredBuffer()
      :endBuild();
  end

  terra SB:replaceWith(device : &Gfx_OpenGL.Device, other : &SB)
    device:deleteBuffer(self.raw)
    self.raw = other.raw
  end

  terra SB:copyToHost(device : &Gfx_OpenGL.Device)
    GL.glBindBuffer(GL.GL_SHADER_STORAGE_BUFFER, self.raw)
    var mapped = GL.glMapBuffer(GL.GL_SHADER_STORAGE_BUFFER, GL.GL_READ_ONLY)

    var ret = [&T](C.malloc(size))
    C.memcpy(ret, mapped, size)

    GL.glUnmapBuffer(GL.GL_SHADER_STORAGE_BUFFER)

    return ret
  end

  return SB;
end
Gfx_OpenGL.RWStructuredBuffer = terralib.memoize(Gfx_OpenGL_RWStructuredBuffer)


return Gfx_OpenGL;
