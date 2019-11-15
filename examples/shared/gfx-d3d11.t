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

-- gfx-d3d11.t

local SDL = require "sdl"
local D3D11 = require "d3d11"
local terraToHLSL = require "terraToHLSL"

local UINT = D3D11.UINT;
local ID3D11Device = D3D11.Device;
local ID3D11DeviceContext = D3D11.DeviceContext;

local C = terralib.includecstring([[
  #include <stdio.h>
  #include <string.h>
  #include <stdlib.h>
]])


local D3DCompiler = terralib.includecstring([[
#include <D3Dcompiler.h>
]])

local terra checkResult( hr : D3D11.HRESULT )
  -- TODO: actual checking!
end


local Gfx_D3D11 = {}

--

local function getSRVCount(counts)
  return counts.textureSamplerCount
end

local function getUAVCount(counts)
  return counts.textureImageCount + counts.bufferCount
end

local Bindings = {}

function Bindings.getNextTextureSamplerBinding(counts)
  return getSRVCount(counts)
end

function Bindings.getNextBufferBinding(counts)
  return getUAVCount(counts)
end

function Bindings.getNextTextureImageBinding(counts)
  return getUAVCount(counts)
end

Gfx_D3D11.Bindings = Bindings


Gfx_D3D11.NilTexture = `nil;
Gfx_D3D11.NilBuffer = `nil;
Gfx_D3D11.NilProgram = `nil;

local Gfx_D3D11_MapMode = {}
Gfx_D3D11.MapMode = Gfx_D3D11_MapMode;
Gfx_D3D11.MapModeT = D3D11.MAP;

Gfx_D3D11_MapMode.ReadOnly = D3D11.MAP_READ;
Gfx_D3D11_MapMode.WriteOnly = D3D11.MAP_WRITE;
Gfx_D3D11_MapMode.ReadWrite = D3D11.MAP_READ_WRITE;
Gfx_D3D11_MapMode.WriteDiscard = D3D11.MAP_WRITE_DISCARD;
Gfx_D3D11_MapMode.WriteNoOverwrite = D3D11.MAP_WRITE_NO_OVERWRITE;

local Gfx_D3D11_ShaderMapMode = {}
Gfx_D3D11.ShaderMapMode = Gfx_D3D11_ShaderMapMode;

--

local fixup0 = terralib.new(vec4, 1.0, 0.0, 0.0, 0.0)
local fixup1 = terralib.new(vec4, 0.0, 1.0, 0.0, 0.0)
local fixup2 = terralib.new(vec4, 0.0, 0.0, 1.0, 0.0)
local fixup3 = terralib.new(vec4, 0.0, 0.0, 0.0, 1.0)
local fixupArray  = terralib.new(vec4[4], {fixup0, fixup1, fixup2, fixup3})
local fixup = terralib.new(mat4, fixupArray)
Gfx_D3D11.FixupMatrix = terralib.constant(fixup)

local cubefixup0 = terralib.new(vec4, 1.0, 0.0, 0.0, 0.0)
local cubefixup1 = terralib.new(vec4, 0.0, -1.0, 0.0, 0.0)
local cubefixup2 = terralib.new(vec4, 0.0, 0.0, 1.0, 0.0)
local cubefixup3 = terralib.new(vec4, 0.0, 0.0, 0.0, 1.0)
local cubefixupArray  = terralib.new(vec4[4], {cubefixup0, cubefixup1, cubefixup2, cubefixup3})
local cubefixup = terralib.new(mat4, cubefixupArray)
Gfx_D3D11.CubeFixupMatrix = terralib.constant(cubefixup)


Gfx_D3D11.VertexLayout = &D3D11.InputLayout;

struct Gfx_D3D11_Framebuffer
{
  -- TOD: multiple color targets
  numRTVs : uint
  rtv : (&D3D11.RenderTargetView)[8];
  dsv : &D3D11.DepthStencilView;
}
Gfx_D3D11.Framebuffer = &Gfx_D3D11_Framebuffer;

local struct Gfx_D3D11_Context
{
  dxContext     : &D3D11.DeviceContext;
  framebuffer   : &Gfx_D3D11_Framebuffer;
  disjointQuery : &D3D11.Query;
  disjointResult: D3D11.QUERY_DATA_TIMESTAMP_DISJOINT;
}

Gfx_D3D11.Context = Gfx_D3D11_Context;


local struct Gfx_D3D11_Device
{
  swapChain           : &D3D11.IDXGISwapChain;
  dxDevice            : &D3D11.Device;
  context             : Gfx_D3D11_Context;
  featureLevel        : D3D11.FEATURE_LEVEL;
  defaultFramebuffer  : Gfx_D3D11_Framebuffer;
}
Gfx_D3D11.Device = Gfx_D3D11_Device;

Gfx_D3D11.CLEAR_DEPTH = D3D11.CLEAR_DEPTH;
Gfx_D3D11.CLEAR_STENCIL = D3D11.CLEAR_STENCIL;
Gfx_D3D11.CLEAR_COLOR = 0x4; -- not an actual flag in D3D11


local Gfx_D3D11_PrimitiveType = {}
Gfx_D3D11.PrimitiveType = Gfx_D3D11_PrimitiveType;
Gfx_D3D11_PrimitiveType.Triangles = D3D11.PRIMITIVE_TOPOLOGY_TRIANGLELIST;

local Gfx_D3D11_DepthFunc = {}
Gfx_D3D11.DepthFunc = Gfx_D3D11_DepthFunc;
Gfx_D3D11_DepthFunc.Less = D3D11.COMPARISON_LESS;


local Gfx_D3D11_FillMode = {}
Gfx_D3D11.FillMode            = Gfx_D3D11_FillMode;
--Gfx_D3D11_FillMode.Point      = D3D11.FILL_POINT; -- no such flag in D3D11
Gfx_D3D11_FillMode.WireFrame  = D3D11.FILL_WIREFRAME;
Gfx_D3D11_FillMode.Solid      = D3D11.FILL_SOLID;



local Gfx_D3D11_Usage = {}
Gfx_D3D11.Usage           = Gfx_D3D11_Usage;
Gfx_D3D11_Usage.Default   = D3D11.USAGE_DEFAULT;
Gfx_D3D11_Usage.Immutable = D3D11.USAGE_IMMUTABLE;
Gfx_D3D11_Usage.Dynamic   = D3D11.USAGE_DYNAMIC;
Gfx_D3D11_Usage.Staging   = D3D11.USAGE_STAGING;

local Gfx_D3D11_CPUAccess = {}
Gfx_D3D11.CPUAccess = Gfx_D3D11_CPUAccess;
Gfx_D3D11_CPUAccess.None = 0;
Gfx_D3D11_CPUAccess.Write = D3D11.CPU_ACCESS_WRITE;
Gfx_D3D11_CPUAccess.Read = D3D11.CPU_ACCESS_READ;
Gfx_D3D11_CPUAccess.ReadWrite = D3D11.CPU_ACCESS_READ or D3D11.CPU_ACCESS_WRITE;


local Gfx_D3D11_IndexFormat = {}
Gfx_D3D11.IndexFormat         = Gfx_D3D11_IndexFormat;
Gfx_D3D11_IndexFormat.UInt16  = D3D11.FORMAT_R16_UINT;
Gfx_D3D11_IndexFormat.UInt32  = D3D11.FORMAT_R32_UINT;

local Gfx_D3D11_AttributeFormat = {}
Gfx_D3D11.AttributeFormat     = Gfx_D3D11_AttributeFormat;
Gfx_D3D11_AttributeFormat.Vec2 = D3D11.FORMAT_R32G32_FLOAT;
Gfx_D3D11_AttributeFormat.Vec3 = D3D11.FORMAT_R32G32B32_FLOAT;
Gfx_D3D11_AttributeFormat.Vec4 = D3D11.FORMAT_R32G32B32A32_FLOAT;
Gfx_D3D11_AttributeFormat.RGBA8 = D3D11.FORMAT_R8G8B8A8_UNORM;


local struct Gfx_D3D11_DepthStencilStateDesc
{
  device : &D3D11.Device;
  desc : D3D11.DEPTH_STENCIL_DESC;
}
Gfx_D3D11.DepthStencilStateDesc = Gfx_D3D11_DepthStencilStateDesc

terra Gfx_D3D11_DepthStencilStateDesc:init()
  self.desc.DepthEnable = D3D11.TRUE; -- TODO: better defaults
  self.desc.DepthWriteMask = D3D11.DEPTH_WRITE_MASK_ALL;
  self.desc.DepthFunc = D3D11.COMPARISON_LESS;
  self.desc.StencilEnable = D3D11.FALSE;
  self.desc.StencilReadMask = 0xFF;
  self.desc.StencilWriteMask = 0xFF;
  self.desc.FrontFace .StencilFailOp      = D3D11.STENCIL_OP_KEEP;
  self.desc.FrontFace .StencilDepthFailOp = D3D11.STENCIL_OP_KEEP;
  self.desc.FrontFace .StencilPassOp      = D3D11.STENCIL_OP_KEEP;
  self.desc.FrontFace .StencilFunc        = D3D11.COMPARISON_ALWAYS;
  self.desc.BackFace  .StencilFailOp      = D3D11.STENCIL_OP_KEEP;
  self.desc.BackFace  .StencilDepthFailOp = D3D11.STENCIL_OP_KEEP;
  self.desc.BackFace  .StencilPassOp      = D3D11.STENCIL_OP_KEEP;
  self.desc.BackFace  .StencilFunc        = D3D11.COMPARISON_ALWAYS;
end

terra Gfx_D3D11_DepthStencilStateDesc.methods.create( device : &D3D11.Device )
  var result : Gfx_D3D11_DepthStencilStateDesc;
  result.device = device;
  result:init();
  return result;
end

terra Gfx_D3D11_DepthStencilStateDesc:setDepthFunc( val : D3D11.COMPARISON_FUNC )
  self.desc.DepthFunc = val;
  return self;
end

terra Gfx_D3D11_DepthStencilStateDesc:setEnableDepthTest( val : bool )
  self.desc.DepthEnable = terralib.select(val, D3D11.TRUE, D3D11.FALSE);
  return self;
end

terra Gfx_D3D11_DepthStencilStateDesc:setEnableDepthWrite( val : bool )
  self.desc.DepthWriteMask = terralib.select(val, D3D11.DEPTH_WRITE_MASK_ALL, D3D11.DEPTH_WRITE_MASK_ZERO);
  return self;
end

Gfx_D3D11.DepthStencilState = &D3D11.DepthStencilState;

terra Gfx_D3D11_Device:beginBuildDepthStencilState() : Gfx_D3D11_DepthStencilStateDesc
  return Gfx_D3D11_DepthStencilStateDesc.create(self.dxDevice);
end

terra Gfx_D3D11_DepthStencilStateDesc:endBuild() : &D3D11.DepthStencilState
  var result : &D3D11.DepthStencilState;

  var hr = self.device.lpVtbl.CreateDepthStencilState(
    self.device,
    &self.desc,
    &result);

  return result;
end



local struct Gfx_D3D11_RasterizerStateDesc
{
  device  : &D3D11.Device;
  desc    : D3D11.RASTERIZER_DESC;
}
Gfx_D3D11.RasterizerStateDesc = Gfx_D3D11_RasterizerStateDesc;

local Gfx_D3D11_RasterizerState = &D3D11.RasterizerState;
Gfx_D3D11.RasterizerState = Gfx_D3D11_RasterizerState;


terra Gfx_D3D11_RasterizerStateDesc:init()
  -- TODO: need to pick good defaults...
  self.desc.FillMode = D3D11.FILL_SOLID;
  self.desc.CullMode = D3D11.CULL_NONE;
  self.desc.FrontCounterClockwise = D3D11.FALSE;
  self.desc.DepthBias = 0;
  self.desc.SlopeScaledDepthBias = 0.0;
  self.desc.DepthBiasClamp = 0.0;
  self.desc.DepthClipEnable = D3D11.TRUE;
  self.desc.ScissorEnable = D3D11.FALSE;
  self.desc.MultisampleEnable = D3D11.FALSE;
  self.desc.AntialiasedLineEnable = D3D11.FALSE;
end

terra Gfx_D3D11_RasterizerStateDesc.methods.create( device : &D3D11.Device )
  var result : Gfx_D3D11_RasterizerStateDesc;
  result.device = device;
  result:init();
  return result;
end

terra Gfx_D3D11_RasterizerStateDesc:setFillMode( val : D3D11.FILL_MODE )
  self.desc.FillMode = val;
  return self;
end

terra Gfx_D3D11_RasterizerStateDesc:setEnableScissor( val : bool )
  self.desc.ScissorEnable = terralib.select(val, D3D11.TRUE, D3D11.FALSE);
  return self;
end

terra Gfx_D3D11_RasterizerStateDesc:setEnableDepthClip( val : bool )
  self.desc.DepthClipEnable = terralib.select(val, D3D11.TRUE, D3D11.FALSE);
  return self;
end

terra Gfx_D3D11_Device:beginBuildRasterizerState() : Gfx_D3D11_RasterizerStateDesc
  return Gfx_D3D11_RasterizerStateDesc.create( self.dxDevice );
end

terra Gfx_D3D11_RasterizerStateDesc:endBuild() : Gfx_D3D11_RasterizerState
  var result : Gfx_D3D11_RasterizerState;

  var hr = self.device.lpVtbl.CreateRasterizerState(
    self.device,
    &self.desc,
    &result);

  return result;
end

--

local Gfx_D3D11_Filter = {};
Gfx_D3D11.Filter = Gfx_D3D11_Filter;
Gfx_D3D11_Filter.MinMagMipPoint = D3D11.FILTER_MIN_MAG_MIP_POINT;

Gfx_D3D11_Filter.MinMagMipPoint                           = D3D11.FILTER_MIN_MAG_MIP_POINT;
Gfx_D3D11_Filter.MinMagPoint_MipLinear                    = D3D11.FILTER_MIN_MAG_POINT_MIP_LINEAR;
Gfx_D3D11_Filter.MinPoint_MagLinear_MipPoint              = D3D11.FILTER_MIN_POINT_MAG_LINEAR_MIP_POINT;
Gfx_D3D11_Filter.MinPoint_MagMipLinear                    = D3D11.FILTER_MIN_POINT_MAG_MIP_LINEAR;
Gfx_D3D11_Filter.MinLinear_MagMipPoint                    = D3D11.FILTER_MIN_LINEAR_MAG_MIP_POINT;
Gfx_D3D11_Filter.MinLinear_MagPoint_MipLinear             = D3D11.FILTER_MIN_LINEAR_MAG_POINT_MIP_LINEAR;
Gfx_D3D11_Filter.MinMagLinear_MipPoint                    = D3D11.FILTER_MIN_MAG_LINEAR_MIP_POINT;
Gfx_D3D11_Filter.MinMagMipLinear                          = D3D11.FILTER_MIN_MAG_MIP_LINEAR;
Gfx_D3D11_Filter.Anisotropic                              = D3D11.FILTER_ANISOTROPIC;
Gfx_D3D11_Filter.Comparison_MinMagMipPoint                = D3D11.FILTER_COMPARISON_MIN_MAG_MIP_POINT;
Gfx_D3D11_Filter.Comparison_MinMagPoint_MipLinear         = D3D11.FILTER_COMPARISON_MIN_MAG_POINT_MIP_LINEAR;
Gfx_D3D11_Filter.Comparison_MinPoint_MagLinear_MipPoint   = D3D11.FILTER_COMPARISON_MIN_POINT_MAG_LINEAR_MIP_POINT;
Gfx_D3D11_Filter.Comparison_MinPoint_MagMipLinear         = D3D11.FILTER_COMPARISON_MIN_POINT_MAG_MIP_LINEAR;
Gfx_D3D11_Filter.Comparison_MinLinear_MagMipPoint         = D3D11.FILTER_COMPARISON_MIN_LINEAR_MAG_MIP_POINT;
Gfx_D3D11_Filter.Comparison_MinLinear_MagPoint_MipLinear  = D3D11.FILTER_COMPARISON_MIN_LINEAR_MAG_POINT_MIP_LINEAR;
Gfx_D3D11_Filter.Comparison_MinMagLinear_MipPoint         = D3D11.FILTER_COMPARISON_MIN_MAG_LINEAR_MIP_POINT;
Gfx_D3D11_Filter.Comparison_MinMagMipLinear               = D3D11.FILTER_COMPARISON_MIN_MAG_MIP_LINEAR;
Gfx_D3D11_Filter.Comparison_Anisotropic                   = D3D11.FILTER_COMPARISON_ANISOTROPIC;


local Gfx_D3D11_TextureAddressMode = {};
Gfx_D3D11.TextureAddressMode = Gfx_D3D11_TextureAddressMode;

Gfx_D3D11_TextureAddressMode.Wrap        = D3D11.TEXTURE_ADDRESS_WRAP;
Gfx_D3D11_TextureAddressMode.Mirror      = D3D11.TEXTURE_ADDRESS_MIRROR;
Gfx_D3D11_TextureAddressMode.Clamp       = D3D11.TEXTURE_ADDRESS_CLAMP;
Gfx_D3D11_TextureAddressMode.Mirror_Once = D3D11.TEXTURE_ADDRESS_MIRROR_ONCE;

--

local struct SamplerStateDesc
{
  dxDevice  : &D3D11.Device;
  desc    : D3D11.SAMPLER_DESC;
}

terra SamplerStateDesc:init( dxDevice : &D3D11.Device )
  self.dxDevice = dxDevice;
  self.desc.Filter = D3D11.FILTER_MIN_MAG_MIP_LINEAR;
  self.desc.AddressU = D3D11.TEXTURE_ADDRESS_WRAP;
  self.desc.AddressV = D3D11.TEXTURE_ADDRESS_WRAP;
  self.desc.AddressW = D3D11.TEXTURE_ADDRESS_WRAP;
  self.desc.MinLOD = -D3D11.FLT_MAX;
  self.desc.MaxLOD = D3D11.FLT_MAX;
  self.desc.MipLODBias = 0.0;
  self.desc.MaxAnisotropy = 16;
  self.desc.ComparisonFunc = D3D11.COMPARISON_NEVER;
  for i = 1,4 do
    self.desc.BorderColor[i] = 0.0;
  end
end

terra SamplerStateDesc:setFilter( f : D3D11.FILTER )
  self.desc.Filter = f;
  return self;
end

terra SamplerStateDesc:setMaxAnisotropy( val : int )
  self.desc.MaxAnisotropy = val;
  return self;
end

terra SamplerStateDesc:setAddressModeU( m : D3D11.TEXTURE_ADDRESS_MODE )
  self.desc.AddressU = m
  return self
end

terra SamplerStateDesc:setAddressModeV( m : D3D11.TEXTURE_ADDRESS_MODE )
  self.desc.AddressV = m
  return self
end

terra SamplerStateDesc:setAddressModeW( m : D3D11.TEXTURE_ADDRESS_MODE )
  self.desc.AddressW = m
  return self
end

local Gfx_D3D11_SamplerState = &D3D11.SamplerState;
Gfx_D3D11.SamplerState = Gfx_D3D11_SamplerState;


terra SamplerStateDesc:endBuild()
  var result : Gfx_D3D11_SamplerState;

  var dxDevice = self.dxDevice;

  var hr = dxDevice.lpVtbl.CreateSamplerState(
    dxDevice,
    &self.desc,
    &result);

  return result;
end

terra Gfx_D3D11_Device:beginBuildSamplerState()
  var desc : SamplerStateDesc;
  desc:init(self.dxDevice);
  return desc;
end

--

Gfx_D3D11.BlendFactor = {}
Gfx_D3D11.BlendFactor.One = D3D11.BLEND_ONE;
Gfx_D3D11.BlendFactor.Zero = D3D11.BLEND_ZERO;
Gfx_D3D11.BlendFactor.SrcAlpha = D3D11.BLEND_SRC_ALPHA;
Gfx_D3D11.BlendFactor.InvSrcAlpha = D3D11.BLEND_INV_SRC_ALPHA;

--

local struct BlendStateBuilder
{
  device : &Gfx_D3D11_Device;
  desc : D3D11.BLEND_DESC;
}

local Gfx_D3D11_MaxColorTargets = 8;

terra BlendStateBuilder:init( device : &Gfx_D3D11_Device )
  self.device = device;

  self.desc.AlphaToCoverageEnable = D3D11.FALSE;
  self.desc.IndependentBlendEnable = D3D11.FALSE;

  for i = 0,Gfx_D3D11_MaxColorTargets do
    self.desc.RenderTarget[i].BlendEnable = D3D11.FALSE;
    self.desc.RenderTarget[i].SrcBlend = D3D11.BLEND_ONE;
    self.desc.RenderTarget[i].DestBlend = D3D11.BLEND_ZERO;
    self.desc.RenderTarget[i].BlendOp = D3D11.BLEND_OP_ADD;
    self.desc.RenderTarget[i].SrcBlendAlpha = D3D11.BLEND_ONE;
    self.desc.RenderTarget[i].DestBlendAlpha = D3D11.BLEND_ZERO;
    self.desc.RenderTarget[i].BlendOpAlpha = D3D11.BLEND_OP_ADD;
    self.desc.RenderTarget[i].RenderTargetWriteMask = D3D11.D3D11_COLOR_WRITE_ENABLE_ALL;
  end
end

terra Gfx_D3D11_Device:beginBuildBlendState()
  var builder : BlendStateBuilder;
  builder:init(self);
  return builder;
end

terra BlendStateBuilder:setEnableBlend( val : bool )
  self.desc.RenderTarget[0].BlendEnable = terralib.select(val, D3D11.TRUE, D3D11.FALSE);
  return self;
end

terra BlendStateBuilder:setBlendFunc( src : D3D11.BLEND, dst : D3D11.BLEND )
  self.desc.RenderTarget[0].SrcBlend = src;
  self.desc.RenderTarget[0].DestBlend = dst;
  self.desc.RenderTarget[0].SrcBlendAlpha = src;
  self.desc.RenderTarget[0].DestBlendAlpha = dst;
  return self;
end

terra BlendStateBuilder:endBuild()

  var dxDevice = self.device.dxDevice;

  var dxBlendState : &D3D11.BlendState = nil;

  var hr = dxDevice.lpVtbl.CreateBlendState(
    dxDevice,
    &self.desc,
    &dxBlendState);
  checkResult(hr);

  return dxBlendState;
end

terra Gfx_D3D11_Device:deleteBlendState( b : &D3D11.BlendState )
  b.lpVtbl.Release(b);
end

Gfx_D3D11.BlendState = &D3D11.BlendState;

--


--

local Gfx_D3D11_TextureFormat = {}
Gfx_D3D11.TextureFormat = Gfx_D3D11_TextureFormat;
Gfx_D3D11.TextureFormatT = D3D11.DXGI_FORMAT;

Gfx_D3D11_TextureFormat.R8 = D3D11.FORMAT_R8_UNORM;
Gfx_D3D11_TextureFormat.RGBA8 = D3D11.FORMAT_R8G8B8A8_UNORM;
Gfx_D3D11_TextureFormat.R16F  = D3D11.FORMAT_R16_FLOAT;
Gfx_D3D11_TextureFormat.RGBA16 = D3D11.FORMAT_R16G16B16A16_UNORM;
Gfx_D3D11_TextureFormat.RGBA16U = D3D11.FORMAT_R16G16B16A16_UINT;
Gfx_D3D11_TextureFormat.RGBA16F = D3D11.FORMAT_R16G16B16A16_FLOAT;
Gfx_D3D11_TextureFormat.RGBA32F = D3D11.FORMAT_R32G32B32A32_FLOAT;
Gfx_D3D11_TextureFormat.DEPTH24_STENCIL8 = D3D11.FORMAT_R24G8_TYPELESS

Gfx_D3D11_TextureFormat.BC1_UNORM = D3D11.FORMAT_BC1_UNORM;
Gfx_D3D11_TextureFormat.BC2_UNORM = D3D11.FORMAT_BC2_UNORM;
Gfx_D3D11_TextureFormat.BC3_UNORM = D3D11.FORMAT_BC3_UNORM;
Gfx_D3D11_TextureFormat.BC5_UNORM = D3D11.FORMAT_BC5_UNORM;
Gfx_D3D11_TextureFormat.BC5_SNORM = D3D11.FORMAT_BC5_SNORM;

-- sRGB formats
Gfx_D3D11_TextureFormat.BC1_UNORM_SRGB = D3D11.FORMAT_BC1_UNORM_SRGB;
Gfx_D3D11_TextureFormat.BC2_UNORM_SRGB = D3D11.FORMAT_BC2_UNORM_SRGB;
Gfx_D3D11_TextureFormat.BC3_UNORM_SRGB = D3D11.FORMAT_BC3_UNORM_SRGB;
Gfx_D3D11_TextureFormat.RGBA8_SRGB     = D3D11.FORMAT_R8G8B8A8_UNORM_SRGB;

Gfx_D3D11_TextureFormat.UNKNOWN = D3D11.FORMAT_UNKNOWN;

local Gfx_D3D11_ShaderTextureFormat = {}
Gfx_D3D11.ShaderTextureFormat = Gfx_D3D11_ShaderTextureFormat;

Gfx_D3D11_ShaderTextureFormat[D3D11.FORMAT_R8_UNORM]            = "uint";
Gfx_D3D11_ShaderTextureFormat[D3D11.FORMAT_R8G8B8A8_UNORM]      = "uint4";
Gfx_D3D11_ShaderTextureFormat[D3D11.FORMAT_R16G16B16A16_UINT]   = "uint4"
Gfx_D3D11_ShaderTextureFormat[D3D11.FORMAT_R16G16B16A16_UNORM]  = "uint4"
Gfx_D3D11_ShaderTextureFormat[D3D11.FORMAT_R16G16B16A16_FLOAT]  = "float4"
Gfx_D3D11_ShaderTextureFormat[D3D11.FORMAT_R32G32B32A32_FLOAT]  = "float4"

local terra textureFormatToDSVFormat( f : D3D11.DXGI_FORMAT )
  if f == D3D11.FORMAT_R24G8_TYPELESS then
    return D3D11.DXGI_FORMAT_D24_UNORM_S8_UINT
  else
    return f
  end
end

local terra textureFormatToSRVFormat( f : D3D11.DXGI_FORMAT )
  if f == D3D11.FORMAT_R24G8_TYPELESS then
    return D3D11.DXGI_FORMAT_R24_UNORM_X8_TYPELESS
  else
    return f
  end
end


local Gfx_D3D11_BindFlag = {};
Gfx_D3D11.BindFlag = Gfx_D3D11_BindFlag;

Gfx_D3D11_BindFlag.SHADER_RESOURCE  = D3D11.BIND_SHADER_RESOURCE;
Gfx_D3D11_BindFlag.DEPTH_STENCIL    = D3D11.BIND_DEPTH_STENCIL;
Gfx_D3D11_BindFlag.UNORDERED_ACCESS = D3D11.BIND_UNORDERED_ACCESS;
Gfx_D3D11_BindFlag.RENDER_TARGET    = D3D11.BIND_RENDER_TARGET;

local struct Texture2DDesc
{
  device    : &D3D11.Device;
  desc      : D3D11.TEXTURE2D_DESC;
  initData  : D3D11.SUBRESOURCE_DATA[16];
  hasInitData : bool;
}

terra Texture2DDesc:init()
  self.desc.Width = 32;
  self.desc.Height = 32;
  self.desc.MipLevels = 0;
  self.desc.ArraySize = 1;
  self.desc.Format = D3D11.FORMAT_R8G8B8A8_UNORM;
  self.desc.SampleDesc.Count = 1;
  self.desc.SampleDesc.Quality = 0;
  self.desc.Usage = D3D11.USAGE_DEFAULT;
  self.desc.BindFlags = D3D11.BIND_SHADER_RESOURCE; -- TODO: no view type by default?
  self.desc.CPUAccessFlags = 0;
  self.desc.MiscFlags = 0;
  self.hasInitData = false;
end

terra Texture2DDesc:setFormat( f : D3D11.FORMAT )
  self.desc.Format = f;
  return self;
end

terra Texture2DDesc:setUsage( u : D3D11.USAGE )
  self.desc.Usage = u;
  return self
end

terra Texture2DDesc:setMipLevelCount( val : int )
  self.desc.MipLevels = val;
  return self;
end

terra Texture2DDesc:enableMipGeneration()
  self.desc.MiscFlags = self.desc.MiscFlags or D3D11.RESOURCE_MISC_GENERATE_MIPS
  return self
end

Texture2DDesc.methods.setInitDataForLevel = terralib.overloadedfunction("setInitDataForLevel",
{
  terra( self : &Texture2DDesc, level : int, val : &uint8, pitch : int )
    self.hasInitData = true;
    self.initData[level].pSysMem = val;
    self.initData[level].SysMemPitch = pitch;
    self.initData[level].SysMemSlicePitch = 0;
    return self;
  end
,
  terra( self : &Texture2DDesc, level : int, val : &uint16, pitch : int )
    self.hasInitData = true;
    self.initData[level].pSysMem = val;
    self.initData[level].SysMemPitch = pitch;
    self.initData[level].SysMemSlicePitch = 0;
    return self;
  end
})

terra Texture2DDesc:setBindFlags( b : D3D11.BIND_FLAG )
  self.desc.BindFlags = b
  return self
end

terra Texture2DDesc:setCPUAccess( access : D3D11.UINT )
  self.desc.CPUAccessFlags = access
  return self
end

struct Gfx_D3D11_Texture
{
  resource : &D3D11.Resource;
  srv : &D3D11.ShaderResourceView;
  uav : &D3D11.UnorderedAccessView;
  format : D3D11.DXGI_FORMAT;
}
Gfx_D3D11.Texture = &Gfx_D3D11_Texture;

local terra nextMipExtent( val : int ) : int
  var v = val / 2;
  if v <= 0 then v = 1 end
  return v;
end

terra Texture2DDesc:endBuild()
  var dxDevice = self.device;
  var dxTexture : &D3D11.Texture2D;

  -- figure out the level count, if needed
  var levelCount = self.desc.MipLevels;
  if( levelCount == 0 )then
    var x = self.desc.Width;
    var y = self.desc.Height;
    levelCount = 1;
    while (x > 1) or (y >1) do
      x = nextMipExtent(x);
      y = nextMipExtent(y);
      levelCount = levelCount + 1;
    end
  end
  self.desc.MipLevels = levelCount;

  var hr = dxDevice.lpVtbl.CreateTexture2D(
    dxDevice,
    &self.desc,
    terralib.select(self.hasInitData, &self.initData[0], nil),
    &dxTexture);
  checkResult(hr);

  var dxResource = [&D3D11.Resource](dxTexture);

  var result = [&Gfx_D3D11_Texture](C.malloc([terralib.sizeof(Gfx_D3D11_Texture)]));
  result.resource = dxResource;
  result.srv = nil;
  result.uav = nil;
  result.format = self.desc.Format;

  if self.desc.Usage ~= D3D11.USAGE_STAGING then
    -- TODO verify the SHADER_RESOURCE_VIEW_DESC is set up properly
    if (self.desc.BindFlags and D3D11.BIND_SHADER_RESOURCE) ~= 0 then
      var srvDesc : D3D11.SHADER_RESOURCE_VIEW_DESC;
      srvDesc.Format = textureFormatToSRVFormat(self.desc.Format);
      srvDesc.ViewDimension = D3D11.SRV_DIMENSION_TEXTURE2D;
      srvDesc._0.Texture2D.MostDetailedMip = 0;
      srvDesc._0.Texture2D.MipLevels = -1;

      var dxSRV : &D3D11.ShaderResourceView;
      hr = dxDevice.lpVtbl.CreateShaderResourceView(
        dxDevice,
        dxResource,
        &srvDesc,
        &dxSRV);

      result.srv = dxSRV;
    end

    if (self.desc.BindFlags and D3D11.BIND_UNORDERED_ACCESS) ~= 0 then
      var uavDesc : D3D11.UNORDERED_ACCESS_VIEW_DESC;
      uavDesc.Format = self.desc.Format;
      uavDesc.ViewDimension = D3D11.UAV_DIMENSION_TEXTURE2D;
      uavDesc._0.Texture2D.MipSlice = 0;

      var dxUAV : &D3D11.UnorderedAccessView;
      hr = dxDevice.lpVtbl.CreateUnorderedAccessView(
        dxDevice,
        dxResource,
        &uavDesc,
        &dxUAV);

      result.uav = dxUAV;
    end
  end

  return result;
end

terra Gfx_D3D11_Device:beginBuildTexture2D( extentX : int, extentY : int )
  var builder : Texture2DDesc;
  builder.device = self.dxDevice;
  builder:init();
  builder.desc.Width = extentX;
  builder.desc.Height = extentY;
  return builder;
end


-- Texture Cube Map

local struct TextureCubeMapDesc
{
  device      : &D3D11.Device;
  desc        : D3D11.TEXTURE2D_DESC;
  initData    : D3D11.SUBRESOURCE_DATA[6][16];
  hasInitData : bool;
}

terra TextureCubeMapDesc:init()
  self.desc.Width = 32;
  self.desc.Height = 32;
  self.desc.MipLevels = 0;
  self.desc.ArraySize = 6;
  self.desc.Format = D3D11.FORMAT_R8G8B8A8_UNORM;
  self.desc.SampleDesc.Count = 1;
  self.desc.SampleDesc.Quality = 0;
  self.desc.Usage = D3D11.USAGE_DEFAULT;
  self.desc.BindFlags = D3D11.BIND_SHADER_RESOURCE; -- TODO: no view type by default?
  self.desc.CPUAccessFlags = 0;
  self.desc.MiscFlags = D3D11.RESOURCE_MISC_TEXTURECUBE;
  self.hasInitData = false;
end

terra TextureCubeMapDesc:setFormat( f : D3D11.FORMAT )
  self.desc.Format = f;
  return self;
end

terra TextureCubeMapDesc:setMipLevelCount( val : int )
  self.desc.MipLevels = val;
  return self;
end

terra TextureCubeMapDesc:setInitDataForLevel( face : int, level : int, val : &uint8, pitch : int )
  self.hasInitData = true;
  self.initData[face][level].pSysMem = val;
  self.initData[face][level].SysMemPitch = pitch;
  self.initData[face][level].SysMemSlicePitch = 0;
  return self;
end

terra TextureCubeMapDesc:setBindFlags( b : D3D11.BIND_FLAG )
  self.desc.BindFlags = b;
  return self;
end

terra TextureCubeMapDesc:setUsage( u : D3D11.USAGE )
  self.desc.Usage = u;
  return self;
end

terra TextureCubeMapDesc:setCPUAccess( access : D3D11.UINT )
  self.desc.CPUAccessFlags = access;
  return self;
end

terra TextureCubeMapDesc:endBuild()
  var dxDevice = self.device;
  var dxTexture : &D3D11.Texture2D;

  -- figure out the level count, if needed
  var levelCount = self.desc.MipLevels;
  if( levelCount == 0 )then
    var x = self.desc.Width;
    var y = self.desc.Height;
    levelCount = 1;
    while (x > 1) or (y >1) do
      x = nextMipExtent(x);
      y = nextMipExtent(y);
      levelCount = levelCount + 1;
    end
  end
  self.desc.MipLevels = levelCount;

  var hr = dxDevice.lpVtbl.CreateTexture2D(
    dxDevice,
    &self.desc,
    terralib.select(self.hasInitData, &self.initData[0][0], nil),
    &dxTexture);
  checkResult(hr);

  var dxResource = [&D3D11.Resource](dxTexture);

  var result = [&Gfx_D3D11_Texture](C.malloc([terralib.sizeof(Gfx_D3D11_Texture)]));
  result.resource = dxResource;
  result.srv = nil;
  result.uav = nil;
  result.format = self.desc.Format;

  if self.desc.Usage ~= D3D11.USAGE_STAGING then
    -- TODO verify the SHADER_RESOURCE_VIEW_DESC is set up properly
    if (self.desc.BindFlags and D3D11.BIND_SHADER_RESOURCE) ~= 0 then
      var srvDesc : D3D11.SHADER_RESOURCE_VIEW_DESC;
      srvDesc.Format = textureFormatToSRVFormat(self.desc.Format);
      srvDesc.ViewDimension = D3D11.SRV_DIMENSION_TEXTURECUBE;
      srvDesc._0.TextureCube.MostDetailedMip = 0;
      srvDesc._0.TextureCube.MipLevels = -1;

      var dxSRV : &D3D11.ShaderResourceView;
      hr = dxDevice.lpVtbl.CreateShaderResourceView(
        dxDevice,
        dxResource,
        &srvDesc,
        &dxSRV);

      result.srv = dxSRV;
    end

    if (self.desc.BindFlags and D3D11.BIND_UNORDERED_ACCESS) ~= 0 then
      var uavDesc : D3D11.UNORDERED_ACCESS_VIEW_DESC;
      uavDesc.Format = self.desc.Format;
      uavDesc.ViewDimension = D3D11.UAV_DIMENSION_TEXTURE2DARRAY;
      uavDesc._0.Texture2DArray.MipSlice = 0;
      uavDesc._0.Texture2DArray.FirstArraySlice = 0;
      uavDesc._0.Texture2DArray.ArraySize = 6;

      var dxUAV : &D3D11.UnorderedAccessView;
      hr = dxDevice.lpVtbl.CreateUnorderedAccessView(
        dxDevice,
        dxResource,
        &uavDesc,
        &dxUAV);

      result.uav = dxUAV;
    end
  end

  return result;
end

terra Gfx_D3D11_Device:beginBuildTextureCubeMap( extentX : int, extentY : int )
  var builder : TextureCubeMapDesc;
  builder.device = self.dxDevice;
  builder:init();
  builder.desc.Width = extentX;
  builder.desc.Height = extentY;
  return builder;
end


--

Gfx_D3D11.Buffer = &D3D11.Buffer;

--Gfx_D3D11.UniformBuffer = GLUtils.UniformBuffer;

terra Gfx_D3D11.getWindowFlags() : int
  return 0;
end

terra Gfx_D3D11_Device:init( sdlWindow : &SDL.SDL_Window )

  var info : SDL.SDL_SysWMinfo;
  info.version.major = SDL.SDL_MAJOR_VERSION;
  info.version.minor = SDL.SDL_MINOR_VERSION;
  info.version.patch = SDL.SDL_PATCHLEVEL;
  SDL.SDL_GetWindowWMInfo(sdlWindow, &info);
  var window = info.info.win.window;

  var deviceFlags : D3D11.UINT = 0;

  deviceFlags = deviceFlags or D3D11.CREATE_DEVICE_DEBUG;

  var swapChainDesc : D3D11.DXGI_SWAP_CHAIN_DESC;
  C.memset(&swapChainDesc, 0, terralib.sizeof(D3D11.DXGI_SWAP_CHAIN_DESC));

  var clientRect : D3D11.RECT;
  D3D11.GetClientRect(window, &clientRect);
  var extentX = clientRect.right - clientRect.left;
  var extentY = clientRect.bottom - clientRect.top;


  swapChainDesc.BufferDesc.Width = extentX;
  swapChainDesc.BufferDesc.Height = extentY;
  swapChainDesc.BufferDesc.RefreshRate.Numerator = 60;
  swapChainDesc.BufferDesc.RefreshRate.Denominator = 1;
  swapChainDesc.BufferDesc.Format = D3D11.DXGI_FORMAT_R8G8B8A8_UNORM; -- TODO: SRGB?

  swapChainDesc.SampleDesc.Count = 1;
  swapChainDesc.SampleDesc.Quality = 0;
  swapChainDesc.BufferUsage = D3D11.DXGI_USAGE_RENDER_TARGET_OUTPUT;
  swapChainDesc.BufferCount = 2;
  swapChainDesc.OutputWindow = window;
  swapChainDesc.Windowed = D3D11.TRUE;
  swapChainDesc.SwapEffect = D3D11.DXGI_SWAP_EFFECT_DISCARD;
  swapChainDesc.Flags = 0;

  var swapChain : &D3D11.IDXGISwapChain = nil;
  var device : &D3D11.Device = nil;
  var featureLevel : D3D11.FEATURE_LEVEL;
  var context : &D3D11.DeviceContext = nil;

  var hr = D3D11.CreateDeviceAndSwapChain(
    nil,                        -- adapter
    D3D11.DRIVER_TYPE_HARDWARE, -- driver type
    nil,                        -- software module
    deviceFlags,                -- device creation flags
    nil,                        -- list of feature levels
    0,                          -- number of feature levels
    D3D11.SDK_VERSION,          -- SDK version
    &swapChainDesc,
    &swapChain,
    &device,
    &featureLevel,
    &context);

  if hr < 0 then
    C.printf("error initializing D3D11 0x%0x\n", hr);
    return false
  end

  self.swapChain = swapChain;
  self.dxDevice = device;
  self.context.dxContext = context;
  self.featureLevel = featureLevel;

  var queryDesc : D3D11.QUERY_DESC;
  queryDesc.MiscFlags = 0;
  queryDesc.Query = D3D11.QUERY_TIMESTAMP_DISJOINT;
  device.lpVtbl.CreateQuery(device, &queryDesc, &self.context.disjointQuery);

  var IID_ID3D11Texture2D = D3D11.GUID{0x6f15aaf2,0xd208,0x4e89,arrayof(uint8, 0x9a,0xb4,0x48,0x95,0x35,0xd3,0x4f,0x9c)};
--  DEFINE_GUID(IID_ID3D11Texture2D,0x6f15aaf2,0xd208,0x4e89,0x9a,0xb4,0x48,0x95,0x35,0xd3,0x4f,0x9c);


  var backBufferTexture : &D3D11.Texture2D;
  hr = swapChain.lpVtbl.GetBuffer(swapChain, 0, &IID_ID3D11Texture2D, [&D3D11.LPVOID](&backBufferTexture));

  var descRTV : D3D11.RENDER_TARGET_VIEW_DESC;
  descRTV.Format = D3D11.FORMAT_R8G8B8A8_UNORM_SRGB;
  descRTV.ViewDimension = D3D11.RTV_DIMENSION_TEXTURE2D;
  descRTV._0.Texture2D.MipSlice = 0;

  var backBufferRTV : &D3D11.RenderTargetView;
  hr = device.lpVtbl.CreateRenderTargetView(device, [&D3D11.Resource](backBufferTexture), &descRTV, &backBufferRTV);


-- make a depth-stencil buffer to match
  var depthBufferDesc : D3D11.TEXTURE2D_DESC;
  depthBufferDesc.Width = extentX;
  depthBufferDesc.Height = extentY;
  depthBufferDesc.MipLevels = 1;
  depthBufferDesc.ArraySize = 1;
  depthBufferDesc.Format = D3D11.FORMAT_D24_UNORM_S8_UINT;
  depthBufferDesc.SampleDesc = swapChainDesc.SampleDesc;
  depthBufferDesc.Usage = D3D11.USAGE_DEFAULT;
  depthBufferDesc.BindFlags = D3D11.BIND_DEPTH_STENCIL;
  depthBufferDesc.CPUAccessFlags = 0;
  depthBufferDesc.MiscFlags = 0;

  var depthBufferTexure : &D3D11.Texture2D;
  hr = device.lpVtbl.CreateTexture2D(device, &depthBufferDesc, nil, &depthBufferTexure);

  var depthBufferDSV : &D3D11.DepthStencilView;
  hr = device.lpVtbl.CreateDepthStencilView(device, [&D3D11.Resource](depthBufferTexure), nil, &depthBufferDSV);


  self.defaultFramebuffer.numRTVs = 1;
  self.defaultFramebuffer.rtv[0] = backBufferRTV;
  self.defaultFramebuffer.dsv = depthBufferDSV;

  return true;


end

local terra Gfx_D3D11_CompileShader(shader : rawstring, profile : rawstring) : &D3D11.ID3DBlob

  var codeBlob : &D3D11.ID3DBlob;
  var errorBlob : &D3D11.ID3DBlob;

--  C.printf("%s:\n---\n%s\n---\n", profile, shader);

  var D3DCOMPILE_PACK_MATRIX_ROW_MAJOR = (1 << 3);

  var flags : D3D11.UINT = 0;
  flags = flags or D3DCOMPILE_PACK_MATRIX_ROW_MAJOR;

  var hr = D3DCompiler.D3DCompile(
    shader,
    C.strlen(shader),
    nil,
    nil,
    nil,
    "main",
    profile,
    flags,
    0,
    &codeBlob,
    &errorBlob);

  -- if errorBlob ~= nil then         -- Print errors and warnings
  if errorBlob ~= nil and hr < 0 then -- Only print errors (not warnings)
    C.printf("%s\n",
      errorBlob.lpVtbl.GetBufferPointer(errorBlob));

    C.printf("<<<\n");
    var c = shader;
    var l = 1;
    var startOfLine = true;
    while @c ~= 0 do
      if startOfLine then
        C.printf("%3d:", l);
        l = l + 1;
        startOfLine = false;
      end

      var cc = @c;
      c = c + 1;

      C.printf("%c", cc);

      startOfLine = (cc == ("\n")[0]);
    end
    C.printf(">>>\n");

    errorBlob.lpVtbl.Release(errorBlob);
  end

  return codeBlob;
end

local struct Gfx_D3D11_ShaderProgram
{
  vs : &D3D11.VertexShader;
  fs : &D3D11.PixelShader;
}
Gfx_D3D11.ShaderProgram = &Gfx_D3D11_ShaderProgram;

terra Gfx_D3D11_Device:createShaderProgram(vshader : rawstring, fshader : rawstring) : Gfx_D3D11.ShaderProgram

  var vsBlob = Gfx_D3D11_CompileShader(vshader, "vs_5_0");
  var fsBlob = Gfx_D3D11_CompileShader(fshader, "ps_5_0");

  var vs : &D3D11.VertexShader;
  var fs : &D3D11.PixelShader;

  var dxDevice = self.dxDevice;

  var hr : D3D11.HRESULT;

  hr = dxDevice.lpVtbl.CreateVertexShader(
    dxDevice,
    vsBlob.lpVtbl.GetBufferPointer(vsBlob),
    vsBlob.lpVtbl.GetBufferSize(vsBlob),
    nil,
    &vs);
  checkResult(hr);

  hr = dxDevice.lpVtbl.CreatePixelShader(
    dxDevice,
    fsBlob.lpVtbl.GetBufferPointer(fsBlob),
    fsBlob.lpVtbl.GetBufferSize(fsBlob),
    nil,
    &fs);
  checkResult(hr);
  

  var program = [&Gfx_D3D11_ShaderProgram](C.malloc([terralib.sizeof(Gfx_D3D11_ShaderProgram)]));
  program.vs = vs;
  program.fs = fs;
  return program;
end

local struct Gfx_D3D11_ComputeShaderProgram
{
  cs : &D3D11.ComputeShader;
}
Gfx_D3D11.ComputeShaderProgram = &Gfx_D3D11_ComputeShaderProgram;

terra Gfx_D3D11_Device:createComputeProgram(shader : rawstring) : Gfx_D3D11.ComputeShaderProgram
  var csBlob = Gfx_D3D11_CompileShader(shader, "cs_5_0");

  var cs : &D3D11.ComputeShader;

  var dxDevice = self.dxDevice;

  var hr : D3D11.HRESULT;

  hr = dxDevice.lpVtbl.CreateComputeShader(
    dxDevice,
    csBlob.lpVtbl.GetBufferPointer(csBlob),
    csBlob.lpVtbl.GetBufferSize(csBlob),
    nil,
    &cs);
  checkResult(hr);

  var program = [&Gfx_D3D11_ComputeShaderProgram](C.malloc([terralib.sizeof(Gfx_D3D11_ComputeShaderProgram)]));
  program.cs = cs;
  return program
end

Gfx_D3D11_Device.methods.deleteShaderProgram = terralib.overloadedfunction("Gfx_D3D11_Device:deleteShaderProgram")
Gfx_D3D11_Device.methods.deleteShaderProgram:adddefinition(terra( self : &Gfx_D3D11_Device, p : Gfx_D3D11.ShaderProgram )
  if p ~= nil then
    var vs = p.vs;
    var fs = p.fs;

    vs.lpVtbl.Release(vs);
    fs.lpVtbl.Release(fs);
    C.free(p);
  end
end)

Gfx_D3D11_Device.methods.deleteShaderProgram:adddefinition(terra( self : &Gfx_D3D11_Device, p : Gfx_D3D11.ComputeShaderProgram )
  if p ~= nil then
    var cs = p.cs;

    cs.lpVtbl.Release(cs);
    C.free(p);
  end
end)

terra Gfx_D3D11_Device:getImmediateContext() : &Gfx_D3D11.Context
  return &self.context;
end


local struct Gfx_D3D11_BufferBuilder
{
  device : &Gfx_D3D11_Device;
  desc : D3D11.BUFFER_DESC;
  initData : D3D11.SUBRESOURCE_DATA;
}

terra Gfx_D3D11_BufferBuilder:init( device : &Gfx_D3D11_Device )
  self.device = device;
  self.initData.pSysMem = nil;
  self.desc.ByteWidth = 0;
  self.desc.Usage = D3D11.USAGE_DEFAULT;
  self.desc.BindFlags = 0;
  self.desc.CPUAccessFlags = 0;
  self.desc.MiscFlags = 0;
  self.desc.StructureByteStride = 0;
end

terra Gfx_D3D11_Device:beginBuildBuffer() : Gfx_D3D11_BufferBuilder
  var builder : Gfx_D3D11_BufferBuilder;
  builder:init(self);
  return builder;
end

terra Gfx_D3D11_BufferBuilder:setSize( size : D3D11.UINT )
  self.desc.ByteWidth = size;
  return self;
end

terra Gfx_D3D11_BufferBuilder:setInitData( data : D3D11.PVOID )
  self.initData.pSysMem = data;
  return self;
end

terra Gfx_D3D11_BufferBuilder:setUsage( usage : D3D11.USAGE )
  self.desc.Usage = usage;
  return self;
end

terra Gfx_D3D11_BufferBuilder:setCPUAccess( access : D3D11.UINT )
  self.desc.CPUAccessFlags = access;
  return self;
end

terra Gfx_D3D11_BufferBuilder:setBindUniformBuffer()
  self.desc.BindFlags = self.desc.BindFlags or D3D11.BIND_CONSTANT_BUFFER;
  return self;
end

terra Gfx_D3D11_BufferBuilder:setBindVertexBuffer()
  self.desc.BindFlags = self.desc.BindFlags or D3D11.BIND_VERTEX_BUFFER;
  return self;
end

terra Gfx_D3D11_BufferBuilder:setBindIndexBuffer()
  self.desc.BindFlags = self.desc.BindFlags or D3D11.BIND_INDEX_BUFFER;
  return self;
end

terra Gfx_D3D11_BufferBuilder:setBindRWStructuredBuffer(byteStride : uint)
  self.desc.BindFlags = self.desc.BindFlags or D3D11.BIND_UNORDERED_ACCESS;
  self.desc.MiscFlags = D3D11.RESOURCE_MISC_BUFFER_STRUCTURED;
  self.desc.StructureByteStride = byteStride
  return self;
end

terra Gfx_D3D11_BufferBuilder:setBindFlags(b : D3D11.BIND_FLAG)
  self.desc.BindFlags = b
  return self
end

terra Gfx_D3D11_BufferBuilder:endBuild() : Gfx_D3D11.Buffer

  var dxDevice = self.device.dxDevice;
  var dxBuffer : &D3D11.Buffer = nil;
  var hr = dxDevice.lpVtbl.CreateBuffer(
    dxDevice,
    &self.desc,
    terralib.select(self.initData.pSysMem ~= nil, &self.initData, nil),
    &dxBuffer);

  checkResult(hr);

  return dxBuffer;
end

--

local Gfx_D3D11_MaxAttributes = 16; -- TODO: get correct maximum
Gfx_D3D11.MaxVertexStreams = Gfx_D3D11_MaxAttributes;

local struct Gfx_D3D11_VertexLayoutDesc
{
  device : &Gfx_D3D11.Device;
  attributes : D3D11.INPUT_ELEMENT_DESC[Gfx_D3D11_MaxAttributes];
  attributeCount : D3D11.UINT;
}
Gfx_D3D11.VertexLayoutDesc = Gfx_D3D11_VertexLayoutDesc;

terra Gfx_D3D11_VertexLayoutDesc:init( device : &Gfx_D3D11_Device )
  self.device = device;
  self.attributeCount   = 0;
end

terra Gfx_D3D11_Device:beginBuildVertexLayout() : Gfx_D3D11_VertexLayoutDesc
  var builder : Gfx_D3D11_VertexLayoutDesc;
  builder:init(self);
  return builder;
end

terra Gfx_D3D11_VertexLayoutDesc:addAttribute( format : int, streamIndex : int, offset : D3D11.UINT )

  var index = self.attributeCount;
  self.attributeCount = self.attributeCount + 1;

  self.attributes[index].SemanticName = "A";
  self.attributes[index].SemanticIndex = index;
  self.attributes[index].Format = format;
  self.attributes[index].InputSlot = streamIndex;
  self.attributes[index].AlignedByteOffset = offset;
  self.attributes[index].InputSlotClass = D3D11.INPUT_PER_VERTEX_DATA;
  self.attributes[index].InstanceDataStepRate = 0;

  return self;
end

local struct Writer
{
  cursor : rawstring;
  begin : rawstring;
  limit : rawstring;
}

terra Writer:init()
  self.cursor = nil;
  self.begin = nil;
  self.limit = nil;
end

terra Writer:deinit()
  C.free(self.begin);
end

terra Writer:writeData( data : rawstring, len : int )
  var cursor = self.cursor;
  var limit = self.limit;
  while cursor + len >= limit do
    var oldBegin = self.begin;
    var oldSize = limit - oldBegin;
    var newSize = terralib.select(oldSize == 0, 16, oldSize*2);

    var newBegin = [rawstring](C.realloc(oldBegin, newSize));
    cursor = newBegin + (cursor - oldBegin);
    limit = newBegin + newSize;

    self.begin = newBegin;
    self.limit = limit;
  end
  C.memcpy(cursor, data, len);
  cursor = cursor + len;
  self.cursor = cursor;
end

terra Writer:writeByte( v : int8 )
  self:writeData( &v, 1 );
end

terra Writer:write( t : rawstring )
  self:writeData( t, C.strlen(t) );
end

terra Writer:writeInt( v : int )
  var buffer : int8[32];
  C.sprintf(buffer, "%d", v);
  self:write(buffer);
end

terra Writer:get()
  self:writeByte(0);
  self.cursor = self.cursor - 1;
  return self.begin;
end


terra Gfx_D3D11_VertexLayoutDesc:endBuild() : Gfx_D3D11.VertexLayout

  var dxDevice = self.device.dxDevice;
  var dxLayout : &D3D11.InputLayout = nil;

  -- generate VS text for this layout
  var w : Writer;
  w:init();

  w:write("struct VS_INPUT {\n");
  for i = 0, self.attributeCount do
    w:write("    ");

    -- type for format
    var format = self.attributes[i].Format;
    if format == D3D11.FORMAT_R32G32B32A32_FLOAT then
      w:write("float4")
    elseif format == D3D11.FORMAT_R32G32B32_FLOAT then
      w:write("float3")
    elseif format == D3D11.FORMAT_R32G32_FLOAT then
      w:write("float2")
    elseif format == D3D11.FORMAT_R32_FLOAT then
      w:write("float")
    elseif format == D3D11.FORMAT_R8G8B8A8_UNORM then
      w:write("float4")
    else
      w:write("UnknownType");
    end

    w:write(" a");
    w:writeInt(i);
    w:write(" : A");
    w:writeInt(i);
    w:write(";\n");
  end
  w:write("};\n");
  w:write("float4 main( VS_INPUT input ) : SV_Position { return 0.0; }\n");

  var vsBlob = Gfx_D3D11_CompileShader(w:get(), "vs_5_0");

  w:deinit();

  var hr = dxDevice.lpVtbl.CreateInputLayout(
    dxDevice,
    &self.attributes[0],
    self.attributeCount,
    vsBlob.lpVtbl.GetBufferPointer(vsBlob),
    vsBlob.lpVtbl.GetBufferSize(vsBlob),
    &dxLayout);
  checkResult(hr);

  vsBlob.lpVtbl.Release(vsBlob);

  return dxLayout;
end

--

terra Gfx_D3D11_Context:clearFramebuffer( c : vec4, d : float, s : uint32, flags : uint )

  var dxContext = self.dxContext;
  var fb = self.framebuffer;

  if fb == nil then
    return;
  end

  if (flags and Gfx_D3D11.CLEAR_COLOR) ~= 0 then
    for i=0, fb.numRTVs do
      dxContext.lpVtbl.ClearRenderTargetView(
        dxContext,
        fb.rtv[i],
        [&float](&c));
    end
  end

  var depthStencilFlags = flags and (Gfx_D3D11.CLEAR_DEPTH or Gfx_D3D11.CLEAR_STENCIL);
  if fb.dsv ~= nil and depthStencilFlags ~= 0 then
    dxContext.lpVtbl.ClearDepthStencilView(
      dxContext,
      fb.dsv,
      depthStencilFlags,
      d,
      s);
  end
end

terra Gfx_D3D11_Context:setViewport( x0 : int, y0 : int, xExtent : int, yExtent : int )

  var vp : D3D11.VIEWPORT;
  vp.TopLeftX = x0;
  vp.TopLeftY = y0;
  vp.Width = xExtent;
  vp.Height = yExtent;
  vp.MinDepth = 0.0;
  vp.MaxDepth = 1.0;

  var dxContext = self.dxContext;
  dxContext.lpVtbl.RSSetViewports(
    dxContext,
    1,
    &vp);
end

terra Gfx_D3D11_Context:setDepthStencilState( s : Gfx_D3D11.DepthStencilState )
  var dxContext = self.dxContext;
  dxContext.lpVtbl.OMSetDepthStencilState(
    dxContext,
    s,
    0);
end

terra Gfx_D3D11_Context:setRasterizerState( s : Gfx_D3D11.RasterizerState )
  var dxContext = self.dxContext;
  dxContext.lpVtbl.RSSetState(
    dxContext,
    s);
end

Gfx_D3D11_Context.methods.setShaderProgram = terralib.overloadedfunction("Gfx_D3D11_Context:setShaderProgram")
Gfx_D3D11_Context.methods.setShaderProgram:adddefinition( terra ( self : &Gfx_D3D11_Context, program : &Gfx_D3D11_ShaderProgram )
  var dxContext = self.dxContext;
  dxContext.lpVtbl.VSSetShader(dxContext, program.vs, nil, 0);
  dxContext.lpVtbl.PSSetShader(dxContext, program.fs, nil, 0);
end)

Gfx_D3D11_Context.methods.setShaderProgram:adddefinition( terra( self : &Gfx_D3D11_Context, program : &Gfx_D3D11_ComputeShaderProgram )
  var dxContext = self.dxContext;
  dxContext.lpVtbl.CSSetShader(dxContext, program.cs, nil, 0);
end)

--terra Gfx_D3D11_Context:setVertexInput( vaoID : D3D11.GLuint )
--  D3D11.glBindVertexArray(vaoID);
--end

terra Gfx_D3D11_Context:setVertexLayout( layout : Gfx_D3D11.VertexLayout )
  var dxContext = self.dxContext;
  dxContext.lpVtbl.IASetInputLayout(dxContext, layout);
end

terra Gfx_D3D11_Context:setVertexBuffer( index : int, buffer : &D3D11.Buffer, stride : D3D11.UINT, offset : D3D11.UINT )
  var dxContext = self.dxContext;
  dxContext.lpVtbl.IASetVertexBuffers(dxContext, index, 1, &buffer, &stride, &offset);
end

terra Gfx_D3D11_Context:setIndexBuffer( buffer : &D3D11.Buffer, format : D3D11.DXGI_FORMAT )
  var dxContext = self.dxContext;
  dxContext.lpVtbl.IASetIndexBuffer(dxContext, buffer, format, 0);
end

terra Gfx_D3D11_Context:setTextureSampler( index : int, type: int, texture : &Gfx_D3D11_Texture, sampler : &D3D11.SamplerState )


  var dxContext = self.dxContext;

  dxContext.lpVtbl.VSSetShaderResources(dxContext, index, 1, &texture.srv);
  dxContext.lpVtbl.PSSetShaderResources(dxContext, index, 1, &texture.srv);
  dxContext.lpVtbl.CSSetShaderResources(dxContext, index, 1, &texture.srv);

  dxContext.lpVtbl.VSSetSamplers(dxContext, index, 1, &sampler);
  dxContext.lpVtbl.PSSetSamplers(dxContext, index, 1, &sampler);
  dxContext.lpVtbl.CSSetSamplers(dxContext, index, 1, &sampler);
end

terra Gfx_D3D11_Context:setTextureImage( index : int, texture : &Gfx_D3D11_Texture,
                                         access : Gfx_D3D11.MapModeT, format : Gfx_D3D11.TextureFormatT)
  var dxContext = self.dxContext;

  dxContext.lpVtbl.CSSetUnorderedAccessViews(dxContext, index, 1, &texture.uav, nil)
end

terra Gfx_D3D11_Context:setUniformBuffer( index : int, buffer : &D3D11.Buffer )
  var dxContext = self.dxContext;
  dxContext.lpVtbl.VSSetConstantBuffers(dxContext, index, 1, &buffer);
  dxContext.lpVtbl.PSSetConstantBuffers(dxContext, index, 1, &buffer);
  dxContext.lpVtbl.CSSetConstantBuffers(dxContext, index, 1, &buffer);
end

terra Gfx_D3D11_Context:setScissorRect( x0 : int, y0 : int, x1 : int, y1 : int, w : int, h : int)

  var r : D3D11.RECT;
  r.left = x0;
  r.top = y0;
  r.right = x1;
  r.bottom = y1;

  var dxContext = self.dxContext;
  dxContext.lpVtbl.RSSetScissorRects(dxContext, 1, &r);
end

Gfx_D3D11_Context.methods.drawIndexed = terralib.overloadedfunction("Gfx_D3D11_Context:drawIndexed")
Gfx_D3D11_Context.methods.drawIndexed:adddefinition(
  terra( self : &Gfx_D3D11_Context, primitiveType : D3D11.PRIMITIVE_TOPOLOGY, vertCount : int )

  var dxContext = self.dxContext;
  dxContext.lpVtbl.IASetPrimitiveTopology(dxContext, primitiveType);
  dxContext.lpVtbl.DrawIndexed(dxContext, vertCount, 0, 0);
end)

Gfx_D3D11_Context.methods.drawIndexed:adddefinition(
  terra( self : &Gfx_D3D11_Context, primitiveType : D3D11.PRIMITIVE_TOPOLOGY,
    vertCount : int, baseIndex : int, baseVertex : int )

  var dxContext = self.dxContext;
  dxContext.lpVtbl.IASetPrimitiveTopology(dxContext, primitiveType);
  dxContext.lpVtbl.DrawIndexed(dxContext, vertCount, baseIndex, baseVertex);
end)


-- Compute shader dispatch
terra Gfx_D3D11_Context:dispatchCompute(x : uint, y : uint, z : uint)
  var dxContext = self.dxContext;
  dxContext.lpVtbl.Dispatch(dxContext, x, y, z);
end

local Gfx_D3D11_BarrierFlags = {};
Gfx_D3D11.BarrierFlags = Gfx_D3D11_BarrierFlags;

Gfx_D3D11_BarrierFlags.TEXTURE_FETCH = 0;
Gfx_D3D11_BarrierFlags.SHADER_IMAGE_ACCESS = 1;
Gfx_D3D11_BarrierFlags.SHADER_BUFFER = 2;

-- This is a bit of a hack.  OpenGL requires memory barriers to prevent
-- one shader from reading from a resource that is still being modified.
-- From what I understand, D3D11 has implicit synchronization between
-- shader dispatches.  However, a resource cannot be bound as both an UAV
-- and a SRV at the same time.  So we're using the memory barrier function
-- as a proxy for telling when to unbind different resources.
terra Gfx_D3D11_Context:memoryBarrier(barrier : uint)
  var dxContext = self.dxContext;
  if barrier == Gfx_D3D11_BarrierFlags.TEXTURE_FETCH
  or barrier == Gfx_D3D11_BarrierFlags.SHADER_BUFFER
  then
    var uavs = arrayof([&D3D11.UnorderedAccessView], nil, nil, nil, nil, nil, nil, nil, nil)
    dxContext.lpVtbl.CSSetUnorderedAccessViews(dxContext, 0, 8, &uavs[0], nil)
  elseif barrier == Gfx_D3D11_BarrierFlags.SHADER_IMAGE_ACCESS then
    var srvs = arrayof([&D3D11.ShaderResourceView], nil, nil, nil, nil, nil, nil, nil, nil)
    dxContext.lpVtbl.VSSetShaderResources(dxContext, 0, 8, &srvs[0]);
    dxContext.lpVtbl.PSSetShaderResources(dxContext, 0, 8, &srvs[0]);

    var rtvs = arrayof([&D3D11.RenderTargetView], nil, nil, nil, nil, nil, nil, nil, nil)
    var dsv : &D3D11.DepthStencilView = nil
    dxContext.lpVtbl.OMSetRenderTargets(dxContext, 8, rtvs, dsv);
  end
end

-- Debugging utilities
terra Gfx_D3D11_Device:setDebug(b: bool)
--  terralib.select(b, D3D11.glEnable, D3D11.glDisable)(D3D11.GL_DEBUG_OUTPUT);
end

--[=[

function Gfx_D3D11.codeGenFn(pipeline)
  -- TODO: proper code generation

  local vs = [[
  cbuffer Uniforms : register(b0) {
    float4x4 mvp;
  };
  float4 main( float2 p : A0 ) : SV_Position {
    return mul(mvp, float4(p, 0.0, 1.0));
  }
  ]];

  local fs = [[
  float4 main() : SV_Target {
    return float4(1,0,0,1);
  }
  ]];

  return { vertexSourceCode = vs, fragmentSourceCode = fs };
end
--]=]
Gfx_D3D11.codeGenFn = terraToHLSL;

local terra mapResource(ctxt : &Gfx_D3D11_Context, resource : &D3D11.Resource, mode : D3D11.MAP,
                  size : int, usage : D3D11.USAGE ) : &int8
  var dxContext = ctxt.dxContext;
  var dxMapped : D3D11.MAPPED_SUBRESOURCE;
  var hr = dxContext.lpVtbl.Map(dxContext, resource, 0, mode, 0, &dxMapped);
  checkResult(hr);

  return [&int8](dxMapped.pData);
end

local terra unmapResource(ctxt : &Gfx_D3D11_Context, resource : &D3D11.Resource)
  var dxContext = ctxt.dxContext;
  dxContext.lpVtbl.Unmap(dxContext, resource, 0)
end

terra Gfx_D3D11_Context:mapBuffer( buffer : &D3D11.Buffer, mode : D3D11.MAP,
                                   size : int, usage : D3D11.USAGE ) : &int8
  return mapResource(self, [&D3D11.Resource](buffer), mode, size, usage)
end

terra Gfx_D3D11_Context:unmapBuffer( buffer : &D3D11.Buffer )

  unmapResource(self, [&D3D11.Resource](buffer))
end

terra Gfx_D3D11_Device:copyTextureToHost( texture : &Gfx_D3D11_Texture, level : int,
                                          format : Gfx_D3D11.TextureFormatT,
                                          width : int, height : int, pixels : &vec4)
  var size = width * height * terralib.sizeof(vec4)

  var dst = self:beginBuildTexture2D(width, height)
    :setFormat(texture.format)
    :setUsage(D3D11.USAGE_STAGING)
    :setBindFlags(0)
    :setCPUAccess(D3D11.CPU_ACCESS_READ)
    :setMipLevelCount(1)
    :endBuild()

  var ctxt = &self.context
  var dxContext = ctxt.dxContext
  dxContext.lpVtbl.CopyResource(dxContext, dst.resource, texture.resource)

  var mapped = mapResource(ctxt, dst.resource, D3D11.MAP_READ, size, D3D11.USAGE_STAGING)
  C.memcpy(pixels, mapped, size)
  unmapResource(ctxt, dst.resource)

  self:deleteTexture(dst)
end

terra Gfx_D3D11_Device:updateTextureData( t : &Gfx_D3D11_Texture, format : Gfx_D3D11.TextureFormatT,
                                          width : int, height: int, level : int,
                                          data : &uint16, numComponents : int)
  var size = width * height * terralib.sizeof(uint16) * numComponents
  var ctxt = &self.context
  var dxContext = ctxt.dxContext

  var dst = self:beginBuildTexture2D(width, height)
    :setFormat(format)
    :setUsage(D3D11.USAGE_STAGING)
    :setBindFlags(0)
    :setCPUAccess(D3D11.CPU_ACCESS_READ or D3D11.CPU_ACCESS_WRITE)
    :setMipLevelCount(1)
    :setInitDataForLevel(level, data, size / height)
    :endBuild()

  dxContext.lpVtbl.CopyResource(dxContext, t.resource, dst.resource)

  self:deleteTexture(dst)
end


terra Gfx_D3D11_Device:deleteTexture( t : &Gfx_D3D11_Texture )
  if t ~= nil then
    var dxResource = t.resource;
    var dxSRV = t.srv;
    var dxUAV = t.uav;

    dxResource.lpVtbl.Release(dxResource);

    if dxSRV ~= nil then
      dxSRV.lpVtbl.Release(dxSRV);
    end

    if dxUAV ~= nil then
      dxUAV.lpVtbl.Release(dxUAV);
    end

    C.free(t);
  end    
end

terra Gfx_D3D11_Device:deleteBuffer( b : &D3D11.Buffer )
  b.lpVtbl.Release(b);
end

terra Gfx_D3D11_Context:setBlendState( s : &D3D11.BlendState )
  var dxContext = self.dxContext;

  var blendColor : float[4];
  var sampleMask : D3D11.UINT = 0xFFFFFFFF;

  dxContext.lpVtbl.OMSetBlendState(dxContext, s, &blendColor[0], sampleMask);
end

-- framebuffer

terra Gfx_D3D11_Device:getDefaultFramebuffer()
  return &self.defaultFramebuffer;
end

terra Gfx_D3D11_Device:generateFramebuffer()
  var result = [&Gfx_D3D11_Framebuffer](C.malloc([terralib.sizeof(Gfx_D3D11_Framebuffer)]));
  result.numRTVs = 0
  result.dsv = nil
  result.rtv[0] = nil
  return result
end

terra Gfx_D3D11_Context:setFramebuffer( f : &Gfx_D3D11_Framebuffer )
  self.framebuffer = f;
  var dxContext = self.dxContext;
  dxContext.lpVtbl.OMSetRenderTargets(
    dxContext,
    f.numRTVs,
    f.rtv,
    f.dsv);
end

terra Gfx_D3D11_Device:swap()
  var dxSwapChain = self.swapChain;
  dxSwapChain.lpVtbl.Present(dxSwapChain, 0, 0);
end


-- RenderTargetView and DepthStencilView
Gfx_D3D11.DepthStencilView = &D3D11.DepthStencilView;

terra Gfx_D3D11_Device:createRenderTargetView( texture : &Gfx_D3D11_Texture )
  var device = self.dxDevice;
  var rtv : &D3D11.RenderTargetView;

  var desc : D3D11.RENDER_TARGET_VIEW_DESC;
  desc.Format = texture.format;
  desc.ViewDimension = D3D11.RTV_DIMENSION_TEXTURE2D;
  desc._0.Texture2D.MipSlice = 0;

  var hr = device.lpVtbl.CreateRenderTargetView(device, [&D3D11.Resource](texture.resource),
                                                &desc, &rtv);
  return rtv
end

Gfx_D3D11_Device.methods.createDepthStencilView = terralib.overloadedfunction("createDepthStencilView",
{
  terra ( self : &Gfx_D3D11_Device, texture : &Gfx_D3D11_Texture )
    var device = self.dxDevice;
    var dsv : &D3D11.DepthStencilView;

    var desc : D3D11.DEPTH_STENCIL_VIEW_DESC;
    desc.Flags = 0;
    desc.Format = textureFormatToDSVFormat(texture.format);
    desc.ViewDimension = D3D11.DSV_DIMENSION_TEXTURE2D;
    desc._0.Texture2D.MipSlice = 0;

    var hr = device.lpVtbl.CreateDepthStencilView(device, [&D3D11.Resource](texture.resource),
                                                  &desc, &dsv);
    return dsv
  end
,
  terra ( self : &Gfx_D3D11_Device, texture : &Gfx_D3D11_Texture, face : int )
    var device = self.dxDevice;
    var dsv : &D3D11.DepthStencilView;

    var desc : D3D11.DEPTH_STENCIL_VIEW_DESC;
    desc.Flags = 0;
    desc.Format = textureFormatToDSVFormat(texture.format);
    desc.ViewDimension = D3D11.DSV_DIMENSION_TEXTURE2DARRAY;
    desc._0.Texture2DArray.MipSlice = 0;
    desc._0.Texture2DArray.ArraySize = 1;
    desc._0.Texture2DArray.FirstArraySlice = face;

    var hr = device.lpVtbl.CreateDepthStencilView(device, [&D3D11.Resource](texture.resource),
                                                  &desc, &dsv);
    return dsv
  end
})

terra Gfx_D3D11_Device:bindRenderTargetView( rtv : &D3D11.RenderTargetView, bindPoint : uint )
  var f = self.context.framebuffer
  f.rtv[bindPoint] = rtv

  if f.numRTVs < bindPoint+1 then
    f.numRTVs = bindPoint+1
  end

  self.context:setFramebuffer(f)
end

Gfx_D3D11_Device.methods.bindDepthStencilView = terralib.overloadedfunction("bindDepthStencilView",
{
  terra( self : &Gfx_D3D11_Device, dsv : &D3D11.DepthStencilView )
    var f = self.context.framebuffer
    f.dsv = dsv
    self.context:setFramebuffer(f)
  end
,
  terra( self : &Gfx_D3D11_Device, dsv : &D3D11.DepthStencilView, face : int )
    var f = self.context.framebuffer
    f.dsv = dsv
    self.context:setFramebuffer(f)
  end
})

terra Gfx_D3D11_Context:generateMipmap( tex : &Gfx_D3D11_Texture)
  var ctxt = self.dxContext;

  ctxt.lpVtbl.GenerateMips(ctxt, tex.srv);
end


-- Timer
local struct Gfx_D3D11_Timer {
  device      : &Gfx_D3D11_Device
  startQuery  : &D3D11.Query
  endQuery    : &D3D11.Query
  startTime   : uint64
  endTime     : uint64
  elapsedTime : float
  acc         : float
  average     : float
  count       : int
}
Gfx_D3D11.Timer = Gfx_D3D11_Timer

terra Gfx_D3D11_Timer:init(device : &Gfx_D3D11_Device)
  self.acc = 0
  self.average = 0
  self.count = 0
  self.device = device
  var dxDevice = device.dxDevice

  var desc : D3D11.QUERY_DESC
  desc.MiscFlags = 0

  desc.Query = D3D11.QUERY_TIMESTAMP
  dxDevice.lpVtbl.CreateQuery(dxDevice, &desc, &self.startQuery)
  dxDevice.lpVtbl.CreateQuery(dxDevice, &desc, &self.endQuery)
end

terra Gfx_D3D11_Timer:start()
  var ctxt = self.device:getImmediateContext()
  var dx = ctxt.dxContext
  dx.lpVtbl.End(dx, [&D3D11.Asynchronous](self.startQuery))
end

terra Gfx_D3D11_Timer:stop()
  var ctxt = self.device:getImmediateContext()
  var dx = ctxt.dxContext
  dx.lpVtbl.End(dx, [&D3D11.Asynchronous](self.endQuery))
end

terra Gfx_D3D11_Timer:accumulate()
  var ctxt = self.device:getImmediateContext()
  var dx = ctxt.dxContext

  if ctxt.disjointResult.Disjoint ~= D3D11.FALSE then
    C.printf("[Warning] Disjoint Timestamps - timer will not be updated")
    return
  end

  while dx.lpVtbl.GetData(dx, [&D3D11.Asynchronous](self.startQuery), &self.startTime,
    terralib.sizeof(uint64), 0) ~= 0 do
  end

  while dx.lpVtbl.GetData(dx, [&D3D11.Asynchronous](self.endQuery), &self.endTime,
    terralib.sizeof(uint64), 0) ~= 0 do
  end

  self.elapsedTime = [float](self.endTime - self.startTime) / [float](ctxt.disjointResult.Frequency) * 1000.0f
  self.acc = self.acc + self.elapsedTime
  self.count = self.count + 1
end

terra Gfx_D3D11_Timer:updateAverage(numFramesToAverage : int)
  if self.count < numFramesToAverage then
    return
  end
  self.average = self.acc / self.count
  self.acc = 0
  self.count = 0
end


terra Gfx_D3D11_Context:beginFrame()
  var dx = self.dxContext
  dx.lpVtbl.Begin(dx, [&D3D11.Asynchronous](self.disjointQuery))
end

terra Gfx_D3D11_Context:endFrame()
  var dx = self.dxContext

  dx.lpVtbl.End(dx, [&D3D11.Asynchronous](self.disjointQuery))

  while dx.lpVtbl.GetData(dx, [&D3D11.Asynchronous](self.disjointQuery), &self.disjointResult,
    terralib.sizeof(D3D11.QUERY_DATA_TIMESTAMP_DISJOINT), 0) ~= 0 do
  end
end


local struct Gfx_D3D11_RWStructuredBufferBase
{
  raw : &D3D11.Buffer;
  uav : &D3D11.UnorderedAccessView;
}
Gfx_D3D11.RWStructuredBufferBase = Gfx_D3D11_RWStructuredBufferBase

terra Gfx_D3D11_Context:setRWStructuredBuffer( index : int, buffer : Gfx_D3D11_RWStructuredBufferBase )
  var dxContext = self.dxContext;
  dxContext.lpVtbl.CSSetUnorderedAccessViews(dxContext, index, 1, &buffer.uav, nil)
end


function Gfx_D3D11_RWStructuredBuffer(T, numElems)
  local size = terralib.sizeof(T) * numElems

  local struct SB
  {
    raw : &D3D11.Buffer;
    uav : &D3D11.UnorderedAccessView;
  }

  terra SB:init( device : &Gfx_D3D11.Device )
    self.raw = device:beginBuildBuffer()
      :setSize(size)
      :setBindRWStructuredBuffer(terralib.sizeof(T))
      :endBuild();

    var uavDesc : D3D11.UNORDERED_ACCESS_VIEW_DESC;
    uavDesc.Format = D3D11.FORMAT_UNKNOWN;
    uavDesc.ViewDimension = D3D11.UAV_DIMENSION_BUFFER;
    uavDesc._0.Buffer.FirstElement = 0;
    uavDesc._0.Buffer.NumElements  = numElems;
    uavDesc._0.Buffer.Flags = 0;

    -- TODO error checking
    var hr = device.dxDevice.lpVtbl.CreateUnorderedAccessView(
      device.dxDevice,
      [&D3D11.Resource](self.raw),
      &uavDesc,
      &self.uav)
  end

  terra SB:replaceWith(device : &Gfx_D3D11.Device, other : &SB)
    if self.uav ~= nil then
      self.uav.lpVtbl.Release(self.uav);
    end
    if self.raw ~= nil then
      device:deleteBuffer(self.raw)
    end

    self.raw = other.raw
    self.uav = other.uav
  end

  terra SB:copyToHost(device : &Gfx_D3D11.Device)
    var dst = device:beginBuildBuffer()
      :setSize(size)
      :setBindRWStructuredBuffer(terralib.sizeof(T))
      :setUsage(D3D11.USAGE_STAGING)
      :setBindFlags(0)
      :setCPUAccess(D3D11.CPU_ACCESS_READ)
      :endBuild();

    var ctxt = &device.context
    var dxContext = ctxt.dxContext
    dxContext.lpVtbl.CopyResource(dxContext, [&D3D11.Resource](dst), [&D3D11.Resource](self.raw))
    var mapped = mapResource(ctxt, [&D3D11.Resource](dst), D3D11.MAP_READ, size, D3D11.USAGE_STAGING)

    var ret = [&T](C.malloc(size))
    C.memcpy(ret, mapped, size)

    unmapResource(ctxt, [&D3D11.Resource](dst))
    device:deleteBuffer(dst)

    return ret
  end

  return SB;
end
Gfx_D3D11.RWStructuredBuffer = terralib.memoize(Gfx_D3D11_RWStructuredBuffer)


return Gfx_D3D11;
