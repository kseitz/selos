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
-- Modified from Falcor/Framework/Source/Utils/DDSHeader.h
--               Falcor/Framework/Source/Utils/DXHeader.h
--               Falcor/Framework/Source/Graphics/TextureHelper.cpp
-- available at https://github.com/NVIDIAGameWorks/Falcor/tree/3.2.1/
--
--
-- License of original files:
----------------------------------------------------------------------------
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
----------------------------------------------------------------------------

local C = terralib.includecstring([[
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
]])

local Gfx = require("gfx")

local kDdsMagicNumber = constant(uint32, 0x20534444)

local DXResourceDimension = {
  RESOURCE_DIMENSION_UNKNOWN   = 0,
  RESOURCE_DIMENSION_BUFFER    = 1,
  RESOURCE_DIMENSION_TEXTURE1D = 2,
  RESOURCE_DIMENSION_TEXTURE2D = 3,
  RESOURCE_DIMENSION_TEXTURE3D = 4
}

local DXFormat = {
  FORMAT_UNKNOWN                     = 0,
  FORMAT_R32G32B32A32_TYPELESS       = 1,
  FORMAT_R32G32B32A32_FLOAT          = 2,
  FORMAT_R32G32B32A32_UINT           = 3,
  FORMAT_R32G32B32A32_SINT           = 4,
  FORMAT_R32G32B32_TYPELESS          = 5,
  FORMAT_R32G32B32_FLOAT             = 6,
  FORMAT_R32G32B32_UINT              = 7,
  FORMAT_R32G32B32_SINT              = 8,
  FORMAT_R16G16B16A16_TYPELESS       = 9,
  FORMAT_R16G16B16A16_FLOAT          = 10,
  FORMAT_R16G16B16A16_UNORM          = 11,
  FORMAT_R16G16B16A16_UINT           = 12,
  FORMAT_R16G16B16A16_SNORM          = 13,
  FORMAT_R16G16B16A16_SINT           = 14,
  FORMAT_R32G32_TYPELESS             = 15,
  FORMAT_R32G32_FLOAT                = 16,
  FORMAT_R32G32_UINT                 = 17,
  FORMAT_R32G32_SINT                 = 18,
  FORMAT_R32G8X24_TYPELESS           = 19,
  FORMAT_D32_FLOAT_S8X24_UINT        = 20,
  FORMAT_R32_FLOAT_X8X24_TYPELESS    = 21,
  FORMAT_X32_TYPELESS_G8X24_UINT     = 22,
  FORMAT_R10G10B10A2_TYPELESS        = 23,
  FORMAT_R10G10B10A2_UNORM           = 24,
  FORMAT_R10G10B10A2_UINT            = 25,
  FORMAT_R11G11B10_FLOAT             = 26,
  FORMAT_R8G8B8A8_TYPELESS           = 27,
  FORMAT_R8G8B8A8_UNORM              = 28,
  FORMAT_R8G8B8A8_UNORM_SRGB         = 29,
  FORMAT_R8G8B8A8_UINT               = 30,
  FORMAT_R8G8B8A8_SNORM              = 31,
  FORMAT_R8G8B8A8_SINT               = 32,
  FORMAT_R16G16_TYPELESS             = 33,
  FORMAT_R16G16_FLOAT                = 34,
  FORMAT_R16G16_UNORM                = 35,
  FORMAT_R16G16_UINT                 = 36,
  FORMAT_R16G16_SNORM                = 37,
  FORMAT_R16G16_SINT                 = 38,
  FORMAT_R32_TYPELESS                = 39,
  FORMAT_D32_FLOAT                   = 40,
  FORMAT_R32_FLOAT                   = 41,
  FORMAT_R32_UINT                    = 42,
  FORMAT_R32_SINT                    = 43,
  FORMAT_R24G8_TYPELESS              = 44,
  FORMAT_D24_UNORM_S8_UINT           = 45,
  FORMAT_R24_UNORM_X8_TYPELESS       = 46,
  FORMAT_X24_TYPELESS_G8_UINT        = 47,
  FORMAT_R8G8_TYPELESS               = 48,
  FORMAT_R8G8_UNORM                  = 49,
  FORMAT_R8G8_UINT                   = 50,
  FORMAT_R8G8_SNORM                  = 51,
  FORMAT_R8G8_SINT                   = 52,
  FORMAT_R16_TYPELESS                = 53,
  FORMAT_R16_FLOAT                   = 54,
  FORMAT_D16_UNORM                   = 55,
  FORMAT_R16_UNORM                   = 56,
  FORMAT_R16_UINT                    = 57,
  FORMAT_R16_SNORM                   = 58,
  FORMAT_R16_SINT                    = 59,
  FORMAT_R8_TYPELESS                 = 60,
  FORMAT_R8_UNORM                    = 61,
  FORMAT_R8_UINT                     = 62,
  FORMAT_R8_SNORM                    = 63,
  FORMAT_R8_SINT                     = 64,
  FORMAT_A8_UNORM                    = 65,
  FORMAT_R1_UNORM                    = 66,
  FORMAT_R9G9B9E5_SHAREDEXP          = 67,
  FORMAT_R8G8_B8G8_UNORM             = 68,
  FORMAT_G8R8_G8B8_UNORM             = 69,
  FORMAT_BC1_TYPELESS                = 70,
  FORMAT_BC1_UNORM                   = 71,
  FORMAT_BC1_UNORM_SRGB              = 72,
  FORMAT_BC2_TYPELESS                = 73,
  FORMAT_BC2_UNORM                   = 74,
  FORMAT_BC2_UNORM_SRGB              = 75,
  FORMAT_BC3_TYPELESS                = 76,
  FORMAT_BC3_UNORM                   = 77,
  FORMAT_BC3_UNORM_SRGB              = 78,
  FORMAT_BC4_TYPELESS                = 79,
  FORMAT_BC4_UNORM                   = 80,
  FORMAT_BC4_SNORM                   = 81,
  FORMAT_BC5_TYPELESS                = 82,
  FORMAT_BC5_UNORM                   = 83,
  FORMAT_BC5_SNORM                   = 84,
  FORMAT_B5G6R5_UNORM                = 85,
  FORMAT_B5G5R5A1_UNORM              = 86,
  FORMAT_B8G8R8A8_UNORM              = 87,
  FORMAT_B8G8R8X8_UNORM              = 88,
  FORMAT_R10G10B10_XR_BIAS_A2_UNORM  = 89,
  FORMAT_B8G8R8A8_TYPELESS           = 90,
  FORMAT_B8G8R8A8_UNORM_SRGB         = 91,
  FORMAT_B8G8R8X8_TYPELESS           = 92,
  FORMAT_B8G8R8X8_UNORM_SRGB         = 93,
  FORMAT_BC6H_TYPELESS               = 94,
  FORMAT_BC6H_UF16                   = 95,
  FORMAT_BC6H_SF16                   = 96,
  FORMAT_BC7_TYPELESS                = 97,
  FORMAT_BC7_UNORM                   = 98,
  FORMAT_BC7_UNORM_SRGB              = 99,
  FORMAT_AYUV                        = 100,
  FORMAT_Y410                        = 101,
  FORMAT_Y416                        = 102,
  FORMAT_NV12                        = 103,
  FORMAT_P010                        = 104,
  FORMAT_P016                        = 105,
  FORMAT_420_OPAQUE                  = 106,
  FORMAT_YUY2                        = 107,
  FORMAT_Y210                        = 108,
  FORMAT_Y216                        = 109,
  FORMAT_NV11                        = 110,
  FORMAT_AI44                        = 111,
  FORMAT_IA44                        = 112,
  FORMAT_P8                          = 113,
  FORMAT_A8P8                        = 114,
  FORMAT_B4G4R4A4_UNORM              = 115,
  FORMAT_P208                        = 130,
  FORMAT_V208                        = 131,
  FORMAT_V408                        = 132,
  FORMAT_FORCE_UINT                  = 0xffffffff
}

-- Flags
local kAlphaPixelsMask  = constant(uint32, 0x1)
local kAlphaMask        = constant(uint32, 0x2)
local kFourCCFlag       = constant(uint32, 0x4)
local kRgbMask          = constant(uint32, 0x40)
local kYuvMask          = constant(uint32, 0x200)
local kLuminanceMask    = constant(uint32, 0x20000)
local kBumpMask         = constant(uint32, 0x00080000)

local kCapsMask         = constant(uint32, 0x1)
local kHeightMask       = constant(uint32, 0x2)
local kWidthMask        = constant(uint32, 0x4)
local kPitchMask        = constant(uint32, 0x8)
local kPixelFormatMask  = constant(uint32, 0x1000)
local kMipCountMask     = constant(uint32, 0x20000)
local kLinearSizeMask   = constant(uint32, 0x80000)
local kDepthMask        = constant(uint32, 0x800000)

-- Caps[0]
local kCapsComplexMask  = constant(uint32, 0x8)
local kCapsMipMapMask   = constant(uint32, 0x400000)
local kCapsTextureMask  = constant(uint32, 0x1000)

-- Caps[1]
local kCaps2CubeMapMask     = constant(uint32, 0x200)
local kCaps2CubeMapPosXMask = constant(uint32, 0x400)
local kCaps2CubeMapNegXMask = constant(uint32, 0x800)
local kCaps2CubeMapPosYMask = constant(uint32, 0x1000)
local kCaps2CubeMapNegYMask = constant(uint32, 0x2000)
local kCaps2CubeMapPosZMask = constant(uint32, 0x4000)
local kCaps2CubeMapNegZMask = constant(uint32, 0x8000)
local kCaps2VolumeMask      = constant(uint32, 0x200000)

local kCubeMapMask = constant(uint32, 0x4)


local struct PixelFormat
{
  structSize  : uint32
  flags       : uint32
  fourCC      : uint32
  bitcount    : uint32
  rMask       : uint32
  gMask       : uint32
  bMask       : uint32
  aMask       : uint32
};

local struct DdsHeader {
  headerSize    : uint32
  flags         : uint32
  height        : uint32
  width         : uint32
  union
  {
    pitch       : uint32
    linearSize  : uint32
  }

  depth         : uint32
  mipCount      : uint32
  reserved      : uint32[11]
  pixelFormat   : PixelFormat
  caps          : uint32[4]
  reserved2     : uint32
};

struct DdsHeaderDX10
{
  dxgiFormat        : uint32  -- DXFormat
  resourceDimension : uint32  -- DXResourceDimension
  miscFlag          : uint32
  arraySize         : uint32
  miscFlags2        : uint32
};

struct DdsData
{
  header        : DdsHeader 
  dx10Header    : DdsHeaderDX10 
  hasDX10Header : bool 
  data          : &uint8
};

local terra makeFourCC(name : &int8)
  var fourCC : uint32 = 0;
  for i = 0, 4 do
    var shift : uint32 = i * 8;
    fourCC = fourCC or ([uint32](name[i])) << shift;
  end
  return fourCC;
end

local terra getDxgiFormatFrom4CC(fourCC : uint32)
  var errMsg : rawstring

  if fourCC == makeFourCC("DXT1") then
    return Gfx.TextureFormat.BC1_UNORM
  elseif fourCC == makeFourCC("DXT2") then
    errMsg = "DXT2"
  elseif fourCC == makeFourCC("DXT3") then
    return Gfx.TextureFormat.BC2_UNORM
  elseif fourCC == makeFourCC("DXT4") then
    errMsg = "DXT4"
  elseif fourCC == makeFourCC("DXT5") then
    return Gfx.TextureFormat.BC3_UNORM

  elseif fourCC == makeFourCC("ATI1") then
    errMsg = "FORMAT_BC4_UNORM"
  elseif fourCC == makeFourCC("BC4U") then
    errMsg = "FORMAT_BC4_UNORM"
  elseif fourCC == makeFourCC("BC4S") then
    errMsg = "FORMAT_BC4_SNORM"

  elseif fourCC == makeFourCC("ATI2") then
    return Gfx.TextureFormat.BC5_UNORM
  elseif fourCC == makeFourCC("BC5U") then
    return Gfx.TextureFormat.BC5_UNORM
  elseif fourCC == makeFourCC("BC5S") then
    return Gfx.TextureFormat.BC5_SNORM

  elseif fourCC == makeFourCC("RGBG") then
    errMsg = "FORMAT_R8G8_B8G8_UNORM"
  elseif fourCC == makeFourCC("GRGB") then
    errMsg = "FORMAT_G8R8_G8B8_UNORM"

  elseif fourCC == makeFourCC("YUY2") then
    errMsg = "FORMAT_YUY2"
  end

  if fourCC == 36 then
    errMsg = "FORMAT_R16G16B16A16_UNORM"
  elseif fourCC == 110 then
    errMsg = "FORMAT_R16G16B16A16_SNORM"
  elseif fourCC == 111 then
    errMsg = "FORMAT_R16_FLOAT"
  elseif fourCC == 112 then
    errMsg = "FORMAT_R16G16_FLOAT"
  elseif fourCC == 113 then
    errMsg = "FORMAT_R16G16B16A16_FLOAT"
  elseif fourCC == 114 then
    errMsg = "FORMAT_R32_FLOAT"
  elseif fourCC == 115 then
    errMsg = "FORMAT_R32G32_FLOAT"
  elseif fourCC == 116 then
    errMsg = "FORMAT_R32G32B32A32_FLOAT"
  end

  C.printf("[Error] Unsupported format %s for dds texture\n", errMsg)
  return Gfx.TextureFormat.UNKNOWN
end

local terra getDDSFormat(ddsData : &DdsData)
  var pixelFlags = ddsData.header.pixelFormat.flags

  if (pixelFlags and kAlphaPixelsMask) > 0 then
    C.printf("[Warning] Unsupported dds format: alpha pixels\n")
  elseif (pixelFlags and kAlphaMask) > 0 then
    C.printf("[Warning] Unsupported dds format: alpha\n")
  elseif (pixelFlags and kFourCCFlag) > 0 then
    return getDxgiFormatFrom4CC(ddsData.header.pixelFormat.fourCC)
  elseif (pixelFlags and kRgbMask) > 0 then
    C.printf("[Warning] Unsupported dds format: rgb\n")
  elseif (pixelFlags and kYuvMask) > 0 then
    C.printf("[Warning] Unsupported dds format: yuv\n")
  elseif (pixelFlags and kLuminanceMask) > 0 then
    C.printf("[Warning] Unsupported dds format: luminance\n")
  elseif (pixelFlags and kBumpMask) > 0 then
    C.printf("[Warning] Unsupported dds format: bump\n")
  end

  return Gfx.TextureFormat.UNKNOWN
end


local terra loadDDSTextureData( path : rawstring, ddsData : &DdsData )
  var file = C.fopen(path, "rb")
  if file == nil then
    C.printf("Error: Unable to open dds file '%s'\n", path)
    return false
  end

  C.fseek(file, 0, C.SEEK_END);
  var size = C.ftell(file);
  C.fseek(file, 0, C.SEEK_SET);

  var ddsIdentifier : uint32
  C.fread(&ddsIdentifier, terralib.sizeof(uint32), 1, file)
  if ddsIdentifier ~= kDdsMagicNumber then
    C.printf("[Error] Invalid dds file: %s\n", path)
    C.fclose(file)
    return false
  end

  C.fread(&ddsData.header, terralib.sizeof(DdsHeader), 1, file)

  if ((ddsData.header.pixelFormat.flags and kFourCCFlag) > 0) and
     (makeFourCC("DX10") == ddsData.header.pixelFormat.fourCC) then
    ddsData.hasDX10Header = true
    C.fread(&ddsData.dx10Header, terralib.sizeof(DdsHeaderDX10), 1, file)
    C.printf("[Warning] dds files with DX10 headers are currently unsupported: %s\n", path)
    C.fclose(file)
    return false
  else
    ddsData.hasDX10Header = false
  end

  var remainingSize = size - C.ftell(file)
  ddsData.data = [&uint8](C.malloc(remainingSize));
  if ddsData.data == nil then
    C.printf("Error: Unable to allocate read buffer for dds file '%s'\n", path)
    C.fclose(file)
    return false
  end

  C.fread(ddsData.data, 1, remainingSize, file)
  C.fclose(file)

  if (ddsData.header.flags and kDepthMask) > 0 then
    C.printf("[Warning] Volume / 3D textures are currently unsupported: %s\n", path)
    C.free(ddsData.data)
    return false
  elseif (ddsData.header.caps[1] and kCaps2CubeMapMask) > 0 then
    C.printf("[Warning] Cubemap textures are currently unsupported: %s\n", path)
    C.free(ddsData.data)
    return false
  end

  return true
end

local terra loadDDSTexture( path : rawstring, device : &Gfx.Device, useSRGB : bool)
  var ddsData : DdsData
  if not loadDDSTextureData(path, &ddsData) then
    C.printf("[Error] Unable to load dds texture file %s\n", path)
    return Gfx.NilTexture
  end

  var format = getDDSFormat(&ddsData)
  if useSRGB then
    format = Gfx.linearToSrgbFormat(format)
  end

  if format == Gfx.TextureFormat.UNKNOWN then
    C.printf("[Error] Unsupported format for dds texture file %s\n", path)
    C.free(ddsData.data)
    return Gfx.NilTexture
  end

  if ddsData.header.mipCount > 16 then
    C.printf("[Error] dds file %s has too many mipmap levels: %d (max 16)\n", ddsData.header.mipCount)
  end

  var builder = device:beginBuildTexture2D(ddsData.header.width, ddsData.header.height)
    :setFormat(format)
    :setMipLevelCount(ddsData.header.mipCount)

  var blockSize = 0
  if   format == Gfx.TextureFormat.BC1_UNORM
    or format == Gfx.TextureFormat.BC1_UNORM_SRGB
  then
    blockSize = 8
  else
    blockSize = 16
  end

  var levelIdx = 0
  var width = ddsData.header.width
  var height = ddsData.header.height
  var pitch = max( 1, ((width+3)/4) ) * blockSize

  for i=0, ddsData.header.mipCount do
    builder:setInitDataForLevel(i, &(ddsData.data[levelIdx]), pitch)
    levelIdx = levelIdx + (max(1, ( (width + 3) / 4 ) ) * max(1, ( (height + 3) / 4 ) ) * blockSize)
    width = width >> 1
    height = height >> 1
    pitch = max( 1, ((width+3)/4) ) * blockSize
  end

  var texture = builder:endBuild()
  return texture
end

return {
  loadDDSTexture = loadDDSTexture,
}
