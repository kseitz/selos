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

import "pipelineWrapperSyntax"

local Gfx = require("gfx")
local CONSTANTS = require("constants")
local TILE_SIZE = CONSTANTS.TILE_SIZE

local SDP = require("ShadingDataProvider")
local MaterialSystem = require("MaterialSystem")
local LightSystem = require("LightSystem")


local pipeline DeferredMaterialShader {
  ConfigurationOptions {
    MaterialType = MaterialSystem.TiledDeferredMaterialType.new()
    LightEnv = LightSystem.TiledLightListEnv.new()
  }

  numthreads(TILE_SIZE, TILE_SIZE, 1)

  textureImage(Gfx.TextureFormat.RGBA32F, Gfx.MapMode.ReadOnly) gbuffers : image2D[5]
  textureImage(Gfx.TextureFormat.RGBA16U, Gfx.MapMode.ReadOnly) tileList : uimage2D
  textureImage(Gfx.TextureFormat.RGBA16F, Gfx.MapMode.WriteOnly) outImage : image2D

  uniform Camera {
    cameraPos       : vec3
    numTilesX       : int
    cameraVP        : mat4
  }


  compute code
    var tileIDxyzw : uvec4 = imageLoad(tileList, make_ivec2(WorkGroupID.x / 4, [getConfigurationID()]))
    var tileID = tileIDxyzw(WorkGroupID.x % 4)

    var tileCoords = make_ivec2(tileID % numTilesX, tileID / numTilesX)

    var locID : ivec2
    locID.x = LocalInvocationID.x
    locID.y = LocalInvocationID.y

    var pixelCoords = (tileCoords * TILE_SIZE) + locID

    var sd : MaterialType:ShadingDataType()
    [SDP.GBufferShadingData.prepare(gbuffers, cameraPos, pixelCoords, sd)]

    if CONSTANTS.ROUGH_METAL_MODEL then
      -- R - Occlusion; G - Roughness; B - Metalness
      var spec = sd.specular
      var baseColor = sd.diffuse

      sd.diffuse = lerp(baseColor.rgb, make_vec3(0, 0, 0), spec.b);

      -- UE4 uses 0.08 multiplied by a default specular value of 0.5 as a base, hence the 0.04
      sd.specular = lerp(make_vec3(0.04f), baseColor.rgb, spec.b);
      sd.linearRoughness = spec.g;
    end

    sd.linearRoughness = max(0.08f, sd.linearRoughness)

    var clipZ : float = (cameraVP * make_vec4(sd.wPos, 1f)).z

    var colorAcc = [LightEnv:illuminate(MaterialType)](sd, clipZ, tileID)

    colorAcc = colorAcc + sd.emissive

    var color = make_vec4(colorAcc, 1)
    imageStore(outImage, pixelCoords, color)
  end
}

return DeferredMaterialShader
