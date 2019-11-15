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
local TILE_SIZE                 = CONSTANTS.TILE_SIZE
local LIGHT_CULLING_NUM_THREADS = CONSTANTS.LIGHT_CULLING_NUM_THREADS

local MAX_POINT_LIGHTS          = CONSTANTS.MAX_POINT_LIGHTS
local MAX_SHADOWED_POINT_LIGHTS = CONSTANTS.MAX_SHADOWED_POINT_LIGHTS

local LightSystem = require("LightSystem")
local TileLightList       = LightSystem.TileLightList
local PointLight          = LightSystem.LightTypes.byName.PointLight
local ShadowedPointLight  = LightSystem.LightTypes.byName.ShadowedPointLight


local terra boxSphereIntersect(bbMin : vec3, bbMax : vec3, c : vec3, r : float)
  var r2 = r * r

  var dmin = 0.0f
  for i=0, 3 do
    if c(i) < bbMin(i) then
      dmin = dmin + pow(c(i) - bbMin(i), 2)
    elseif c(i) > bbMax(i) then
      dmin = dmin + pow(c(i) - bbMax(i), 2)
    end
  end

  return dmin <= r2
end

local pipeline LightCulling {
  numthreads(LIGHT_CULLING_NUM_THREADS, 1, 1)

  textureImage(Gfx.TextureFormat.RGBA32F, Gfx.MapMode.ReadOnly) wPos : image2D

  uniform Lights {
    pointLightCount         : int
    shadowedPointLightCount : int
    pointLights             : PointLight[MAX_POINT_LIGHTS]
    shadowedPointLights     : ShadowedPointLight[MAX_SHADOWED_POINT_LIGHTS]
  }

  uniform TileInfo {
    numTilesX : int
  }

  buffer lightLists : TileLightList[CONSTANTS.MAX_NUM_TILES]

  compute code
    var tileID = (WorkGroupID.x * LIGHT_CULLING_NUM_THREADS) + LocalInvocationID.x
    if tileID < CONSTANTS.MAX_NUM_TILES then
      var tileCoords = make_ivec2(tileID % numTilesX, tileID / numTilesX)
      var tileFirstPixel = tileCoords * TILE_SIZE

      var bbMin = imageLoad(wPos, tileFirstPixel).xyz
      var bbMax = bbMin

      for x=0, TILE_SIZE do
        for y=0, TILE_SIZE do
          var pixelCoords = tileFirstPixel + make_ivec2(x,y)
          var pos = imageLoad(wPos, pixelCoords).xyz

          bbMin = min(bbMin, pos)
          bbMax = max(bbMax, pos)
        end
      end

      var r = CONSTANTS.POINT_LIGHT_RADIUS

      -- Point Lights
      var count = 0
      for l=0, pointLightCount do
        var c = pointLights[l].position
        var isLightValid = boxSphereIntersect(bbMin, bbMax, c, r)

        if isLightValid then
          lightLists[tileID].idxPointLight[count] = l
          count = count + 1
        end
      end
      lightLists[tileID].numPointLight = count

      -- ShadowedPointLights
      count = 0
      for l=0, shadowedPointLightCount do
        var c = shadowedPointLights[l].position
        var isLightValid = boxSphereIntersect(bbMin, bbMax, c, r)

        if isLightValid then
          lightLists[tileID].idxShadowedPointLight[count] = l
          count = count + 1
        end
      end
      lightLists[tileID].numShadowedPointLight = count
    end
  end
}

return LightCulling
