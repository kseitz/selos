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

-- TODO We are currently not automating specialization of the forward shader,
--      so these values are hardcoded for now. Fix this.
local FWD_BIT_FLAGS = {}
FWD_BIT_FLAGS.DIFFUSE_MAP   = 1
FWD_BIT_FLAGS.SPECULAR_MAP  = 2
FWD_BIT_FLAGS.EMISSIVE_MAP  = 4
FWD_BIT_FLAGS.NORMAL_MAP    = 8

-- For benchmarking, we want the exact same frames each time, so we assume a constant FPS
local BENCHMARK_FPS = terralib.constant(int, 60)                      -- Lock to a constant FPS
local BENCHMARK_FRAMETIME = terralib.constant(float, (`(1.0f) / BENCHMARK_FPS))
local BENCHMARK_MEASURE_EVERY_X_FRAMES = terralib.constant(int, 100)  -- Perform benchmarking every X frames
local BENCHMARK_NUM_TIMES_TO_LOOP = terralib.constant(int, 1000)      -- Number of times to repeat the operation before stopping timer
                                                                      --   (to ensure enough time has elapsed between starting/stopping the timer)
local BENCHMARK_RENDER_ALL_FRAMES = terralib.constant(false)
local BENCHMARK_NUM_LIGHT_ITERS   = terralib.constant(1)

local TILE_SIZE = 16
local MAX_TEX_COORDS = 8
local NUM_CASCADES = 3
local LIGHT_CULLING_NUM_THREADS = 16

local MAX_DIR_LIGHTS = 1
local MAX_POINT_LIGHTS = 11
local MAX_SHADOWED_POINT_LIGHTS = 2

local POINT_LIGHT_FARZ = constant(float, 50.0)

-- TODO Using hardcoded radius for now. Fix this.
--      The value of 25 seems reasonable for the Sun Temple scene.
local POINT_LIGHT_RADIUS = `25.0f

local MAX_WINDOW_WIDTH  = 1920
local MAX_WINDOW_HEIGHT = 1080

local function calcMaxNumTiles()
  local numTilesX = math.ceil((MAX_WINDOW_WIDTH  / TILE_SIZE))
  local numTilesY = math.ceil((MAX_WINDOW_HEIGHT / TILE_SIZE))

  return numTilesX * numTilesY
end

local MAX_NUM_TILES = calcMaxNumTiles()

return {
  BENCHMARK_FPS = BENCHMARK_FPS,
  BENCHMARK_FRAMETIME = BENCHMARK_FRAMETIME,
  BENCHMARK_MEASURE_EVERY_X_FRAMES = BENCHMARK_MEASURE_EVERY_X_FRAMES,
  BENCHMARK_NUM_TIMES_TO_LOOP = BENCHMARK_NUM_TIMES_TO_LOOP,
  BENCHMARK_RENDER_ALL_FRAMES = BENCHMARK_RENDER_ALL_FRAMES,
  BENCHMARK_NUM_LIGHT_ITERS   = BENCHMARK_NUM_LIGHT_ITERS,
  FWD_BIT_FLAGS = FWD_BIT_FLAGS,
  TILE_SIZE     = TILE_SIZE,
  MAX_NUM_TILES = MAX_NUM_TILES,
  NUM_CASCADES  = NUM_CASCADES,
  LIGHT_CULLING_NUM_THREADS = LIGHT_CULLING_NUM_THREADS,
  MAX_DIR_LIGHTS            = MAX_DIR_LIGHTS,
  MAX_POINT_LIGHTS          = MAX_POINT_LIGHTS,
  MAX_SHADOWED_POINT_LIGHTS = MAX_SHADOWED_POINT_LIGHTS,
  POINT_LIGHT_FARZ = POINT_LIGHT_FARZ,
  POINT_LIGHT_RADIUS = POINT_LIGHT_RADIUS,
  MAX_TEX_COORDS = MAX_TEX_COORDS,
  DEFAULT_WINDOW_WIDTH  = MAX_WINDOW_WIDTH,
  DEFAULT_WINDOW_HEIGHT = MAX_WINDOW_HEIGHT,
  ROUGH_METAL_MODEL = `false,
}
