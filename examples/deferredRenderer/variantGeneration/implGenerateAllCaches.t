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

local MaterialShader = require "materialShader"

local bit = require("bit")
local Utils = require("utilities")

local outputCache = require("implGenerateShaderCache")

local cacheDir = "build/caches/"
if gCurrentGraphicsAPIName == "gfx-d3d11" then
  cacheDir = cacheDir .. "D3D11/"
else
  cacheDir = cacheDir .. "OpenGL/"
end

local w = io.write
local function w(ln)
  io.write(ln)
  io.write("\n")
end

local NUM_MATERIAL_TYPES = #(MaterialShader.deferred:getDefaultConfiguration().MaterialType.statusList)
local NUM_LIGHT_TYPES    = #(MaterialShader.deferred:getDefaultConfiguration().LightEnv.statusList)
local NUM_COMPONENTS     = NUM_MATERIAL_TYPES + NUM_LIGHT_TYPES

local function combinations(elems, start, n, k, maxk, specLists)
  if k > maxk then
    local list = terralib.newlist()
    for i=1, n do
      list[i] = false
    end
    for i=1, maxk do
      list[elems[i]] = true
    end
    specLists:insert(list)
  else
    for i=start, n do
      elems[k] = i
      combinations(elems, i+1, n, k+1, maxk, specLists)
    end
  end
end

local function createCaches(n)
  local specLists = terralib.newlist()
  combinations({}, 1, NUM_COMPONENTS, 1, n, specLists)

  local specSetOutput = terralib.newlist()
  for i=1, #specLists do
    local spec = specLists[i]

    local specString = ""
    for j=1, #spec do
      specString = specString .. Utils.boolToInt(spec[j])
    end
    local outpath = cacheDir .. specString .. ".dat"

    local file = io.open(outpath, "w")
    io.output(file)
    outputCache(spec)
    io.close(file)

    specSetOutput:insert("../" .. outpath)
    specSetOutput:insert(specString)
  end

  local outpath = cacheDir .. "specSet-" .. n .. ".dat"
  local file = io.open(outpath, "w")
  io.output(file)
  for _, v in ipairs(specSetOutput) do
    w(v)
  end
  io.close(file)
end


for i=0, NUM_COMPONENTS do
  createCaches(i)
end
