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

package.terrapath = package.terrapath .. "?.t;";
package.terrapath = package.terrapath .. "variantGeneration/?.t;";
package.terrapath = package.terrapath .. "shaders/?.t;";

package.terrapath = package.terrapath .. "../../src/?.t;";
package.terrapath = package.terrapath .. "../shared/?.t;";

gCurrentGraphicsAPIName = "gfx-opengl";

local ffi = require "ffi"
local os = ffi.os

if ffi.arch ~= "x64" then
  error("Selos only supports 64-bit architectures.")
end

-- parse the args provided at compile time, and filter to produce a runtime arg list
local function parseCompileTimeArgs(args)
  local runtimeArgs = {}
  runtimeArgs[-1] = arg[-1]
  runtimeArgs[0] = arg[0]

  local i = 1;
  local argCount = #arg;
  local rtArgCount = 0;
  while i <= argCount do
    local arg = args[i];
    if arg == "--d3d11" then
      if os ~= "Windows" then
        error("D3D11 is only supported on Windows")
      end
      gCurrentGraphicsAPIName = "gfx-d3d11";
    elseif arg == "--opengl" then
      gCurrentGraphicsAPIName = "gfx-opengl";
    else
      rtArgCount = rtArgCount + 1;
      runtimeArgs[rtArgCount] = arg;
    end
    i = i + 1;
  end

  return runtimeArgs;
end

local arg = parseCompileTimeArgs(arg);

externalPath = "../../external/"
SDLPath = externalPath .. "SDL2/"

print("Generating Shader Caches...")

local outputCache = require("implGenerateShaderCache")
local MaterialShader = require "materialShader"

local config = MaterialShader.deferred:getDefaultConfiguration()
local numComponents = (#config.MaterialType.statusList + #config.LightEnv.statusList)

local cachePath = "build/caches/"
if gCurrentGraphicsAPIName == "gfx-d3d11" then
  cachePath = cachePath .. "D3D11/"
else
  cachePath = cachePath .. "OpenGL/"
end

local files = {
                {allowSpec = false,  path = cachePath .. "shaderCache-fullyGeneral.dat"},
                {allowSpec = true,   path = cachePath .. "shaderCache-fullySpecialized.dat"},
              }

for i,v in ipairs(files) do
  local allowSpecList = terralib.newlist()
  for i=1, numComponents do
    allowSpecList:insert(v.allowSpec)
  end
  local file = io.open(v.path, "w")
  io.output(file)
  outputCache(allowSpecList)
  io.close(file)
end

require "implGenerateAllCaches"

print("Finished\n")

require "implCompileShaders"
