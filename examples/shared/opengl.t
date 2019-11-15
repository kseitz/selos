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
--

local global = terralib.global

local C = terralib.includecstring([[
  #include <stdio.h>
  #include <stdlib.h>
]])

local SDL = require "sdl"

local GL = terralib.includecstring([[
#ifdef _WIN32
  #include <windows.h>
  #include <GL/GLU.h>
#endif
  #include "glcorearb.h"
  #include "glext.h"
]], terralib.newlist{"-I" .. externalPath})


-- Make a list of all loadable GL functions
local loadableFunctions = {}
for k,v in pairs(terralib.includec("glcorearb.h",
                 terralib.newlist{"-I" .. externalPath, "-D GL_GLEXT_PROTOTYPES"})) do
	-- only consider functions with names that start with `gl`
	-- since a bunch of Windows functions also get included along the way...
    s, e = string.find(k, "gl")
    if s == 1 and e == 2 then
      loadableFunctions[k] = v
    end
end

-- When a loadable function is requested via the `GL` table,
-- make a global variable to represent it, and add it to the
-- list of to-be-loaded functions.

local functionsToLoad = {}

local loadFunctionsKey = "loadFunctions"
local gCachedLoadFunctions = nil

local function generateLoadFunctions()
  	local body = terralib.newlist()
	for k,v in pairs(functionsToLoad) do
		local f = GL[k]
		body:insert(quote
        f = [f:gettype()](SDL.SDL_GL_GetProcAddress(k))
	  		if f == nil then
	    		C.printf("Unable to load OpenGL function `%s`\n", k)
	    		return false
	  		end       
		end)
	end

	local terra loadFunctions()
		[body]
		return true
	end

	return loadFunctions
end

GL.loadFunctionsMacro = macro(function()
	if gCachedLoadFunctions == nil then
		gCachedLoadFunctions = generateLoadFunctions()
	end
	return `(gCachedLoadFunctions())
end)

local metaTableForGL = getmetatable(GL)
local oldIndexFunc = metaTableForGL.__index
metaTableForGL.__index = nil

-- Function definition only
-- Declaration is at the end of gfx-opengl.t so that it is finalized after
-- all GL function have been used (because Terra now does eager type-checking)
terra GL.loadFunctions :: {} -> bool

metaTableForGL.__index = function(table, key)
	local f = rawget(loadableFunctions, key)
	if f == nil then
		return oldIndexFunc(table, key)
	end

	-- otherwise, this is a function we haven't seen before
	functionsToLoad[key] = key
	gCachedLoadFunctions = nil

	local t = f:gettype()
	local g = global(&t, nil)
	table[key] = g
	return g
end

--

return GL;
