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


local Array = require("array")
local IO = require("io-utilities")
local Parser = IO.Parser
local charLit = IO.charLit

local struct FwdShaderDesc {
  materialID          : int
  vertexSourceCode    : rawstring
  fragmentSourceCode  : rawstring
}
local FwdArrayType = Array(FwdShaderDesc)

local struct DefShaderDesc {
  shaderID      : int
  shaderIndex   : int
  filename : rawstring
  computeSourceCode : rawstring
}
local DefArrayType = Array(DefShaderDesc)

local MatlMapArrayType = Array(int)

local terra tryParseComment( p : &Parser )
  p:parseSpace()

  if p:tryParseToken("--") then
    p:skipToEndOfLine()
    p:parseEndOfLine()
    return true
  end

  return false
end

local terra tryParseEqualSign( p : &Parser )
  p:parseSpace()

  if not p:tryParseToken("=") then
    p:reportError("Expected '='")
    return false
  end

  p:parseSpace()
  return true
end


local terra parseFwdShader( p : &Parser, s : &Array(FwdShaderDesc))
  p:parseSpace()

  if not p:tryParseToken("{") then
    p:reportError("Expected '{' to open fwdshader")
    return false
  end
  p:parseEndOfLine()

  var desc : FwdShaderDesc

  while p:peek() ~= 0 do
    p:parseSpace()
    if p:tryParseToken("materialID") then
      if not tryParseEqualSign(p) then return false end
      desc.materialID = p:tryParseUnsignedInt()
    elseif p:tryParseToken("vertexSourceCodeFile") then
      if not tryParseEqualSign(p) then return false end
      var filename = p:parseLine()
      desc.vertexSourceCode = IO.loadTextFile(filename)
    elseif p:tryParseToken("fragmentSourceCodeFile") then
      if not tryParseEqualSign(p) then return false end
      var filename = p:parseLine()
      desc.fragmentSourceCode = IO.loadTextFile(filename)
    elseif p:peek() == charLit'}' then
      break
    elseif p:atEndOfLine() then
      p:parseEndOfLine()
    elseif tryParseComment(p) then
      -- do nothing
    else
      p:reportError("Unexpected symbol while parsing shader")
      return false
    end
  end

  if not p:tryParseToken("}") then
    p:reportError("Expected '}' to close fwdshader")
    return false
  end

  s:add(desc)

  return true
end

local terra parseDefShader( p: &Parser, s : &Array(DefShaderDesc))
  p:parseSpace()

  if not p:tryParseToken("{") then
    p:reportError("Expected '{' to open fwdshader")
    return false
  end
  p:parseEndOfLine()

  var desc : DefShaderDesc

  while p:peek() ~= 0 do
    p:parseSpace()
    if p:tryParseToken("shaderID") then
      if not tryParseEqualSign(p) then return false end
      desc.shaderID  = p:tryParseUnsignedInt()
    elseif p:tryParseToken("shaderIndex") then
      if not tryParseEqualSign(p) then return false end
      desc.shaderIndex = p:tryParseUnsignedInt()
      if desc.shaderIndex ~= s.count then
        p:reportError("shaderIndex value does not match order of shaders")
        return false
      end
    elseif p:tryParseToken("computeSourceCodeFile") then
      if not tryParseEqualSign(p) then return false end
      var filename = p:parseLine()
      desc.filename = filename
      desc.computeSourceCode = IO.loadTextFile(filename)
    elseif p:peek() == charLit'}' then
      break
    elseif p:atEndOfLine() then
      p:parseEndOfLine()
    elseif tryParseComment(p) then
      -- do nothing
    else
      p:reportError("Unexpected symbol while parsing shader")
      return false
    end
  end

  if not p:tryParseToken("}") then
    p:reportError("Expected '}' to close defshader")
    return false
  end

  s:add(desc)

  return true
end

local terra parseMaterialMap( p: &Parser, m : &Array(int))
  p:parseSpace()

  if not p:tryParseToken("{") then
    p:reportError("Expected '{' to open fwdshader")
    return false
  end
  p:parseEndOfLine()

  while p:peek() ~= 0 do
    p:parseSpace()

    if p:peek() == charLit'}' then
      break
    elseif p:atEndOfLine() then
      p:parseEndOfLine()
    elseif tryParseComment(p) then
      -- do nothing
    else
      var mask = p:tryParseUnsignedInt()
      if mask ~= m.count then
        p:skipToEndOfLine()
        p:reportError("material masks must be in increasing order (without skipping any values)")
        return false
      end
      if not tryParseEqualSign(p) then return false end
      m:add(p:tryParseUnsignedInt())
    end
  end

  if not p:tryParseToken("}") then
    p:reportError("Expected '}' to close defshader")
    return false
  end

  return true
end

local checkError = macro(function(s,e)
  return quote
    if not s then
      e = true
      break
    end
  end
end)

local terra loadShaderCache(path : rawstring) : {&FwdArrayType, &DefArrayType, Array(int)}
  var pp : Parser
  pp:init(path)
  var p = &pp

  var matlMap : Array(int)
  matlMap:init()

  if not p:isValid() then
    C.printf("[Error] Could not load shader cache file '%s'\n", path)
    p:delete()
    return nil, nil, matlMap
  end

  var fwdShaders = [&FwdArrayType](C.malloc(terralib.sizeof(FwdArrayType)))
  fwdShaders:init()

  var defShaders = [&DefArrayType](C.malloc(terralib.sizeof(DefArrayType)))
  defShaders:init()

  var e = false

  while(p:peek() ~= 0) do
    p:parseSpace()

    if p:tryParseToken("fwdshader") then
      checkError(parseFwdShader(p, fwdShaders), e)
    elseif p:tryParseToken("defshader") then
      checkError(parseDefShader(p, defShaders), e)
    elseif p:tryParseToken("materialMap") then
      checkError(parseMaterialMap(p, &matlMap), e)
    elseif p:atEndOfLine() then
      p:parseEndOfLine()
    elseif tryParseComment(p) then
      -- do nothing
    else
      p:reportError("Unexpectes symbol")
      e = true
    end
  end

  p:delete()

  if e then
    fwdShaders:delete()
    C.free(fwdShaders)
    defShaders:delete()
    C.free(defShaders)
    return nil, nil, matlMap
  end

  return fwdShaders, defShaders, matlMap
end

return {
  loadShaderCache = loadShaderCache,
}
