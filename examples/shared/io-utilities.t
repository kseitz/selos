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
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
]])

local Array = require "array"

local terra loadTextFile( path : rawstring )
  var file = C.fopen( path, "rb" );
  if file == nil then
    C.printf("Error: Unable to load text file '%s'\n", path)
    return nil
  end

  C.fseek(file, 0, C.SEEK_END);
  var size = C.ftell(file);
  C.fseek(file, 0, C.SEEK_SET);

  var buffer = [rawstring](C.malloc(size + 1));
  if buffer == nil then
    C.printf("Error: Unable to allocate read buffer for file '%s'\n", path)
    C.fclose(file)
    return nil
  end

  C.fread(buffer, 1, size, file);
  C.fclose(file);

  buffer[size] = 0;

  return buffer;
end

local terra strncasecmp( a : rawstring, b : rawstring, n : int ) : int
  for i = 0,n do
    var aa = C.tolower(a[i]);
    var bb = C.tolower(b[i]);
    if aa ~= bb then
      return aa - bb;
    end
    if aa == 0 then
      return 0;
    end
  end
  return 0;
end


local charLit = macro(function(s) return `(s[0]) end);

local struct Parser
{
  path    : rawstring;
  line    : int;
  buffer  : rawstring;
  cursor  : rawstring;

  scratchSize : int;
  scratch : rawstring;
}

terra Parser:init(path : rawstring)
  self.path = path
  self.line = 1
  self.buffer = loadTextFile(path)
  self.cursor = self.buffer
  self.scratchSize = 16
  self.scratch = [rawstring](C.malloc(self.scratchSize));
end

terra Parser:delete()
  if self.buffer ~= nil then
    C.free(self.buffer)
  end
  if self.scratch ~= nil then
    C.free(self.scratch)
  end
end

terra Parser:isValid()
  if self.buffer == nil or self.scratch == nil then
    return false
  end
  return true
end

terra Parser:reportError(msg : rawstring)
  C.printf("%s(%d): error: %s\n", self.path, self.line, msg);
  C.printf("%s\n", self:parseLine())
end

terra Parser:peek()
  return @self.cursor;
end

terra Parser:advance()
  self.cursor = self.cursor + 1;
end

terra Parser:parseSpace()
  while true do
    var c = self:peek();
    if( c == charLit(' ') or c == charLit('\t')) then
      self:advance();
    else
      return;
    end
  end
end

terra Parser:skipToEndOfLine()
  while true do
    var c = self:peek();
    if( c ~= 0 and c ~= charLit'\r' and c ~= charLit'\n') then
      self:advance();
    else
      return;
    end
  end
end

terra Parser:parseEndOfLine()
  while true do
    var c = self:peek();
    if( c == charLit'\r' or c == charLit'\n' ) then
      self:advance();
      var d = self:peek();
      if( (c ^ d) == (charLit'\r' ^ charLit'\n') ) then
        self:advance();
      end
      self.line = self.line + 1;
    else
      return
    end
  end
end

terra Parser:atEndOfLine() : bool
  var c = self:peek();
  return c == charLit'\r' or c == charLit'\n' or c == 0;
end

terra Parser:parseLine() : rawstring
  var line : Array(int8)
  line:init()

  while not self:atEndOfLine() do
    line:add(self:peek())
    self:advance()
  end

  self:parseEndOfLine()

  line:add(charLit'\0')

  return line:getraw()
end

terra Parser:getScratch(len : int) : rawstring
  if len > self.scratchSize then
    self.scratch = [rawstring](C.realloc(self.scratch, len));
  end
  return self.scratch;
end

terra Parser:parseFloat() : float
  var begin = self.cursor;
  var digitCount = 0;

  var c = self:peek();
  if( c == charLit'+' or c == charLit'-' ) then
    self:advance();
    c = self:peek();
  end

  while( c >= charLit'0' and c <= charLit'9' ) do
    digitCount = digitCount + 1;
    self:advance();
    c = self:peek();
  end

  if( c == charLit'.' ) then
    self:advance();
    c = self:peek();
    while( c >= charLit'0' and c <= charLit'9' ) do
      digitCount = digitCount + 1;
      self:advance();
      c = self:peek();
    end
  end

  if( c == charLit'e' or c == charLit'E' ) then
    self:advance();
    c = self:peek();
    if( c == charLit'+' or c == charLit'-' ) then
      self:advance();
      c = self:peek();
    end
    while( c >= charLit('0') and c <= charLit('9') ) do
      digitCount = digitCount + 1;
      self:advance();
      c = self:peek();
    end
  end

  if( digitCount == 0 ) then
    self:reportError("unexpected");
    return 0.0;
  end

  -- Note: we copy the string (terminated) to a scratch buffer
  -- before calling `strtof()`, because at least one implementation
  -- seems to have complexity linear in the length of the input
  -- string (not just the float prefix).
  --
  -- An alternative here would be to temporarily overwrite a byte
  -- in the input buffer to zero, to terminate the substring,
  -- but this wouldn't work if we ever decided to memory-map input files.
  --
  var len = self.cursor - begin;
  var scratch = self:getScratch(len+1);
  C.memcpy(scratch, begin, len);
  scratch[len] = 0;

  var value = C.strtof(scratch, nil);

  self:parseSpace();

  return value;
end

terra Parser:tryParseUnsignedInt() : uint
  var begin = self.cursor;
  var digitCount = 0;

  var c = self:peek();
  while( c >= charLit('0') and c <= charLit('9') ) do
    digitCount = digitCount + 1;
    self:advance();
    c = self:peek();
  end

  if( digitCount == 0 ) then
--        reportError(p, "unexpected");
      return -1;
  end

  var value = C.strtoul( begin, nil, 0 );

  self:parseSpace();

  return value;
end

terra Parser:tryParseToken(token : rawstring) : bool
  var len = C.strlen(token);
  if( strncasecmp(self.cursor, token, len) ~= 0 ) then
    return false;
  else
    self.cursor = self.cursor + len;
    return true;
  end
end

terra Parser:parseToken(token : rawstring)
  if not self:tryParseToken(token) then
    self:reportError("unexpected");
  end
end

-- Parses until either whitespace or the stop character is found
terra Parser:parseNextTokenUntil(stopChar : uint8)
  self:parseSpace();

  var begin = self.cursor;

  var c = self:peek();
  while c ~= 0 do
    if( c == charLit(' ') or c == charLit('\t') or self:atEndOfLine() or c == stopChar) then
      break;
    end
    self:advance();
    c = self:peek();
  end

  var len = self.cursor - begin;
  var ret = [rawstring](C.malloc(len+1));
  C.memcpy(ret, begin, len);
  ret[len] = charLit'\0';

  self:parseSpace();

  return ret
end


return {
  loadTextFile = loadTextFile,
  strncasecmp = strncasecmp,
  charLit = charLit,
  Parser = Parser,
}
