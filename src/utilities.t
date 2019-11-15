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

local C = terralib.includecstring([[
  #include <stdio.h>
  #include <stdlib.h>
]])

local Utils = {}

function Utils.count(t)
  local c = 0
  for _,_ in pairs(t) do
    c = c + 1
  end
  return c
end

function Utils.combineTables(t1, t2)
  local ret = {}
  for k,v in pairs(t1) do
    ret[k] = v
  end
  for k,v in pairs(t2) do
    ret[k] = v
  end
  return ret
end

function Utils.subrange(l, first, last)
  local ret = terralib.newlist()
  for i = first, last do
    ret:insert(l[i])
  end
  return ret
end

function Utils.shallowCopy(orig)
  local copy = {}
  for k,v in pairs(orig) do
    copy[k] = v
  end
  return copy
end

-- helper function for debugging
function Utils.printkind(k)
  print("\nKind is:", terralib.kinds[k])
end

-- helper function for debugging
function Utils.printtable(t)
  if t.kind then
    print("kind",":",t.kind)
  end
  for k,v in pairs(t) do
    print(k,":",v)
  end
  print()
end

function Utils.unimpl(msg)
  error("Unimplemented: " .. debug.getinfo(2,"n").name .. " " .. (msg or ""), 2)
end


function Utils.find(tbl, value)
  for k,v in pairs(tbl) do
    if v == value then
      return v
    end
  end

  return false
end

function Utils.findIdx(tbl, value)
  for k,v in ipairs(tbl) do
    if v == value then
      return k
    end
  end

  return false
end



function Utils.alignof(ty)
  local struct T {
    a : int8;
    b : ty;
  }
  return terralib.offsetof(T, "b");
end

function Utils.joinEnv(fst, snd)
  return setmetatable({}, {__index = function(t,k)
    local v = fst[k];
    if v then
      return v;
    end
    return snd[k];
  end});
end

Utils.assert = terralib.internalmacro(function(ctx, tree, expr, msg)
  return quote
    if not expr then
      C.printf("%s:%d: Assertion failed: %s\n", [tree.filename], [tree.linenumber], msg)
      C.exit(1)
    end
  end
end)

-- From https://stackoverflow.com/a/9080080
-- Optional leading 0 padding to a specified number of bits (bits argument)
function Utils.toBits(num, bits)
  -- returns a table of bits, most significant first.
  bits = bits or math.max(1, select(2, math.frexp(num)))
  local t = {} -- will contain the bits
  for b = bits, 1, -1 do
      t[b] = math.fmod(num, 2)
      num = math.floor((num - t[b]) / 2)
  end
  return t
end

function Utils.toBitString(num, bits)
  local t = Utils.toBits(num, bits)
  return table.concat(t)
end

function Utils.boolToInt(b)
  if b then return 1 else return 0 end
end


return Utils
