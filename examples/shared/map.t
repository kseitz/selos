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

-- A really simple, slow map implementation
local C = terralib.includecstring([[
  #include "stdio.h"
  #include "string.h"
]])

local Array = require("array")

local Map = terralib.memoize(function(K,V, COMP)
  if COMP == nil then
    COMP = terra(a: K, b: K) return a == b end
  end

  local struct M
  {
    elements  : Array(tuple(K,V))
  }

  M.methods.init = terralib.overloadedfunction("init")
  M.methods.init:adddefinition( terra(self : &M) : {}
    self.elements:init()
  end)

  terra M:delete()
    self.elements:delete()
  end

  terra M:insert(k : K, v : V)
    return self.elements:add({k,v})
  end

  local terra find_ptr(m : &M, k : K)
    for i=0, m.elements.count do
      var v = m.elements:get_ptr(i)
      if COMP(v._0, k) then
        return &v._1
      end
    end
    return nil
  end

  terra M:contains(k : K)
    return find_ptr(self, k) ~= nil
  end

  terra M:get_ptr(k : K)
    var v : &V = find_ptr(self,k)
    if v == nil then
      var newV : V
      v = &self.elements:get(self:insert(k,newV))._1
    end
    return v
  end

  M.metamethods.__apply = macro(function(self, k)
    return `@self:get_ptr(k)
  end)

  return M
end)

return Map
