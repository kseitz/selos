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

local M = {}

function M.newExtensionBuilder(name)
  local ext = {
    name = name,
    MemberTypes = {},
  }

  setmetatable(ext, {
    __index = function(t,k)
      return t.MemberTypes[k]
    end,
  })

  function ext:registerMemberType(typename)
    ext.MemberTypes[typename] = {}
  end

  function ext:finishBuild()
    local ret = {
      name = self.name,
      memberMetatables = {}
    }

    for k, v in pairs(self.MemberTypes) do
      ret.memberMetatables[k] = {
        __indexTable = v,
        __index = function(table, key)
          local indexTable = getmetatable(table).__indexTable
          return indexTable[key]
        end,
      }
    end

    for k, v in pairs(self) do
      if type(v) == "function" then
        if not (k == "RegiseterMemberType" or k == "finishBuild") then
          ret[k] = v
        end
      end
    end

    return ret
  end

  return ext
end

return M
