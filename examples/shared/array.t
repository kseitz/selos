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
#include <math.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
]])

local Array = terralib.memoize(function(T)
    local struct A
    {
        elements : &T;
        count : int;
        capacity : int;
    }
    A.methods.init = terralib.overloadedfunction("init")
    A.methods.init:adddefinition(
        terra(self : &A): {}
            C.memset(self, 0, sizeof(A));
    end)
    A.methods.init:adddefinition(
        terra(self : &A, numElements: int): {}
            self:init()
            self.count    = numElements
            self.capacity = numElements
            self.elements = [&T](C.realloc(self.elements, self.capacity*terralib.sizeof(T)))
    end)
    A.methods.init:adddefinition(
        terra (self : &A, other : A): {}
            self:init(other.count)
            if other.count > 0 then
                C.memcpy(self.elements, other.elements, other.count * terralib.sizeof(T))
            end
    end)
    terra A:add( val : T )
        var count = self.count;
        var capacity = self.capacity;
        var elements = self.elements;
        if count == capacity then
            if capacity ~= 0 then
                capacity = 2 * capacity;
            else
                capacity = 16;
            end

            elements = [&T](C.realloc(self.elements, capacity*terralib.sizeof(T)));

            self.elements = elements;
            self.capacity = capacity;
        end
        var index = count;
        count = count + 1;

        elements[index] = val;

        self.count = count;
        return index;
    end
    terra A:get_ptr( idx : int)
        return &self.elements[idx];
    end
    A.methods.get = macro(function(self, idx)
        return `@self:get_ptr(idx)
    end)
    terra A:trim()
        var count = self.count;
        if count ~= self.capacity then
            self.elements = [&T](C.realloc(self.elements, count*terralib.sizeof(T)));
            self.capacity = count
        end
    end
    terra A:getraw()
        self:trim()
        return self.elements
    end
    terra A:delete()
        C.free(self.elements)
    end
    return A;
end)


return Array;
