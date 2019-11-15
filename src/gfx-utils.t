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

-- gfx-utils.t

local Gfx = require "gfx"

function Gfx.UniformBuffer(T, sym, setupCode)
  pcall(function()
    T = T:astype()
  end)

  local size = terralib.sizeof(T);

  local struct UB
  {
  	raw : Gfx.Buffer;
  }

  terra UB:init( device : &Gfx.Device )
  	self.raw = device:beginBuildBuffer()
	    :setSize(size)
	    :setUsage(Gfx.Usage.Dynamic)
	    :setBindUniformBuffer()
      :setCPUAccess(Gfx.CPUAccess.Write)
	    :endBuild();
  end

  terra UB:getRaw()
    return self.raw
  end

  terra UB:setRaw(r : Gfx.Buffer)
    self.raw = r
  end

  terra UB.methods.make( device : &Gfx.Device )
    var u : UB;
    u:init(device)
    return u;
  end

  terra UB:map( context : &Gfx.Context, mode : Gfx.MapModeT ) : &T
    var ptr = context:mapBuffer(self.raw, mode, size, Gfx.Usage.Dynamic);
    return [&T](ptr);
  end

  terra UB:unmap( context : &Gfx.Context )
  	context:unmapBuffer(self.raw);
  end

  if setupCode then
    terra UB:setup( context : &Gfx.Context )
      var [sym] = self:map(context, Gfx.MapMode.WriteDiscard)
      [setupCode]
      self:unmap(context)
    end
  end

  return UB;
end

local attributeFormatForType = {
  [vec2] = Gfx.AttributeFormat.Vec2,
  [vec3] = Gfx.AttributeFormat.Vec3,
  [vec4] = Gfx.AttributeFormat.Vec4,

  -- TODO: other variations here!!!
}

function Gfx.getAttributeFormatForType(type)
  local format = attributeFormatForType[type];
  assert(format ~= nil, "Unknown attribute format type")
  return format;
end

-- Convert an linear format to sRGB. If the format doesn't have a matching sRGB format, will return the original
terra Gfx.linearToSrgbFormat(format : Gfx.TextureFormatT)
  if format == Gfx.TextureFormat.BC1_UNORM then
    return Gfx.TextureFormat.BC1_UNORM_SRGB
  elseif format == Gfx.TextureFormat.BC2_UNORM then
    return Gfx.TextureFormat.BC2_UNORM_SRGB
  elseif format == Gfx.TextureFormat.BC3_UNORM then
    return Gfx.TextureFormat.BC3_UNORM_SRGB
  elseif format == Gfx.TextureFormat.RGBA8 then
    return Gfx.TextureFormat.RGBA8_SRGB
  end

  return format
end


return Gfx;