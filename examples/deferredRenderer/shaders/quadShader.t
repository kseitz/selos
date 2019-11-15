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

import "pipelineWrapperSyntax"

local gLuminanceLod = 16
local gExposureKey = `(0.042f)

local terra calcLuminance(color : vec3)
  return dot(color.xyz, make_vec3(0.299f, 0.587f, 0.114f));
end

local terra linearToneMap(color : vec3)
  return color; -- Do nothing
end


local pipeline LuminanceShader {
  input inPosition  : vec3
  input inTexCoord  : vec2

  varying vTexCoord : vec2
  out outColor      : vec4

  textureSampler tex : sampler2D

  vertex code
    Position  = make_vec4(inPosition, 1)
    vTexCoord = inTexCoord
  end

  fragment code
    var color = texture(tex, make_vec2(vTexCoord.x, 1-vTexCoord.y))

    var luminance = calcLuminance(color.xyz);
    luminance = 0.0060f -- hardcoding luminance to avoid flickering as camera moves
    luminance = log2(max(0.0001f, luminance));
    outColor = make_vec4(luminance, 0, 0, 1);
  end
}

local pipeline QuadShader {
  input inPosition  : vec3
  input inTexCoord  : vec2

  varying vTexCoord : vec2
  out outColor      : vec4

  textureSampler tex : sampler2D
  textureSampler lumTex : sampler2D


  local calcExposedColor = terra(color : vec3, texC : vec2)
    var pixelLuminance = calcLuminance(color);
    var avgLuminance : float = textureLod(lumTex, make_vec2(texC.x, 1-texC.y), gLuminanceLod).r;
    avgLuminance = exp2(avgLuminance);
    var exposedLuminance = (gExposureKey / avgLuminance);
    return exposedLuminance*color;
  end


  vertex code
    Position  = make_vec4(inPosition, 1)
    vTexCoord = inTexCoord
  end

  fragment code
    var color = texture(tex, make_vec2(vTexCoord.x, 1-vTexCoord.y))
    var exposedColor = calcExposedColor(color.rgb, vTexCoord);

    outColor = make_vec4(linearToneMap(exposedColor), color.a);
  end
}

return {
  QuadShader = QuadShader,
  LuminanceShader = LuminanceShader,
}
