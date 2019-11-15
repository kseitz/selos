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

local kPI = terralib.constant(float, 3.14159265359)
local k1_PI = terralib.constant(float, 0.31830988618379)

local Gfx = require("gfx")

local CONSTANTS = require("constants")
local FWD_BIT_FLAGS = CONSTANTS.FWD_BIT_FLAGS

-- Forward bitflag:
-- 00000001 -> diffuse map
-- 00000010 -> specular map
-- 00000100 -> emissive map
-- 00001000 -> bump map

local struct TangentFrame {
  u : vec3;
  v : vec3;
  n : vec3;
}

local pipeline ForwardMaterialShader {
  textureSampler map_Kd : sampler2D
  textureSampler map_Ks : sampler2D
  textureSampler map_Ke : sampler2D

  textureSampler map_bump : sampler2D

  uniform MaterialProperties {
    Kd : vec3
    Ks : vec3
    Ke : vec3
    kdTexCoordIdx : int
    ksTexCoordIdx : int
    keTexCoordIdx : int
    normalTexCoordIdx : int
    fwdbitflag : int
    defbitflag : int
  }

  param p : mat4
  param v : mat4

  uniform Uniforms {
    model           : mat4
    modelView       : mat4 = v * model_param
    modelViewProj   : mat4 = p * v * model_param
  }

  input inPosition  : vec3
  input inNormal    : vec3
  input inBitangent : vec3
  input inTexCoord  : vec2[CONSTANTS.MAX_TEX_COORDS]

  varying wPos      : vec3
  varying vNormal   : vec3
  varying vBitangent : vec3
  varying vTexCoord : vec2[CONSTANTS.MAX_TEX_COORDS]

  -- TODO Figure out how to replace these "out" declarations
  --      with references to GBufferShadingData elements
  --      in order to keep things consistent (because the order
  --      of these "out" declarations has to match the order that
  --      they are expected to be stored in the GBuffers).
  out outWPos       : vec4
  out outNormal     : vec4
  out diffuse       : vec4
  out specular      : vec4
  out emissive      : vec4

  vertex code
    var pos = make_vec4(inPosition, 1)

    Position  = modelViewProj * pos
    wPos      = (model * pos).xyz
    vNormal   = (transpose(inverse(model)) * make_vec4(inNormal,0)).xyz
    vBitangent = (model * make_vec4(inBitangent,0)).xyz
    for i=0, CONSTANTS.MAX_TEX_COORDS do
      vTexCoord[i] = inTexCoord[i]
    end
  end

  fragment code
    if (fwdbitflag and FWD_BIT_FLAGS.DIFFUSE_MAP) > 0 then
      diffuse = texture(map_Kd, vTexCoord[kdTexCoordIdx].xy)
    else
      diffuse = make_vec4(Kd, 1)
    end

    if (fwdbitflag and FWD_BIT_FLAGS.SPECULAR_MAP) > 0 then
      specular = texture(map_Ks, vTexCoord[ksTexCoordIdx].xy)
    else
      specular = make_vec4(Ks, 0.5) -- TODO roughness is currently hardcoded here
    end

    if (fwdbitflag and FWD_BIT_FLAGS.EMISSIVE_MAP) > 0 then
      emissive = texture(map_Ke, vTexCoord[keTexCoordIdx].xy)
    else
      emissive  = make_vec4(Ke, 1)
    end

    var n = normalize(vNormal)
    if (fwdbitflag and FWD_BIT_FLAGS.NORMAL_MAP) > 0 then
      var b = normalize(vBitangent - n * (dot(vBitangent, n)))
      var t = normalize(cross(b, n))
      var map = texture(map_bump, vTexCoord[normalTexCoordIdx].xy).xyz
      if CONSTANTS.ROUGH_METAL_MODEL then
        var rg = map.rg
        map.x = rg.r * 2 - 1;
        map.y = rg.g * 2 - 1;

        -- Saturate because error from BC5 can break the sqrt
        map.z = saturate(dot(rg, rg)); -- z = r*r + g*g
        map.z = sqrt(1 - map.z);
        normalize(map);
      else
        map = map * 2.f - 1.f
        map = normalize(map)
      end
      n = t * map.x + b * map.y + n * map.z
    end

    var IoR = 5.f -- TODO hardcoded for now

    outWPos   = make_vec4(wPos, defbitflag)
    outNormal = make_vec4(n,IoR)
  end
}


local DeferredMaterialShader = require("DeferredShader")


return {
  forward = ForwardMaterialShader,
  deferred = DeferredMaterialShader,
}
