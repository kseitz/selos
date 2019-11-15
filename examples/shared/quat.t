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

-- quat.t

local C = terralib.includec("math.h")

-- to be deleted just for debugging purposes
local C = terralib.includecstring([[
  #include "math.h"
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
]])

local struct Quat
{
    x : float;
    y : float;
    z : float;
    w : float;
}

terra Quat.metamethods.__mul( left : Quat, right : Quat )
    

    var result : Quat;

    result.x = left.w * right.x
            + left.x * right.w
            + left.y * right.z
            - left.z * right.y;

    result.y = left.w * right.y
            + left.y * right.w
            + left.z * right.x
            - left.x * right.z;

    result.z = left.w * right.z
            + left.z * right.w
            + left.x * right.y
            - left.y * right.x;

    result.w = left.w * right.w
            - left.x * right.x
            - left.y * right.y
            - left.z * right.z;

    return result;
end

terra Quat:getConjugate()
    return Quat{-self.x, -self.y, -self.z, self.w};
end

terra Quat:transformVector( v : vec3 )
    var vecQuat = Quat{v.x, v.y, v.z, 0};
    var conj = self:getConjugate();

    var rotQuat = conj * vecQuat * @self;

    return vec3{rotQuat.x, rotQuat.y, rotQuat.z};
end

terra Quat.methods.rotationFromAxisAngle(
    axis    : vec3,
    angle   : float ) : Quat

    var a = angle * 0.5; 
    var c = C.cosf(a);
    var s = C.sinf(a);

    var as = axis * s;

    var q : Quat;
    q.x = as.x;
    q.y = as.y;
    q.z = as.z;
    q.w = c;

    return q;
end

-- Adapted from: https://github.com/g-truc/glm/blob/master/glm/gtc/quaternion.inl
terra Quat.methods.fromMat4x4(m : mat4)
  var fourXSquaredMinus1 = m(0)(0) - m(1)(1) - m(2)(2);
  var fourYSquaredMinus1 = m(1)(1) - m(0)(0) - m(2)(2);
  var fourZSquaredMinus1 = m(2)(2) - m(0)(0) - m(1)(1);
  var fourWSquaredMinus1 = m(0)(0) + m(1)(1) + m(2)(2);

  var biggestIndex = 0;
  var fourBiggestSquaredMinus1 = fourWSquaredMinus1;
  if fourXSquaredMinus1 > fourBiggestSquaredMinus1 then
    fourBiggestSquaredMinus1 = fourXSquaredMinus1;
    biggestIndex = 1;
  end
  if fourYSquaredMinus1 > fourBiggestSquaredMinus1 then
    fourBiggestSquaredMinus1 = fourYSquaredMinus1;
    biggestIndex = 2;
  end
  if fourZSquaredMinus1 > fourBiggestSquaredMinus1 then
    fourBiggestSquaredMinus1 = fourZSquaredMinus1;
    biggestIndex = 3;
  end

  var biggestVal = sqrt(fourBiggestSquaredMinus1 + 1f) * 0.5f;
  var mult = 0.25f / biggestVal;

  if biggestIndex == 0 then
    return Quat {(m(2)(1) - m(1)(2)) * mult, (m(0)(2) - m(2)(0)) * mult, (m(1)(0) - m(0)(1)) * mult, biggestVal};
  elseif biggestIndex == 1 then
    return Quat {biggestVal, (m(1)(0) + m(0)(1)) * mult, (m(0)(2) + m(2)(0)) * mult, (m(2)(1) - m(1)(2)) * mult};
  elseif biggestIndex == 2 then
    return Quat {(m(1)(0) + m(0)(1)) * mult, biggestVal, (m(2)(1) + m(1)(2)) * mult, (m(0)(2) - m(2)(0)) * mult};
  elseif biggestIndex == 3 then
    return Quat {(m(0)(2) + m(2)(0)) * mult, (m(2)(1) + m(1)(2)) * mult, biggestVal, (m(1)(0) - m(0)(1)) * mult};
  else
    return Quat {0,0,0,1}
  end
end

-- Adapted from: https://github.com/g-truc/glm/blob/master/glm/ext/matrix_transform.inl
terra Quat.methods.fromVectors(eye : vec3, center : vec3, up : vec3)
  var f = normalize(center - eye);
  var u = normalize(up);
  var s = normalize(cross(f, u));
  u = cross(s, f);

  var res = make_mat4(1)
  res(0)(0) = s.x;
  res(0)(1) = s.y;
  res(0)(2) = s.z;
  res(1)(0) = u.x;
  res(1)(1) = u.y;
  res(1)(2) = u.z;
  res(2)(0) =-f.x;
  res(2)(1) =-f.y;
  res(2)(2) =-f.z;
  res(0)(3) =-dot(s, eye);
  res(1)(3) =-dot(u, eye);
  res(2)(3) = dot(f, eye);

  return Quat.fromMat4x4(res);
end

terra Quat:toMat4x4() : mat4x4
	var q = self;

    var m : mat4x4;

    var x2 = q.x + q.x;
    var y2 = q.y + q.y;
    var z2 = q.z + q.z;
    var xx2 = q.x * x2;
    var yy2 = q.y * y2;
    var zz2 = q.z * z2;
    var yz2 = q.y * z2;
    var wx2 = q.w * x2;
    var xy2 = q.x * y2;
    var wz2 = q.w * z2;
    var xz2 = q.x * z2;
    var wy2 = q.w * y2;

    -- TODO: the `row` members of the metashader
    -- matrix types are poorly named...

    m.rows[0].x = 1.0 - yy2 - zz2;
    m.rows[1].x = xy2 + wz2;
    m.rows[2].x = xz2 - wy2;
    m.rows[3].x = 0.0;

    m.rows[0].y = xy2 - wz2;
    m.rows[1].y = 1.0 - xx2 - zz2;
    m.rows[2].y = yz2 + wx2;
    m.rows[3].y = 0.0;

    m.rows[0].z = xz2 + wy2;
    m.rows[1].z = yz2 - wx2;
    m.rows[2].z = 1.0 - xx2 - yy2;
    m.rows[3].z = 0.0;

    m.rows[0].w = 0.0;
    m.rows[1].w = 0.0;
    m.rows[2].w = 0.0;
    m.rows[3].w = 1.0;


--    C.printf("{%f %f %f %f\n", m.row1.x, m.row1.y, m.row1.z, m.row1.w);
--    C.printf("%f %f %f %f\n", m.row2.x, m.row2.y, m.row2.z, m.row2.w);
--    C.printf("%f %f %f %f\n", m.row3.x, m.row3.y, m.row3.z, m.row3.w);
--    C.printf("%f %f %f %f}\n", m.row4.x, m.row4.y, m.row4.z, m.row4.w);

--    C.printf("%f", m.row1.x);

    return m;
end

-- Quaternion lookat function (up cannot be changed)
terra Quat:lookAt(position : vec3) : mat4x4
  var m = self:toMat4x4();

  var t = make_mat4(1.0f);

  t.rows[0].w = -position.x;
  t.rows[1].w = -position.y;
  t.rows[2].w = -position.z;

  var res = m * t;

  return res;
end

terra Quat:conjugate() : Quat
  var q = self;
  var result : Quat;

  result.x = -q.x;
  result.y = -q.y;
  result.z = -q.z;
  result.w = q.w;
  return result;
end

terra Quat.methods.rotationFromVectors(u : vec3, v : vec3) : Quat
  u = normalize(u)
  v = normalize(v)

  -- dot(u, v)
  -- this is not working for some reason
  var cosTheta = u.x * v.x + u.y * v.y + u.z * v.z

  var angle = C.acosf(cosTheta)

  var axis = normalize(cross(u, v))
  return Quat.rotationFromAxisAngle(axis, angle)
end

return Quat;
