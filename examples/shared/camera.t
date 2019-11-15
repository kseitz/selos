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

-- camera.t

local C = terralib.includecstring([[
  #include <math.h>
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
]])

local Array = require("array")
local KeyFrame = require("path").KeyFrame
local Quat = require "quat"

local struct Camera
{
    position : vec3;
    rotation : Quat;
    sensitivity : float;
    aspect : float;

    zNear : float;
    zFar  : float;
    fovX  : float; -- in radians
    fovY  : float; -- in radians
    path  : Array(KeyFrame)
}

-- fovY in degrees
-- to use default zNear, zFar, or fovY, pass in values < 0
terra Camera:init(position : vec3, rotation : Quat, sensitivity : float,
                  zNear : float, zFar : float, fovY : float, aspect : float, path : Array(KeyFrame))
  self.position = position
  self.rotation = rotation
  self.sensitivity = sensitivity
  self.aspect = aspect

  self.zNear = zNear
  self.zFar  = zFar
  self.fovY  = fovY * [math.pi] / 180.0f

  if zNear < 0 then
    self.zNear = 0.1
  end
  if zFar < 0 then
    self.zFar = 10000.0
  end
  if fovY < 0 then
    self.fovY = 60.0f * [math.pi] / 180.0f
  end

  self.fovX = 2 * atan(tan(self.fovY * 0.5) * self.aspect)

  self.path:init(path)
end

terra Camera:animate(curTime : float)
  if self.path.count <= 0 then
    return
  end

  var frame : KeyFrame

  -- find keyframes
  if curTime <= self.path:get(0).time then
    frame = self.path:get(0)
  elseif curTime >= self.path:get(self.path.count - 1).time then
    frame = self.path:get(self.path.count - 1)
  else
    for i=0, self.path.count - 1 do
      var cur   = self.path:get(i).time
      var next  = self.path:get(i+1).time

      if curTime >= cur and curTime < next then
        var delta = next - cur
        var c = curTime - cur
        var t = c / delta

        var curFrame  = self.path:get(i)
        var nextFrame = self.path:get(i+1)

        frame.time    = mix(curFrame.time, nextFrame.time, t);
        frame.pos     = mix(curFrame.pos, nextFrame.pos, t);
        frame.target  = mix(curFrame.target, nextFrame.target, t);
        frame.up      = mix(curFrame.up, nextFrame.up, t);

        break
      end
    end
  end

  self.position = frame.pos
  self.rotation = Quat.fromVectors(frame.pos, frame.target, frame.up)
end

terra Camera:getAxes() : {vec3,vec3,vec3}

  -- TODO: make this faster
  var m = self.rotation:toMat4x4();

  var xAxis = vec3{m.rows[0].x, m.rows[0].y, m.rows[0].z};
  var yAxis = vec3{m.rows[1].x, m.rows[1].y, m.rows[1].z};
  var zAxis = vec3{m.rows[2].x, m.rows[2].y, m.rows[2].z};
  
  return { xAxis, yAxis, zAxis };
end

terra Camera:setViewByMouse(xOffset : float, yOffset : float)  : &Camera
  var cam = self;

  var axes = self:getAxes();

  var sensitivity : float;
  sensitivity = 1000.0;
	cam:rotateCamera(-yOffset/sensitivity, axes._0);
	cam:rotateCamera(-xOffset/sensitivity, axes._1);

	return cam;
end

terra Camera:rotateCamera(angle : float, axis : vec3) : &Camera
	var cam = self;
  var r = Quat.rotationFromAxisAngle(axis, angle);
  cam.rotation = cam.rotation * r;

  return cam;
end

terra Camera:lookAt() : mat4x4
  var cam = self;

  var m = self.rotation:toMat4x4();

  var t = make_mat4(1.0f);
  t.rows[0].w = -cam.position.x;
  t.rows[1].w = -cam.position.y;
  t.rows[2].w = -cam.position.z;

  var res = m * t;

  return res;
end

terra Camera:moveCam(direction : int) : &Camera
	var cam = self;

  var axes = self:getAxes();

  var right = axes._0;
  var forward = -axes._2;


	if direction == 0 then -- forward
		cam.position = cam.position + cam.sensitivity * forward;
	elseif direction == 1 then -- backwards
		cam.position = cam.position - cam.sensitivity * forward;
	elseif direction == 2 then -- left
		cam.position = cam.position - cam.sensitivity * right;
	elseif direction == 3 then -- right
		cam.position = cam.position + cam.sensitivity * right;
	end
	return cam;
end

terra Camera:getView() : vec3
  return -self:getAxes()._2;
end


terra Camera:getRight() : vec3
  return self:getAxes()._0;
end

return Camera;