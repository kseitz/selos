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
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
]])

local Array = require "array"
local Quat = require "quat"
local IO = require "io-utilities"
local Parser = IO.Parser
local charLit = IO.charLit
local KeyFrame = require("path").KeyFrame

local MaterialTypes = require("MaterialSystem").MaterialTypes.byName


local struct Object
{
  name      : rawstring
  filename  : rawstring
  overrideMaterial : rawstring
  materialModel : int
  materialModelOverrides : Array(tuple(rawstring, int, bool))
  transform : mat4
}

terra Object:init()
  self.name = ""
  self.filename = ""
  self.overrideMaterial = nil
  self.materialModel = MaterialTypes.StandardMaterial.bitflag
  self.materialModelOverrides:init()
  self.transform = make_mat4(1.0)
end

local struct DirLight
{
  name      : rawstring
  direction : vec3
  intensity : vec3
}

terra DirLight:init()
  self.name = ""
  self.direction = make_vec3(1)
  self.intensity = make_vec3(1)
end

local struct PointLight
{
  name      : rawstring
  position  : vec3
  intensity : vec3
}

terra PointLight:init()
  self.name = ""
  self.position = make_vec3(1)
  self.intensity = make_vec3(1)
end

local struct Camera
{
  position : vec3
  rotation : Quat
  sensitivity : float
  aspect : float
  zNear  : float
  zFar   : float
  fovY   : float -- in degrees
}

terra Camera:init()
  self.rotation = Quat{0,0,0,1}
  self.aspect = 0
  self.zNear = -1
  self.zFar  = -1
  self.fovY  = -1
end

local struct Scene
{
  objects       : Array(Object)
  dirLights     : Array(DirLight)
  pointLights   : Array(PointLight)
  shadowedPointLights : Array(PointLight)
  camera        : Camera
  path          : Array(KeyFrame)
}

terra Scene:init()
  self.objects:init()
  self.dirLights:init()
  self.pointLights:init()
  self.shadowedPointLights:init()
  self.camera:init()
  self.path:init()
end

terra Scene:delete()
  self.objects:delete()
  self.dirLights:delete()
  self.pointLights:delete()
  self.shadowedPointLights:delete()
  self.path:delete()
end

--

local terra getScale(v : vec3)
  var mat = make_mat4(1)
  mat(0)(0) = v.x
  mat(1)(1) = v.y
  mat(2)(2) = v.z
  return mat
end

local terra getTranslate(v : vec3)
  var mat = make_mat4(1)
  mat(0)(3) = v.x
  mat(1)(3) = v.y
  mat(2)(3) = v.z
  return mat
end

local terra getRotate(a : float, v : vec3)
  var rad = radians(a)
  var axis = normalize(make_vec3(v.x, v.y, v.z))
  var rot = Quat.rotationFromAxisAngle(axis, rad)
  return rot:toMat4x4()
end

--

local terra tryParseComment( p : &Parser )
  p:parseSpace()

  if p:tryParseToken("--") then
    p:skipToEndOfLine()
    p:parseEndOfLine()
    return true
  end

  return false
end

local terra tryParseEqualSign( p : &Parser )
  p:parseSpace()

  if not p:tryParseToken("=") then
    p:reportError("Expected '='")
    return false
  end

  p:parseSpace()
  return true
end

local terra parseVec2( p : &Parser )
  p:parseSpace()

  var v = make_vec2(1)
  v.x = p:parseFloat()
  p:parseSpace()
  v.y = p:parseFloat()
  p:parseSpace()

  p:parseEndOfLine()

  return v
end

local terra parseVec3( p : &Parser )
  p:parseSpace()

  var v = make_vec3(1)
  v.x = p:parseFloat()
  p:parseSpace()
  v.y = p:parseFloat()
  p:parseSpace()
  v.z = p:parseFloat()
  p:parseSpace()

  p:parseEndOfLine()

  return v
end

local terra parseTransforms( p : &Parser, obj : &Object )
  p:parseSpace()

  var t = make_mat4(1)

  if not p:tryParseToken("{") then
    p:reportError("Expected '{' to open transforms")
    return false
  end
  p:parseEndOfLine()

  while p:peek() ~=0 do
    p:parseSpace()
    if p:tryParseToken("scale") then
      p:parseSpace()
      t = t * getScale(parseVec3(p))
    elseif p:tryParseToken("translate") then
      p:parseSpace()
      t = t * getTranslate(parseVec3(p))
    elseif p:tryParseToken("rotate") then
      p:parseSpace()
      t = t * getRotate(p:parseFloat(), parseVec3(p))
    elseif p:peek() == charLit'}' then
      break
    elseif p:atEndOfLine() then
      p:parseEndOfLine()
    elseif tryParseComment(p) then
      -- do nothing
    else
      p:reportError("Unexpected symbol when parsing transforms")
      return false
    end
  end

  if not p:tryParseToken("}") then
    p:reportError("Expected '}' to close transforms")
    return false
  end

  obj.transform = t
  return true
end

local terra findMaterialModelID(materialModel : rawstring)
  [(function()
    local ret = quote
      return -1
    end
    for k, v in pairs(MaterialTypes) do
      ret = quote
        if C.strcmp([k], materialModel) == 0 then
          return v.bitflag
        else
          [ret]
        end
      end
    end
    return ret
  end)()]
end

local terra parseMaterialModelOverrides( p : &Parser, obj : &Object )
  p:parseSpace()

  if not p:tryParseToken("{") then
    p:reportError("Expected '{' to open material model overrides")
    return false
  end
  p:parseEndOfLine()

  while p:peek() ~=0 do
    p:parseSpace()
    if p:peek() == charLit'}' then
      break
    elseif p:atEndOfLine() then
      p:parseEndOfLine()
    elseif tryParseComment(p) then
      -- do nothing
    else
      var matName = p:parseNextTokenUntil(charLit'=')
      if not p:tryParseToken("=") then
        p:reportError("Expected '=' after material name when parsing material model overrides")
        return false
      end
      var matModelName = p:parseNextTokenUntil(charLit' ')
      var matModelID = findMaterialModelID(matModelName)
      if matModelID < 0 then
        p:reportError("Invalid materialModel")
        return false
      end
      obj.materialModelOverrides:add( {matName, matModelID, false} )
    end
  end

  if not p:tryParseToken("}") then
    p:reportError("Expected '}' to close material model overrides")
    return false
  end

  return true
end

local terra parseObject( p : &Parser, s : &Scene )
  p:parseSpace()

  var obj : Object
  obj:init()

  if not p:tryParseToken("{") then
    p:reportError("Expected '{' to open object")
    return false
  end
  p:parseEndOfLine()

  while p:peek() ~=0 do
    p:parseSpace()
    if p:tryParseToken("name") then
      if not tryParseEqualSign(p) then return false end
      obj.name = p:parseLine()
    elseif p:tryParseToken("filename") then
      if not tryParseEqualSign(p) then return false end
      obj.filename = p:parseLine()
    elseif p:tryParseToken("overrideMaterial") then
      if not tryParseEqualSign(p) then return false end
      obj.overrideMaterial = p:parseLine()
    elseif p:tryParseToken("materialModelOverrides") then
      -- Note: This elseif must come before the "materialModel" elseif due to how the parser works
      if not parseMaterialModelOverrides(p, &obj) then
        return false
      end
    elseif p:tryParseToken("materialModel") then
      if not tryParseEqualSign(p) then return false end
      var materialModel = p:parseLine()
      obj.materialModel = findMaterialModelID(materialModel)
      if obj.materialModel < 0 then
        p:reportError("Invalid materialModel")
        return false
      end
    elseif p:tryParseToken("transforms") then
      if not parseTransforms(p, &obj) then return false end
    elseif p:peek() == charLit'}' then
      break
    elseif p:atEndOfLine() then
      p:parseEndOfLine()
    elseif tryParseComment(p) then
      -- do nothing
    else
      p:reportError("Unexpected symbol when parsing object")
      return false
    end
  end

  if not p:tryParseToken("}") then
    p:reportError("Expected '}' to close object")
    return false
  end

  s.objects:add(obj)
  return true
end

local terra parseDirLight( p : &Parser, s : &Scene )
  p:parseSpace()

  var l : DirLight
  l:init()

  if not p:tryParseToken("{") then
    p:reportError("Expected '{' to open dirLight")
    return false
  end
  p:parseEndOfLine()

  while p:peek() ~=0 do
    p:parseSpace()
    if p:tryParseToken("name") then
      if not tryParseEqualSign(p) then return false end
      l.name = p:parseLine()
    elseif p:tryParseToken("intensity") then
      if not tryParseEqualSign(p) then return false end
      l.intensity = parseVec3(p)
    elseif p:tryParseToken("direction") then
      if not tryParseEqualSign(p) then return false end
      l.direction = parseVec3(p)
    elseif p:peek() == charLit'}' then
      break
    elseif p:atEndOfLine() then
      p:parseEndOfLine()
    elseif tryParseComment(p) then
      -- do nothing
    else
      p:reportError("Unexpected symbol when parsing object")
      return false
    end
  end

  if not p:tryParseToken("}") then
    p:reportError("Expected '}' to close dirLight")
    return false
  end

  s.dirLights:add(l)
  return true
end

local terra parsePointLight( p : &Parser, s : &Scene, isShadowed : bool )
  p:parseSpace()

  var l : PointLight
  l:init()

  if not p:tryParseToken("{") then
    p:reportError("Expected '{' to open pointLight")
    return false
  end
  p:parseEndOfLine()

  while p:peek() ~=0 do
    p:parseSpace()
    if p:tryParseToken("name") then
      if not tryParseEqualSign(p) then return false end
      l.name = p:parseLine()
    elseif p:tryParseToken("intensity") then
      if not tryParseEqualSign(p) then return false end
      l.intensity = parseVec3(p)
    elseif p:tryParseToken("position") then
      if not tryParseEqualSign(p) then return false end
      l.position = parseVec3(p)
    elseif p:peek() == charLit'}' then
      break
    elseif p:atEndOfLine() then
      p:parseEndOfLine()
    elseif tryParseComment(p) then
      -- do nothing
    else
      p:reportError("Unexpected symbol when parsing object")
      return false
    end
  end

  if not p:tryParseToken("}") then
    p:reportError("Expected '}' to close pointLight")
    return false
  end

  if isShadowed then
    s.shadowedPointLights:add(l)
  else
    s.pointLights:add(l)
  end

  return true
end

local terra parseCamera( p : &Parser, s : &Scene )
  p:parseSpace()

  var c = &s.camera

  if not p:tryParseToken("{") then
    p:reportError("Expected '{' to open camera")
    return false
  end
  p:parseEndOfLine()

  var hasRotation = false
  var hasTarget = false
  var target = make_vec3(0,0,0)
  var up = make_vec3(0,1,0)

  while p:peek() ~=0 do
    p:parseSpace()
    if p:tryParseToken("position") then
      if not tryParseEqualSign(p) then return false end
      c.position = parseVec3(p)
    elseif p:tryParseToken("rotation") then
      if not tryParseEqualSign(p) then return false end
      hasRotation = true
      var rot = radians(parseVec3(p))
      var rotX = Quat.rotationFromAxisAngle(make_vec3(0,1,0), rot.x)
      var rotY = Quat.rotationFromAxisAngle(make_vec3(1,0,0), rot.y)
      var rotZ = Quat.rotationFromAxisAngle(make_vec3(0,0,1), rot.z)
      c.rotation = rotX * rotY * rotZ
    elseif p:tryParseToken("aspect") then
      if not tryParseEqualSign(p) then return false end
      c.aspect = p:parseFloat()
    elseif p:tryParseToken("target") then
      if not tryParseEqualSign(p) then return false end
      target = parseVec3(p)
      hasTarget = true
    elseif p:tryParseToken("up") then
      if not tryParseEqualSign(p) then return false end
      up = parseVec3(p)
    elseif p:tryParseToken("sensitivity") then
      if not tryParseEqualSign(p) then return false end
      c.sensitivity = p:parseFloat()
    elseif p:peek() == charLit'}' then
      break
    elseif p:atEndOfLine() then
      p:parseEndOfLine()
    elseif tryParseComment(p) then
      -- do nothing
    else
      p:reportError("Unexpected symbol when parsing object")
      return false
    end
  end

  if hasTarget and hasRotation then
    p:reportError("Camera cannot have both rotation and target")
  end

  if not p:tryParseToken("}") then
    p:reportError("Expected '}' to close camera")
    return false
  end

  if hasTarget then
    c.rotation = Quat.fromVectors(c.position, target, up)
  end

  return true
end

local terra parseKeyFrame( p : &Parser, s : &Scene )
  p:parseSpace()

  var f : KeyFrame
  f:init()

  if not p:tryParseToken("{") then
    p:reportError("Expected '{' to open frame")
    return false
  end
  p:parseEndOfLine()

  while p:peek() ~= 0 do
    p:parseSpace()
    if p:tryParseToken("time") then
      if not tryParseEqualSign(p) then return false end
      f.time = p:parseFloat()
    elseif p:tryParseToken("pos") then
      if not tryParseEqualSign(p) then return false end
      f.pos = parseVec3(p)
    elseif p:tryParseToken("target") then
      if not tryParseEqualSign(p) then return false end
      f.target = parseVec3(p)
    elseif p:tryParseToken("up") then
      if not tryParseEqualSign(p) then return false end
      f.up = parseVec3(p)
    elseif p:peek() == charLit'}' then
      break
    elseif p:atEndOfLine() then
      p:parseEndOfLine()
    else
      p:reportError("Unexpected symbol when parsing frame")
      return false
    end
  end

  if not p:tryParseToken("}") then
    p:reportError("Expected '}' to close frame")
    return false
  end

  s.path:add(f)
  return true
end

local terra parsePath( p : &Parser, s : &Scene )
  p:parseSpace()

  if not p:tryParseToken("{") then
    p:reportError("Expected '{' to open path")
    return false
  end
  p:parseEndOfLine()

  while p:peek() ~= 0 do
    p:parseSpace()
    if p:tryParseToken("frame") then
      if not parseKeyFrame(p, s) then return false end
    elseif p:peek() == charLit'}' then
      break
    elseif p:atEndOfLine() then
      p:parseEndOfLine()
    else
      p:reportError("Unexpected symbol when parsing path")
      return false
    end
  end

  if not p:tryParseToken("}") then
    p:reportError("Expected '}' to close path")
    return false
  end

  -- Path elements already added by calls to parseKeyFrame()
  return true
end


local checkError = macro(function(s, e)
  return quote
    if not s then
      e = true
      break
    end
  end
end)

local terra loadSceneFile(path : rawstring)
  var pp : Parser
  pp:init(path)
  var p = &pp

  if not p:isValid() then
    C.printf("Error initializing parser for scene file '%s'\n", path)
    p:delete()
    return nil
  end

  var scene = [&Scene](C.malloc(terralib.sizeof(Scene)))
  scene:init()

  var e = false

  while(p:peek() ~= 0) do
    p:parseSpace()

    if p:tryParseToken("object") then
      checkError(parseObject(p, scene), e)
    elseif p:tryParseToken("dirLight") then
      checkError(parseDirLight(p, scene), e)
    elseif p:tryParseToken("pointLight") then
      checkError(parsePointLight(p, scene, false), e)
    elseif p:tryParseToken("shadowedPointLight") then
      checkError(parsePointLight(p, scene, true), e)
    elseif p:tryParseToken("camera") then
      checkError(parseCamera(p, scene), e)
    elseif p:tryParseToken("path") then
      checkError(parsePath(p, scene), e)
    elseif p:atEndOfLine() then
      p:parseEndOfLine()
    elseif tryParseComment(p) then
      -- do nothing
    else
      p:reportError("Unexpected symbol")
      e = true
    end
  end

  p:delete()

  if e then
    scene:delete()
    C.free(scene)
    return nil
  end

  return scene
end

return {
  loadSceneFile = loadSceneFile;
}
