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

-- assimp.t

local Array = require("array")
local MAX_TEX_COORDS = require("constants").MAX_TEX_COORDS

local C = terralib.includecstring([[
  #include <math.h>
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
]])

local assimpIncludeString = [[
  #include <assimp/cimport.h>        // Plain-C interface
  #include <assimp/material.h>       // Material system
  #include <assimp/postprocess.h>    // Post processing flags
  #include <assimp/scene.h>          // Output data structure
]]

local ai = terralib.includecstring(assimpIncludeString, { "-I" .. externalPath .. "assimp/include" , "-w" })

local charLit = macro(function(s) return `(s[0]) end);

-- #defines are not imported by Terra, so define these manually
ai.aiProcessPreset_TargetRealtime_Quality =
  ai.aiProcess_CalcTangentSpace             or
  ai.aiProcess_GenSmoothNormals             or
  ai.aiProcess_JoinIdenticalVertices        or
  ai.aiProcess_ImproveCacheLocality         or
  ai.aiProcess_LimitBoneWeights             or
  ai.aiProcess_RemoveRedundantMaterials     or
  ai.aiProcess_SplitLargeMeshes             or
  ai.aiProcess_Triangulate                  or
  ai.aiProcess_GenUVCoords                  or
  ai.aiProcess_SortByPType                  or
  ai.aiProcess_FindDegenerates              or
  ai.aiProcess_FindInvalidData              or
  0;

ai.aiProcessPreset_TargetRealtime_MaxQuality = 
  ai.aiProcessPreset_TargetRealtime_Quality   or
  ai.aiProcess_FindInstances                  or
  ai.aiProcess_ValidateDataStructure          or
  ai.aiProcess_OptimizeMeshes                 or
  0;

ai.AI_MATKEY_NAME = {"?mat.name",0,0}
ai.AI_MATKEY_TWOSIDED = {"$mat.twosided",0,0}
ai.AI_MATKEY_SHADING_MODEL = {"$mat.shadingm",0,0}
ai.AI_MATKEY_ENABLE_WIREFRAME = {"$mat.wireframe",0,0}
ai.AI_MATKEY_BLEND_FUNC = {"$mat.blend",0,0}
ai.AI_MATKEY_OPACITY = {"$mat.opacity",0,0}
ai.AI_MATKEY_BUMPSCALING = {"$mat.bumpscaling",0,0}
ai.AI_MATKEY_SHININESS = {"$mat.shininess",0,0}
ai.AI_MATKEY_REFLECTIVITY = {"$mat.reflectivity",0,0}
ai.AI_MATKEY_SHININESS_STRENGTH = {"$mat.shinpercent",0,0}
ai.AI_MATKEY_REFRACTI = {"$mat.refracti",0,0}
ai.AI_MATKEY_COLOR_DIFFUSE = {"$clr.diffuse",0,0}
ai.AI_MATKEY_COLOR_AMBIENT = {"$clr.ambient",0,0}
ai.AI_MATKEY_COLOR_SPECULAR = {"$clr.specular",0,0}
ai.AI_MATKEY_COLOR_EMISSIVE = {"$clr.emissive",0,0}
ai.AI_MATKEY_COLOR_TRANSPARENT = {"$clr.transparent",0,0}
ai.AI_MATKEY_COLOR_REFLECTIVE = {"$clr.reflective",0,0}
ai.AI_MATKEY_GLOBAL_BACKGROUND_IMAGE = {"?bg.global",0,0}

ai.AI_MATKEY_UVWSRC = macro(function(type, N) return {"$tex.uvwsrc", type, N} end)


-- rename these so we can follow a naming convention
-- where types are `UpperCase` and value are `lowerCase`
local Vec2 = vec2
local Vec3 = vec3

local struct Material
{
    name : rawstring
    Ns : float
    Ni : float
    d  : float
    Tr : float
    Tf : Vec3
    illum : uint
    Ka : Vec3
    Kd : Vec3
    Ks : Vec3
    Ke : Vec3
    map_Ka : rawstring
    map_Kd : rawstring
    map_Ks : rawstring
    map_Ke : rawstring
    map_d  : rawstring
    map_bump : rawstring
    kdTexCoordIdx : int
    ksTexCoordIdx : int
    keTexCoordIdx : int
    normalTexCoordIdx : int
    isConductor : bool
    isFeature5  : bool
    isFeature6  : bool
}

terra Material:init()
    self.name = ""
    self.Ka = make_vec3(0,0,0)
    self.Kd = make_vec3(0,0,0)
    self.Ks = make_vec3(0,0,0)
    self.Ke = make_vec3(0,0,0)
    self.map_Ka = nil
    self.map_Kd = nil
    self.map_Ks = nil
    self.map_Ke = nil
    self.map_d = nil
    self.map_bump = nil
    self.isConductor = false
    self.isFeature5 = false
    self.isFeature6 = false
end

local struct Vertex
{
    p : int;
    n : int;
    b : int; -- bitangent
    t : int; -- texCoord
}

local struct Face
{
    firstVertex : int;
    vertexCount : int;
    material    : int;
}

local struct Mesh
{
    positions : Array(Vec3);
    normals : Array(Vec3);
    bitangents : Array(Vec3);
    texCoords : Array(Vec2[MAX_TEX_COORDS]);
    vertices : Array(Vertex);
    faces : Array(Face);
    materials : Array(Material);
    meshes : Array(tuple(rawstring, int)) -- index into faces array
    toWorld : mat4
}

terra Mesh:init()
    self.positions:init()
    self.normals:init()
    self.bitangents:init()
    self.texCoords:init()
    self.vertices:init()
    self.faces:init()
    self.materials:init()
    self.meshes:init()
end

terra Mesh:delete()
    self.positions:delete()
    self.normals:delete()
    self.texCoords:delete()
    self.vertices:delete()
    self.faces:delete()
    self.materials:delete()
    self.meshes:delete()
end

local terra replaceSlashes(path : rawstring)
  var len = C.strlen(path)
  for i=0, len do
      if path[i] == charLit'\\' then
          path[i] = charLit'/'
      end
  end
end

local terra getDirectoryPath(origPath : rawstring)
  var path = [rawstring](C.malloc(sizeof(int8) * C.strlen(origPath) + 1))
  C.strcpy(path, origPath)

  var lastSlash = C.strrchr(path, charLit'/')
  if lastSlash == nil then
      lastSlash = C.strrchr(path, charLit'\\')
  end

  if lastSlash ~= nil then
      path[lastSlash-path+1] = charLit'\0'
  end

  return path
end

local function checkMaterialPropertyError(fn, property)
  return quote
    if not ai.aiReturn_SUCCESS == [fn] then
      C.printf("[Error] Could not retrieve \"%s\" from material\n", property)
    end
  end
end

local terra getColor(matlAI : &ai.aiMaterial, keyString : rawstring, keyType : uint32, keyIdx : uint32)
  var colorAI : ai.aiColor4D
  [checkMaterialPropertyError(`ai.aiGetMaterialColor(matlAI, keyString, keyType, keyIdx, &colorAI), keyString)]
  return make_vec3(colorAI.r, colorAI.g, colorAI.b)
end

local terra getFloat(matlAI : &ai.aiMaterial, keyString : rawstring, keyType : uint32, keyIdx : uint32)
  var floatAI : ai.ai_real
  [checkMaterialPropertyError(`ai.aiGetMaterialFloatArray(matlAI, keyString, keyType, keyIdx, &floatAI, nil), keyString)]
  return floatAI
end

local terra getInt(matlAI : &ai.aiMaterial, keyString : rawstring, keyType : uint32, keyIdx : uint32)
  var intAI : int
  [checkMaterialPropertyError(`ai.aiGetMaterialIntegerArray(matlAI, keyString, keyType, keyIdx, &intAI, nil), keyString)]
  return intAI
end

local terra getTexturePath(dirPath : rawstring, matlAI : &ai.aiMaterial, textureTypeAI : int, typeName : rawstring)
  var numTextures = ai.aiGetMaterialTextureCount(matlAI, textureTypeAI)
  if numTextures == 0 then
    return nil
  end

  if numTextures > 1 then
    C.printf("[Warning] Material has multiple %s textures, but only one texture is supported", typeName)
  end

  var pathAI : ai.aiString
  [checkMaterialPropertyError(`ai.aiGetMaterialTexture(matlAI, textureTypeAI, 0, &pathAI,
    nil, nil, nil, nil, nil, nil), typeName)]

  var length = C.strlen(dirPath) + pathAI.length + 1
  var path = [rawstring](C.malloc(sizeof(int8) * length))
  C.strcpy(path, dirPath)
  C.strcat(path, pathAI.data)
  replaceSlashes(path)

  return path
end


local function getTextureAndTexCoordIdx(mapField, coordIdxField, textureType, matlAI, dirPath, errString)
  return quote
    mapField = getTexturePath(dirPath, matlAI, textureType, errString)
    if mapField ~= nil then
      coordIdxField = getInt(matlAI, ai.AI_MATKEY_UVWSRC(textureType, 0))
    end
  end
end

local terra convertAIMat(aiMat : &ai.aiMatrix4x4)
  var ret = make_mat4(1)

  ret(0)(0) = aiMat.a1; ret(0)(1) = aiMat.a2; ret(0)(2) = aiMat.a3; ret(0)(3) = aiMat.a4;
  ret(1)(0) = aiMat.b1; ret(1)(1) = aiMat.b2; ret(1)(2) = aiMat.b3; ret(1)(3) = aiMat.b4;
  ret(2)(0) = aiMat.c1; ret(2)(1) = aiMat.c2; ret(2)(2) = aiMat.c3; ret(2)(3) = aiMat.c4;
  ret(3)(0) = aiMat.d1; ret(3)(1) = aiMat.d2; ret(3)(2) = aiMat.d3; ret(3)(3) = aiMat.d4;

  return ret
end

local terra loadMesh(path : rawstring)
  var dirPath = getDirectoryPath(path)

  var importFlags =
    ai.aiProcessPreset_TargetRealtime_MaxQuality or
    ai.aiProcess_OptimizeGraph or
    0;

  var scene = ai.aiImportFile(path, importFlags)

  if scene == nil then
    C.printf("[Error] Could not load model file '%s'\n", path)
    C.free(dirPath)
    return nil
  end

  var root = scene.mRootNode
  if root.mNumChildren ~= 0 then
    C.printf("[Error] Non-flat scene hierarchies not currently supported (used in %s)\n", path)
    C.free(dirPath)
    return nil
  end

  var m = [&Mesh](C.malloc(terralib.sizeof(Mesh)))
  m:init()

  m.toWorld = convertAIMat(&root.mTransformation)

  for i = 0, scene.mNumMeshes do
    var mesh = scene.mMeshes[i]
    var name = [rawstring](C.malloc(sizeof(int8) * mesh.mName.length + 1))
    C.strcpy(name, mesh.mName.data)
    m.meshes:add( { name, m.meshes.count } )

    for j = 0, mesh.mNumFaces do
      var face = mesh.mFaces[j]
      var f : Face
      f.firstVertex = m.vertices.count
      f.vertexCount = face.mNumIndices
      f.material    = mesh.mMaterialIndex
      m.faces:add(f)

      for k = 0, face.mNumIndices do
        var vert : Vertex
        vert.p = m.positions.count + face.mIndices[k]
        vert.n = m.normals.count   + face.mIndices[k]
        vert.b = m.bitangents.count + face.mIndices[k]
        vert.t = m.texCoords.count + face.mIndices[k]
        m.vertices:add(vert)
      end
    end -- end faces

    for j = 0, mesh.mNumVertices do
      var pos = mesh.mVertices[j]
      var normal = mesh.mNormals[j]
      var bitangent = mesh.mBitangents[j]

      m.positions:add( make_vec3(pos.x, pos.y, pos.z) )
      m.normals:add( make_vec3(normal.x, normal.y, normal.z) )
      m.bitangents:add( make_vec3(bitangent.x, bitangent.y, bitangent.z) )

      var texCoords : Vec2[MAX_TEX_COORDS]

      for k = 0, MAX_TEX_COORDS do
        var texCoordPtr = mesh.mTextureCoords[k]

        if texCoordPtr ~= nil then
          var texCoord = texCoordPtr[j]
          texCoords[k] = make_vec2(texCoord.x, -texCoord.y)
        end
      end

      m.texCoords:add(texCoords)
    end -- end vertices
  end -- end meshes

  for i = 0, scene.mNumMaterials do
    var matlAI = scene.mMaterials[i]

    var matl : Material
    matl:init()

    var nameAI : ai.aiString
    ai.aiGetMaterialString(matlAI, ai.AI_MATKEY_NAME, &nameAI)
    matl.name = [rawstring](C.malloc(sizeof(int8) * nameAI.length + 1))
    C.strcpy(matl.name, nameAI.data)

    matl.Ka = getColor(matlAI, ai.AI_MATKEY_COLOR_AMBIENT)
    matl.Kd = getColor(matlAI, ai.AI_MATKEY_COLOR_DIFFUSE)
    matl.Ks = getColor(matlAI, ai.AI_MATKEY_COLOR_SPECULAR)
    matl.Ke = getColor(matlAI, ai.AI_MATKEY_COLOR_EMISSIVE)

    matl.map_Ka = getTexturePath(dirPath, matlAI, ai.aiTextureType_AMBIENT,   "ambient map")
    matl.map_d  = getTexturePath(dirPath, matlAI, ai.aiTextureType_OPACITY,   "opacity map")

    [getTextureAndTexCoordIdx(`matl.map_Kd, `matl.kdTexCoordIdx, `ai.aiTextureType_DIFFUSE,  `matlAI, `dirPath, "diffuse map")]
    [getTextureAndTexCoordIdx(`matl.map_Ks, `matl.ksTexCoordIdx, `ai.aiTextureType_SPECULAR, `matlAI, `dirPath, "specular map")]
    [getTextureAndTexCoordIdx(`matl.map_Ke, `matl.keTexCoordIdx, `ai.aiTextureType_EMISSIVE, `matlAI, `dirPath, "emissive map")]
    [getTextureAndTexCoordIdx(`matl.map_bump, `matl.normalTexCoordIdx, `ai.aiTextureType_NORMALS, `matlAI, `dirPath, "normal map")]

    -- If there isn't a "normal" map, try the "height" map property (OBJ files seem to use this property, even for normal maps)
    if matl.map_bump == nil then
      [getTextureAndTexCoordIdx(`matl.map_bump, `matl.normalTexCoordIdx, `ai.aiTextureType_HEIGHT, `matlAI, `dirPath, "height map")]
    end

    matl.Ns = getFloat(matlAI, ai.AI_MATKEY_SHININESS)
    matl.d  = getFloat(matlAI, ai.AI_MATKEY_OPACITY)
    matl.Tr = 1 - matl.d

    m.materials:add(matl)
  end -- end materials

  C.free(dirPath)
  ai.aiReleaseImport(scene)

  return m
end

return {
  Mesh = Mesh,
  Material = Material,
  loadMesh = loadMesh,
}
