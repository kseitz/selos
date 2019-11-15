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
--
-- Copyright (c) 2015, NVIDIA CORPORATION. All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--  * Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--  * Neither the name of NVIDIA CORPORATION nor the names of its
--    contributors may be used to endorse or promote products derived
--    from this software without specific prior written permission.
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

usingGLSL = true

local C = terralib.includecstring([[
  #include <stdio.h>
  #include <stdlib.h>
]])
local Utils = require "utilities"

local Builtin = {}


local assert = terralib.internalmacro(function(ctx, tree, expr, msg)
  return quote
    if not expr then
      C.printf("%s:%d: Assertion failed: %s\n", [tree.filename], [tree.linenumber], msg)
      C.exit(1)
    end
  end
end)


-- Builtin tables
Builtin.structs = {}
Builtin.types = {}
Builtin.functions = {}
Builtin.operators = {}
Builtin.systemInputs = {}
Builtin.systemOutputs = {}
Builtin.variables = {}

-- keep a table for matrix multiply function because they require special casing
local matrixMult = {}
local indexOperators = {}


-- Functions for querying builtin tables
function Builtin.isBuiltinType(t)
  return Builtin.types[t] ~= nil
end

function Builtin.isBuiltinFunction(t)
  return Builtin.functions[t] ~= nil
end

function Builtin.isBuiltinOperator(t)
  return Builtin.operators[t] ~= nil
end

function Builtin.isBuiltinVariable(v)
  return Builtin.variables[v] ~= nil
end

function Builtin.isMatrixMult(f)
  return matrixMult[f] ~= nil
end

function Builtin.isIndexOperator(f)
  return indexOperators[f] ~= nil
end


-- Functions for adding to the builtin tables
local function addBuiltinFunction(fn, name)
  for _,v in pairs(fn.definitions) do
    Builtin.functions[v] = name or fn.name
  end
end

local function addBuiltinOperator(fn, symbol)
  for _,v in pairs(fn.definitions) do
    Builtin.operators[v] = symbol
  end
end

local function addMatrixMult(fn, symbol)
  addBuiltinOperator(fn, symbol)
  for _,v in pairs(fn.definitions) do
    matrixMult[v] = symbol
  end
end

local function addIndexOperator(fn)
  for _, v in pairs(fn.definitions) do
    indexOperators[v] = true
  end
end


--------------------------------------------------------------------------------
------------------------------ BULTIN TYPES ------------------------------------
--------------------------------------------------------------------------------

-- Builtin texture types
struct sampler2D { x: uint64 }
struct samplerCube { x: uint64 }
struct image2D   { x: uint64 }
struct uimage2D   { x: uint64 }


local function permuteWithReplacement(n, input)
  local ret = terralib.newlist()
  if n == 1 then
    for _, elem in ipairs(input) do
      local l = terralib.newlist({elem})
      ret:insert(l)
    end
  else
    local perms = permuteWithReplacement(n-1, input)
    for _, perm in ipairs(perms) do
      for _, elem in ipairs(input) do
        local l = terralib.newlist({elem})
        for _, v in ipairs(perm) do
          l:insert(v)
        end
        ret:insert(l)
      end
    end
  end
  return ret
end

-- Generate builtin vector types
-- Implements the following for these types:
--   swizzles
local vecTypes = { {"vec", float} , {"ivec", int},
                   {"uvec", uint}, {"bvec", bool}
                 }
local vecFieldsXYZW = { "x", "y", "z", "w" }
local vecFieldsRGBA = { "r", "g", "b", "a" }
local vecFieldsSTPQ = { "s", "t", "p", "q" }

local vecFieldMapping = {}
for idx, realFieldName in ipairs(vecFieldsXYZW) do
  vecFieldMapping[vecFieldsXYZW[idx]] = realFieldName
  vecFieldMapping[vecFieldsRGBA[idx]] = realFieldName
  vecFieldMapping[vecFieldsSTPQ[idx]] = realFieldName
end


for _, vecTypeTuple in ipairs(vecTypes) do
  for i = 2, 4 do
    local vecType = vecTypeTuple[1]
    local elemType = vecTypeTuple[2]

    local typename = vecType .. tostring(i)
    local newType = terralib.types.newstruct(typename)
    local swizzles = terralib.newlist()

    for j = 1, i do
      newType.entries:insert( {field = vecFieldsXYZW[j], type = elemType} )
    end

    -- Create mappings for rgba and stpq
    for idx = 1, #vecFieldsXYZW do
      local rgba = vecFieldsRGBA[idx]
      swizzles[rgba] = function(myobj)
        return `myobj.[vecFieldMapping[rgba]]
      end

      local stpq = vecFieldsSTPQ[idx]
      swizzles[stpq] = function(myobj)
        return `myobj.[vecFieldMapping[stpq]]
      end
    end

    -- Create swizzles
    for j = 2, 4 do
      local perms = terralib.newlist()

      perms:insertall(permuteWithReplacement(j, Utils.subrange(vecFieldsXYZW, 1, i)))
      perms:insertall(permuteWithReplacement(j, Utils.subrange(vecFieldsRGBA, 1, i)))
      perms:insertall(permuteWithReplacement(j, Utils.subrange(vecFieldsSTPQ, 1, i)))

      for _, perm in ipairs(perms) do
        local swiz = ""
        for _, v in ipairs(perm) do
          swiz = swiz .. v
        end

        local function swizzleVecInit(myobj)
          local ret = terralib.newlist()
          for _,v in ipairs(perm) do
            ret:insert(`myobj.[vecFieldMapping[v]])
          end
          return ret
        end

        swizzles[swiz] = function(myobj)
          -- if swizzling the result of a function call, store result
          -- into a temporary, then perform the swizzle on the temporary
          if myobj.tree:is "apply" then
            return quote
              var temp = myobj
            in
              [_G[vecType .. tostring(j)]] { [swizzleVecInit(temp)] }
            end
          else
            return `[_G[vecType .. tostring(j)]] { [swizzleVecInit(myobj)] }
          end
        end
      end
    end

    newType.metamethods.__entrymissing = macro(function(entryname, self)
      return swizzles[entryname](self)
    end)

    -- add some dummy entries to the `metamethods` table
    newType.metamethods.isShaderVectorType = true;
    newType.metamethods.elementCount = i;
    newType.metamethods.elementType = elemType;

    -- operator overloads
    local function addOperatorDefinition(func, args, initList)
      func:adddefinition(terra([args])
        return [newType] { [initList] }
      end)
    end

    local function addUnaryOperator(fnName, opString, op)
      local opImpl = terralib.overloadedfunction(typename .. ".metamethods." .. fnName)
      newType.metamethods[fnName] = opImpl

      local arg = symbol(newType, "v")
      local init = terralib.newlist()

      for j = 1, i do
        init:insert(op(`arg.[vecFieldsXYZW[j]]))
      end

      addOperatorDefinition(opImpl, arg, init)
      addBuiltinOperator(opImpl, opString)
    end

    local function addBinaryOperator(fnName, opString, op)
      local opImpl = terralib.overloadedfunction(typename .. ".metamethods." .. fnName)
      newType.metamethods[fnName] = opImpl

      -- overloads for vec op vec, vec op scalar, and scalar op vec, respectively
      local args = { terralib.newlist(), terralib.newlist(), terralib.newlist() }
      local init = { terralib.newlist(), terralib.newlist(), terralib.newlist() }
      args[1]:insert(symbol(newType, "v1"))
      args[1]:insert(symbol(newType, "v2"))
      args[2]:insert(symbol(newType, "v"))
      args[2]:insert(symbol(elemType, "s"))
      args[3]:insert(symbol(elemType, "s"))
      args[3]:insert(symbol(newType, "v"))

      for j = 1, i do
        init[1]:insert(op(`[args[1][1]].[vecFieldsXYZW[j]], `[args[1][2]].[vecFieldsXYZW[j]]))
        init[2]:insert(op(`[args[2][1]].[vecFieldsXYZW[j]], `[args[2][2]]))
        init[3]:insert(op(`[args[3][1]], `[args[3][2]].[vecFieldsXYZW[j]]))
      end

      addOperatorDefinition(opImpl, args[1], init[1])
      addOperatorDefinition(opImpl, args[2], init[2])
      addOperatorDefinition(opImpl, args[3], init[3])

      addBuiltinOperator(opImpl, opString)
    end

    if vecType ~= "bvec" then
      local op = function(lhs, rhs) return `lhs + rhs end
      addBinaryOperator("__add", "+", op)
      local op = function(lhs, rhs) return `lhs - rhs end
      addBinaryOperator("__sub", "-", op)
      local op = function(lhs, rhs) return `lhs * rhs end
      addBinaryOperator("__mul", "*", op)
      local op = function(lhs, rhs) return `lhs / rhs end
      addBinaryOperator("__div", "/", op)

      local op = function(rhs) return `-rhs end
      addUnaryOperator("__unm", "-", op)
    end


    local vector_index_helper = terralib.overloadedfunction("vector_index_helper", {
      terra(v : &newType, idx : int)
        return [&elemType]([&int8](v) + idx * sizeof(elemType))
      end
    })
    addIndexOperator(vector_index_helper)

    newType.metamethods.__apply = macro(function(self, idx)
      return `@vector_index_helper(&self, idx)
    end)

    _G[typename] = newType
  end
end

-- Generate vector constructors
-- Note: Constructor function generation must be separate from the loop above
--       so that we can use the other vector types as constructor arguments
for _, vecTypeTuple in ipairs(vecTypes) do
  for i = 2, 4 do
    local vecType = vecTypeTuple[1]
    local elemType = vecTypeTuple[2]

    local typename = vecType .. tostring(i)

    local constructor = terralib.overloadedfunction("make_" .. typename)
    _G[constructor.name] = constructor

    local function addConstructorDefintion(args, initList)
      constructor:adddefinition(terra([args])
        return [_G[typename]] { [initList] }
      end)
    end

    -- One scalar argument
    local args = terralib.newlist()
    local initList = terralib.newlist()
    args:insert(symbol(elemType, "s"))
    for j = 1, i do
      initList:insert(args[1])
    end
    addConstructorDefintion(args, initList)

    -- One scalar argument per vector element
    local args = terralib.newlist()
    local initList = args
    for j = 1, i do
      args:insert(symbol(elemType, "s" .. j))
    end
    addConstructorDefintion(args, initList)

    -- One vector argument of size > current vector size
    for j = i+1, #vecTypes do
      local args = terralib.newlist()
      local initList = terralib.newlist()
      args:insert(symbol(_G[vecType .. j], "v"))
      for k = 1, i do
        initList:insert(`[args[1]].[vecFieldsXYZW[k]])
      end
      addConstructorDefintion(args, initList)
    end

    -- One vector argument of size < current vector, plus scalars for the rest
    for j = 2, i-1 do
      local args = terralib.newlist()
      local initList = terralib.newlist()
      args:insert(symbol(_G[vecType .. j], "v"))
      for k = 1, j do
        initList:insert(`[args[1]].[vecFieldsXYZW[k]])
      end
      for k = j+1, i do
        local arg = symbol(elemType, "s" .. k)
        args:insert(arg)
        initList:insert(arg)
      end
      addConstructorDefintion(args, initList)
    end

    if usingGLSL then
      local targetTypename = typename
      addBuiltinFunction(constructor, targetTypename)
    end
  end
end

-- Special uvec2 constructor
-- TODO verify this implementation
make_vec2:adddefinition(terra(s: sampler2D)
  return uvec2 { (s.x and 0xFFFFFFFF), ((s.x >> 32) and 0xFFFFFFFF) }
end)

-- matrix types
for i = 2, 4 do
  for j = 2, 4 do
    local typename = "mat" .. tostring(i) .. "x" .. tostring(j)
    local newType = terralib.types.newstruct(typename)
    local vType = _G["vec" .. tostring(j)]
    local elemType = float

    newType.entries:insert( { field = "rows", type = terralib.types.array(vType, i) } )

    -- add some dummy entries to the `metamethods` table
    newType.metamethods.isShaderMatrixType = true;
    newType.metamethods.colCount = i;
    newType.metamethods.rowCount = j;
    newType.metamethods.colType = vType
    newType.metamethods.elementType = vType.metamethods.elementType;

    _G[typename] = newType

    if i == j then
      _G["mat" .. tostring(i)] = newType
    end

    -- operator overloads
    local function addOperatorDefinition(func, args, initList)
      func:adddefinition(terra([args])
        return [newType] { array([initList]) }
      end)
    end

    local function addBinaryOperator(fnName, opString, op)
      local opImpl = terralib.overloadedfunction(typename .. ".metamethods." .. fnName)
      newType.metamethods[fnName] = opImpl

      -- overloads for mat op mat, mat op scalar, and scalar op mat, respectively
      local args = { terralib.newlist(), terralib.newlist(), terralib.newlist() }
      local init = { terralib.newlist(), terralib.newlist(), terralib.newlist() }
      args[1]:insert(symbol(newType, "m1"))
      args[1]:insert(symbol(newType, "m2"))
      args[2]:insert(symbol(newType, "m"))
      args[2]:insert(symbol(elemType, "s"))
      args[3]:insert(symbol(elemType, "s"))
      args[3]:insert(symbol(newType, "m"))

      for k = 1, i do
        init[1]:insert(op(`[args[1][1]].rows[k-1], `[args[1][2]].rows[k-1]))
        init[2]:insert(op(`[args[2][1]].rows[k-1], `[args[2][2]]))
        init[3]:insert(op(`[args[3][1]], `[args[3][2]].rows[k-1]))
      end

      if fnName ~= "__mul" then
        addOperatorDefinition(opImpl, args[1], init[1])
      end
      addOperatorDefinition(opImpl, args[2], init[2])
      addOperatorDefinition(opImpl, args[3], init[3])

      addBuiltinOperator(opImpl, opString)
    end

    local op = function(lhs, rhs) return `lhs + rhs end
    addBinaryOperator("__add", "+", op)
    local op = function(lhs, rhs) return `lhs - rhs end
    addBinaryOperator("__sub", "-", op)
    local op = function(lhs, rhs) return `lhs * rhs end
    addBinaryOperator("__mul", "*", op)
    local op = function(lhs, rhs) return `lhs / rhs end
    addBinaryOperator("__div", "/", op)

    local matrix_index_helper = terralib.overloadedfunction("matrix_index_helper", {
      terra(m : &newType, idx : int)
        return &(m.rows[idx])
      end
    })
    addIndexOperator(matrix_index_helper)

    newType.metamethods.__apply = macro(function(self, idx)
      return `@matrix_index_helper(&self, idx)
    end)
  end
end

-- Generate matrix constructors
-- TODO add other constructor types
for i = 2, 4 do
  for j = 2, 4 do
    local typename = "mat" .. tostring(i) .. "x" .. tostring(j)
    local vType = _G["vec" .. tostring(j)]
    local elemType = float

    local constructor = terralib.overloadedfunction("make_" .. typename)
    _G[constructor.name] = constructor

    if i == j then
      _G["make_mat" .. tostring(i)] = constructor
    end

    local function addConstructorDefintion(args, initList)
      constructor:adddefinition(terra([args])
        return [_G[typename]] { array([initList]) }
      end)
    end

    -- One scalar argument
    local args = terralib.newlist()
    local initList = terralib.newlist()
    args:insert(symbol(elemType, "s"))
    for k = 1, i do
      local vecInit = terralib.newlist()
      for l = 1, j do
        if k == l then
          vecInit:insert(args[1])
        else
          vecInit:insert(`0.0f)
        end
      end
      initList:insert(`vType{[vecInit]})
    end
    addConstructorDefintion(args, initList)

    -- i*j scalar arguments
    local args = terralib.newlist()
    local initList = terralib.newlist()
    for k = 1, i*j do
      args:insert(symbol(elemType, "s" .. k))
    end
    local idx = 1
    for k = 1, i do
      local vecInit = terralib.newlist()
      for l = 1, j do
        vecInit:insert(args[idx])
        idx = idx + 1
      end
      initList:insert(`vType{[vecInit]})
    end
    addConstructorDefintion(args, initList)


    if usingGLSL then
      local targetTypename = typename
      addBuiltinFunction(constructor, targetTypename)
    end
  end
end


-- uvec2 operators
uvec2.metamethods.__ne = terralib.overloadedfunction("uvec2.metamethods.__ne")
uvec2.metamethods.__ne:adddefinition(terra(v1: uvec2, v2: uvec2)
  return (v1.x ~= v2.x) and (v1.y ~= v2.y)
end)

uvec2.metamethods.__ne:adddefinition(terra(v: uvec2, s: uint)
  return (v.x ~= s) and (v.y ~= s)
end)


-- mat3x3 operators
make_mat3x3:adddefinition(terra(val: mat4x4)
  assert(false, "Unimplemented builtin")
  return mat3x3{} -- TODO: implement
end)

mat3x3.metamethods.__mul:adddefinition(terra(a: mat3x3, b: mat3x3)
  assert(false, "Unimplemented builtin")
  return mat3x3{} -- TODO: implement
end)

mat3x3.metamethods.__mul:adddefinition(terra(a: mat3x3, b: vec3)
  assert(false, "Unimplemented builtin")
  return vec3{} -- TODO: implement
end)

-- mat4 operators
mat4.metamethods.__mul:adddefinition(terra(a: mat4, b: mat4)
  var aa: float[4][4]
  var bb: float[4][4]
  var cc: float[4][4]

  for i=0, 4 do
    aa[i][0] = a.rows[i].x
    aa[i][1] = a.rows[i].y
    aa[i][2] = a.rows[i].z
    aa[i][3] = a.rows[i].w
    bb[i][0] = b.rows[i].x
    bb[i][1] = b.rows[i].y
    bb[i][2] = b.rows[i].z
    bb[i][3] = b.rows[i].w
  end

  for i=0, 4 do
    for j=0, 4 do
      cc[i][j] = 0
      for k = 0, 4 do
        cc[i][j] = cc[i][j] + aa[i][k] * bb[k][j]
      end
    end
  end

  var ret: mat4
  for i=0, 4 do
    ret.rows[i].x = cc[i][0]
    ret.rows[i].y = cc[i][1]
    ret.rows[i].z = cc[i][2]
    ret.rows[i].w = cc[i][3]
  end

  return ret
end)

if usingGLSL then
  Builtin.structs[vec2] = "vec2"
  Builtin.structs[vec3] = "vec3"
  Builtin.structs[vec4] = "vec4"
  Builtin.structs[ivec2] = "ivec2"
  Builtin.structs[ivec3] = "ivec3"
  Builtin.structs[ivec4] = "ivec4"
  Builtin.structs[uvec2] = "uvec2"
  Builtin.structs[uvec3] = "uvec3"
  Builtin.structs[uvec4] = "uvec4"
  Builtin.structs[bvec2] = "bvec2"
  Builtin.structs[bvec3] = "bvec3"
  Builtin.structs[bvec4] = "bvec4"

  Builtin.structs[mat3x3] = "mat3"
  Builtin.structs[mat4x3] = "mat4x3"
  Builtin.structs[mat4x4] = "mat4"
  Builtin.structs[sampler2D] = "sampler2D"
  Builtin.structs[samplerCube] = "samplerCube"
  Builtin.structs[image2D] = "image2D"
  Builtin.structs[uimage2D] = "uimage2D"
end

if usingGLSL then
  Builtin.types[float]  = "float"
  Builtin.types[int]    = "int"
  Builtin.types[uint]   = "uint"
  Builtin.types[bool]   = "bool"
end

for k,v in pairs(Builtin.structs) do
  Builtin.types[k] = v
end

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
------------------------------ BULTIN FUNCTIONS --------------------------------
--------------------------------------------------------------------------------

local createBuiltin = {}

local function addFunctionDefinition(func, args, body, returnType)
  func:adddefinition(terra([args]) : returnType
    [body]
  end)
end

-- returnType is optional.  If nil, the generated function's return type will
-- be the same type as the argument.
function createBuiltin.oneGenTypeArg(fn, scalarTypes, vecTypes, bodyFn, returnType)
  for _, scalarType in ipairs(scalarTypes) do
    local arg = symbol(scalarType, "s")
    local body = bodyFn(arg)
    addFunctionDefinition(fn, arg, body, returnType or scalarType)
  end

  for _, vecType in ipairs(vecTypes) do
    for i = 2, 4 do
      local arg = symbol(_G[vecType .. i], "v")
      local body = bodyFn(arg, i)
      addFunctionDefinition(fn, arg, body, returnType or _G[vecType .. i])
    end
  end

  if usingGLSL then
    addBuiltinFunction(fn)
  end
end

any = terralib.overloadedfunction("any")
local vecTypes = { "bvec" }
local bodyFn = function(arg, numElems)
  local ret = `[arg].[vecFieldsXYZW[1]]
  for i = 2, numElems do
    ret = `[ret] or [arg].[vecFieldsXYZW[i]]
  end
  return quote return [ret] end
end
createBuiltin.oneGenTypeArg(any, {}, vecTypes, bodyFn, bool)

abs = terralib.overloadedfunction("abs")
local scalarTypes = { float, int }
local vecTypes = { "vec", "ivec" }
local bodyFn = function(arg, numElems)
  if not numElems then
    return quote
      if [arg] >= 0 then return [arg] else return -[arg] end
    end
  end

  local elems = terralib.newlist()
  for i = 1, numElems do
    elems:insert(`abs([arg].[vecFieldsXYZW[i]]))
  end

  return quote return [arg.type] { [elems] } end
end
createBuiltin.oneGenTypeArg(abs, scalarTypes, vecTypes, bodyFn)

local builtins = {}

local function declareBuiltinFunc(name)
  local val = builtins[name];
  if val then return val end

  local fn = terralib.overloadedfunction(name)
  builtins[name] = fn
  return fn
end

local function createBuiltinParams(paramSepcs)
  local params = {}
  for _,v in ipairs(paramSepcs) do
    local name, type = unpack(v)
    local param = symbol(type, name)
    table.insert(params, param)
  end
  return params
end

function createBuiltinFunc(name, resultType, paramSpecs, body)
  local fn = declareBuiltinFunc(name)

  local params = createBuiltinParams(paramSpecs)
  local def = terra([params]) : resultType
    [body(unpack(params))]
  end

  fn:adddefinition(def:getdefinitions()[1])
end

function createVectorBuiltinBody(scalarFunc, elemCount, vectorType, params)
  local results = {}
  for i = 1, elemCount do
    local args = {}
    for _,p in ipairs(params) do
      table.insert(args, `(p.[vecFieldsXYZW[i]]))
    end
    local result = `scalarFunc([args])
    table.insert(results, result)
  end
  return quote return [vectorType] { [results] } end
end

function createVectorBuiltins(scalarFunc, paramNames, ...)
  local prefixes = { ... }
  for _,prefix in ipairs(prefixes) do
    for i = 2, 4 do
      local vectorType = _G[prefix .. i] -- TODO: don't use global scope here

      local params = {}
      for i,paramName in ipairs(paramNames) do
        params[i] = symbol(vectorType, paramName)
      end

      scalarFunc:adddefinition(terra([params]) : vectorType
        [createVectorBuiltinBody(scalarFunc, i, vectorType, params)]
      end)

      addBuiltinFunction(scalarFunc)
    end
  end
end

local Cmath = terralib.includec("math.h")
local builtinFunctions = {}

function simpleBuiltin(name, paramNames, bodyFn)
  local fn = terralib.overloadedfunction(name)

  local params = {}
  for i,p in ipairs(paramNames) do
    params[i] = symbol(float, p)
  end

  fn:adddefinition(terra([params]) : float
    return [bodyFn(unpack(params))]
  end)

  createVectorBuiltins(fn, paramNames, "vec")
  builtinFunctions[name] = fn
end


function simpleUnaryBuiltin(name, bodyFn)
  local fn = terralib.overloadedfunction(name)
  fn:adddefinition(terra(val: float): float
    return [bodyFn(val)]
  end)
  createVectorBuiltins(fn, {"val"}, "vec")
  builtinFunctions[name] = fn
end

function simpleUnaryBuiltinMath(name, mathName)
  simpleUnaryBuiltin(name, function(val)
    return `([Cmath[mathName]](val))
  end)
end

function simpleBinaryBuiltin(name, bodyFn)
  local fn = terralib.overloadedfunction(name)
  fn:adddefinition(terra(x: float, y: float): float
    return [bodyFn(x, y)]
  end)
  createVectorBuiltins(fn, {"x", "y"}, "vec")
  builtinFunctions[name] = fn
end

function simpleBinaryBuiltinMath(name, mathName)
  simpleBinaryBuiltin(name, function(x, y)
    return `([Cmath[mathName]](x, y))
  end)
end

-- GLSL Spec Section 8.1 - Angle and Trigonometry Functions

simpleUnaryBuiltin("radians", function(val)
  return `( [math.pi / 180] * val )
end)

simpleUnaryBuiltin("degrees", function(val)
  return `( [180.0 / math.pi] * val )
end)

simpleUnaryBuiltinMath("sin", "sinf")
simpleUnaryBuiltinMath("cos", "cosf")
simpleUnaryBuiltinMath("tan", "tanf")
simpleUnaryBuiltinMath("asin", "asinf")
simpleUnaryBuiltinMath("acos", "acosf")

-- TODO: atan has two variations in GLSL, need to provide both
simpleUnaryBuiltinMath("atan", "atanf")
--simpleBinaryBuiltinMath("atan", "atanf")

simpleUnaryBuiltinMath("sinh", "sinhf")
simpleUnaryBuiltinMath("cosh", "coshf")
simpleUnaryBuiltinMath("tanh", "tanhf")
simpleUnaryBuiltinMath("asinh", "asinhf")
simpleUnaryBuiltinMath("acosh", "acoshf")
simpleUnaryBuiltinMath("atanh", "atanhf")

-- GLSL Spec Section 8.1 - Exponential Functions

simpleBinaryBuiltinMath("pow", "powf")
simpleUnaryBuiltinMath("sqrt", "sqrtf")
simpleUnaryBuiltinMath("exp", "expf")
simpleUnaryBuiltinMath("exp2", "exp2f")
simpleUnaryBuiltinMath("log2", "log2f")

simpleBuiltin("mix", {"x", "y", "a"}, function(x,y,a)
  return `(x*(1-a) + y*a)
end)
for _,V in ipairs({vec2, vec3, vec4}) do
  builtinFunctions.mix:adddefinition(terra(x: V, y: V, a: float) : V
    return x*(1-a) + y*a
  end)
end

builtinFunctions["lerp"] = builtinFunctions.mix

simpleBuiltin("max", {"x", "y"}, function(x,y)
  return quote
    var tmpX = x
    var tmpY = y
    if tmpY > tmpX then
      tmpX = tmpY
    end
  in
    tmpX
  end
end)
for _, p in ipairs({"vec2", "vec3", "vec4"}) do
  V = _G[p]
  builtinFunctions.max:adddefinition(terra(x: V, y: float) : V
    return builtinFunctions.max(x, [_G["make_" .. p]](y))
  end)
end

simpleBuiltin("min", {"x", "y"}, function(x,y)
  return quote
    var tmpX = x
    var tmpY = y
    if tmpY < tmpX then
      tmpX = tmpY
    end
  in
    tmpX
  end
end)
builtinFunctions.min:adddefinition(terra(x: int, y: int) : int
  if x < y then return x
  else return y
  end
end)
for _, p in ipairs({"vec2", "vec3", "vec4"}) do
  V = _G[p]
  builtinFunctions.min:adddefinition(terra(x: V, y: float) : V
    return builtinFunctions.min(x, [_G["make_" .. p]](y))
  end)
end

-- TODO: these should be defined in builtins file...
simpleBuiltin("clamp", {"value", "low", "high"}, function(value, low, high)
  return `builtinFunctions.max(low, builtinFunctions.min(value, high))
end)
for _, V in ipairs({vec2, vec3, vec4}) do
  builtinFunctions.clamp:adddefinition(terra(value: V, low: float, high: float)
    return builtinFunctions.min(builtinFunctions.max(value, low), high)
  end)
end

simpleBuiltin("smoothstep", {"edge0", "edge1", "x"}, function(edge0, edge1, x)
  return quote
    var t = builtinFunctions.clamp((x - edge0) / (edge1 - edge0), 0, 1)
  in
    t * t * (3.0f - 2.0f * t)
  end
end)
for _, V in ipairs({vec2, vec3, vec4}) do
  builtinFunctions.smoothstep:adddefinition(terra(edge0: float, edge1: float, x: V)
    var t = builtinFunctions.clamp((x - edge0) / (edge1 - edge0), 0, 1)
    return t * t * (3.0f - 2.0f * t)
  end)
end

simpleBuiltin("saturate", {"value"}, function(value)
  return `builtinFunctions.clamp(value, 0, 1)
end)


-- fake builtins for derivatives
simpleUnaryBuiltin("dFdx", function(value) return `0 end);
simpleUnaryBuiltin("dFdy", function(value) return `0 end);


--

local dot = terralib.overloadedfunction("dot")
local function createDotBody(left, right, elemCount)
  if elemCount == 1 then
    return `( left.x * right.x )
  else
    return `([createDotBody(left,right,elemCount-1)]
      + left.[vecFieldsXYZW[elemCount]] * right.[vecFieldsXYZW[elemCount]])
  end
end
local function createDot(S, p)
  for i = 2,4 do
    local V = _G[p .. i]
    dot:adddefinition(terra( left: V, right: V ) : S
      return [createDotBody(left, right, i)]
    end)
  end
end
createDot(float, "vec")
createDot(int, "ivec")
createDot(uint, "uvec")
builtinFunctions.dot = dot


-- Matrix builtin functions

mat4.metamethods.__mul:adddefinition(terra(a: mat4, b: vec4)
  var ret : vec4
  for i=0, 4 do
    ret(i) = dot(a.rows[i], b)
  end
  return ret
end)


local transpose = terralib.overloadedfunction("transpose")
builtinFunctions.transpose = transpose
for i=2,4 do
  for j=2,4 do
    local matType = _G["mat" .. tostring(i) .. "x" .. tostring(j)]
    builtinFunctions.transpose:adddefinition(terra(m : matType)
      var ret = [_G["make_mat" .. tostring(j) .. "x" .. tostring(i)]](0.0f)
      for r=0,i do
        for c=0,j do
          ret(c)(r) = m(r)(c)
        end
      end
      return ret
    end)
  end
end

local determinant = terralib.overloadedfunction("determinant")
determinant:adddefinition(terra(m : mat2) : float
  var a = m(0)(0)
  var b = m(0)(1)
  var c = m(1)(0)
  var d = m(1)(1)
  return  a*d - b*c
end)

determinant:adddefinition(terra(m : mat3) : float
  var a = m(0)(0)
  var b = m(0)(1)
  var c = m(0)(2)
  var d = m(1)(0)
  var e = m(1)(1)
  var f = m(1)(2)
  var g = m(2)(0)
  var h = m(2)(1)
  var i = m(2)(2)
  return a*e*i + b*f*g + c*d*h - c*e*g - b*d*i - a*f*h
end)

determinant:adddefinition(terra(m : mat4) : float
  var a,b,c,d = m(0)(0), m(0)(1), m(0)(2), m(0)(3)
  var e,f,g,h = m(1)(0), m(1)(1), m(1)(2), m(1)(3)
  var i,j,k,l = m(2)(0), m(2)(1), m(2)(2), m(2)(3)
  var m,n,o,p = m(3)(0), m(3)(1), m(3)(2), m(3)(3)

  var m0 = make_mat3(f,g,h,j,k,l,n,o,p)
  var m1 = make_mat3(e,g,h,i,k,l,m,o,p)
  var m2 = make_mat3(e,f,h,i,j,l,m,n,p)
  var m3 = make_mat3(e,f,g,i,j,k,m,n,o)

  return a*determinant(m0) - b*determinant(m1) + c*determinant(m2) - d*determinant(m3)
end)

local threshold = 0.000001

local inverse = terralib.overloadedfunction("inverse")
inverse:adddefinition(terra(m : mat2) : mat2
  var det = determinant(m)
  if det < threshold and det > -threshold then
    return make_mat2(0.0f)
  end

  var ret = make_mat2(m(1)(1), -m(0)(1),
                     -m(1)(0),  m(0)(0))
  return (1.0f/det) * ret
end)

inverse:adddefinition(terra(m : mat3) : mat3
  var det = determinant(m)
  if det < threshold and det > -threshold then
    return make_mat3(0.0f)
  end

  var m00 = determinant(make_mat2(m(1)(1), m(1)(2), m(2)(1), m(2)(2)))
  var m01 = determinant(make_mat2(m(0)(2), m(0)(1), m(2)(2), m(2)(1)))
  var m02 = determinant(make_mat2(m(0)(1), m(0)(2), m(1)(1), m(1)(2)))

  var m10 = determinant(make_mat2(m(1)(2), m(1)(0), m(2)(2), m(2)(0)))
  var m11 = determinant(make_mat2(m(0)(0), m(0)(2), m(2)(0), m(2)(2)))
  var m12 = determinant(make_mat2(m(0)(2), m(0)(0), m(1)(2), m(1)(0)))

  var m20 = determinant(make_mat2(m(1)(0), m(1)(1), m(2)(0), m(2)(1)))
  var m21 = determinant(make_mat2(m(0)(1), m(0)(0), m(2)(1), m(2)(0)))
  var m22 = determinant(make_mat2(m(0)(0), m(0)(1), m(1)(0), m(1)(1)))

  var ret = make_mat3(m00, m01, m02,
                      m10, m11, m12,
                      m20, m21, m22)
  return (1.0f/det) * ret
end)

inverse:adddefinition(terra(m : mat4) : mat4
  var det = determinant(m)
  if det < threshold and det > -threshold then
    return make_mat4(0.0f)
  end

  var m00 = m(1)(2)*m(2)(3)*m(3)(1) - m(1)(3)*m(2)(2)*m(3)(1) + m(1)(3)*m(2)(1)*m(3)(2)
          - m(1)(1)*m(2)(3)*m(3)(2) - m(1)(2)*m(2)(1)*m(3)(3) + m(1)(1)*m(2)(2)*m(3)(3)
  var m01 = m(0)(3)*m(2)(2)*m(3)(1) - m(0)(2)*m(2)(3)*m(3)(1) - m(0)(3)*m(2)(1)*m(3)(2)
          + m(0)(1)*m(2)(3)*m(3)(2) + m(0)(2)*m(2)(1)*m(3)(3) - m(0)(1)*m(2)(2)*m(3)(3)
  var m02 = m(0)(2)*m(1)(3)*m(3)(1) - m(0)(3)*m(1)(2)*m(3)(1) + m(0)(3)*m(1)(1)*m(3)(2)
          - m(0)(1)*m(1)(3)*m(3)(2) - m(0)(2)*m(1)(1)*m(3)(3) + m(0)(1)*m(1)(2)*m(3)(3)
  var m03 = m(0)(3)*m(1)(2)*m(2)(1) - m(0)(2)*m(1)(3)*m(2)(1) - m(0)(3)*m(1)(1)*m(2)(2)
          + m(0)(1)*m(1)(3)*m(2)(2) + m(0)(2)*m(1)(1)*m(2)(3) - m(0)(1)*m(1)(2)*m(2)(3)
  var m10 = m(1)(3)*m(2)(2)*m(3)(0) - m(1)(2)*m(2)(3)*m(3)(0) - m(1)(3)*m(2)(0)*m(3)(2)
          + m(1)(0)*m(2)(3)*m(3)(2) + m(1)(2)*m(2)(0)*m(3)(3) - m(1)(0)*m(2)(2)*m(3)(3)
  var m11 = m(0)(2)*m(2)(3)*m(3)(0) - m(0)(3)*m(2)(2)*m(3)(0) + m(0)(3)*m(2)(0)*m(3)(2)
          - m(0)(0)*m(2)(3)*m(3)(2) - m(0)(2)*m(2)(0)*m(3)(3) + m(0)(0)*m(2)(2)*m(3)(3)
  var m12 = m(0)(3)*m(1)(2)*m(3)(0) - m(0)(2)*m(1)(3)*m(3)(0) - m(0)(3)*m(1)(0)*m(3)(2)
          + m(0)(0)*m(1)(3)*m(3)(2) + m(0)(2)*m(1)(0)*m(3)(3) - m(0)(0)*m(1)(2)*m(3)(3)
  var m13 = m(0)(2)*m(1)(3)*m(2)(0) - m(0)(3)*m(1)(2)*m(2)(0) + m(0)(3)*m(1)(0)*m(2)(2)
          - m(0)(0)*m(1)(3)*m(2)(2) - m(0)(2)*m(1)(0)*m(2)(3) + m(0)(0)*m(1)(2)*m(2)(3)
  var m20 = m(1)(1)*m(2)(3)*m(3)(0) - m(1)(3)*m(2)(1)*m(3)(0) + m(1)(3)*m(2)(0)*m(3)(1)
          - m(1)(0)*m(2)(3)*m(3)(1) - m(1)(1)*m(2)(0)*m(3)(3) + m(1)(0)*m(2)(1)*m(3)(3)
  var m21 = m(0)(3)*m(2)(1)*m(3)(0) - m(0)(1)*m(2)(3)*m(3)(0) - m(0)(3)*m(2)(0)*m(3)(1)
          + m(0)(0)*m(2)(3)*m(3)(1) + m(0)(1)*m(2)(0)*m(3)(3) - m(0)(0)*m(2)(1)*m(3)(3)
  var m22 = m(0)(1)*m(1)(3)*m(3)(0) - m(0)(3)*m(1)(1)*m(3)(0) + m(0)(3)*m(1)(0)*m(3)(1)
          - m(0)(0)*m(1)(3)*m(3)(1) - m(0)(1)*m(1)(0)*m(3)(3) + m(0)(0)*m(1)(1)*m(3)(3)
  var m23 = m(0)(3)*m(1)(1)*m(2)(0) - m(0)(1)*m(1)(3)*m(2)(0) - m(0)(3)*m(1)(0)*m(2)(1)
          + m(0)(0)*m(1)(3)*m(2)(1) + m(0)(1)*m(1)(0)*m(2)(3) - m(0)(0)*m(1)(1)*m(2)(3)
  var m30 = m(1)(2)*m(2)(1)*m(3)(0) - m(1)(1)*m(2)(2)*m(3)(0) - m(1)(2)*m(2)(0)*m(3)(1)
          + m(1)(0)*m(2)(2)*m(3)(1) + m(1)(1)*m(2)(0)*m(3)(2) - m(1)(0)*m(2)(1)*m(3)(2)
  var m31 = m(0)(1)*m(2)(2)*m(3)(0) - m(0)(2)*m(2)(1)*m(3)(0) + m(0)(2)*m(2)(0)*m(3)(1)
          - m(0)(0)*m(2)(2)*m(3)(1) - m(0)(1)*m(2)(0)*m(3)(2) + m(0)(0)*m(2)(1)*m(3)(2)
  var m32 = m(0)(2)*m(1)(1)*m(3)(0) - m(0)(1)*m(1)(2)*m(3)(0) - m(0)(2)*m(1)(0)*m(3)(1)
          + m(0)(0)*m(1)(2)*m(3)(1) + m(0)(1)*m(1)(0)*m(3)(2) - m(0)(0)*m(1)(1)*m(3)(2)
  var m33 = m(0)(1)*m(1)(2)*m(2)(0) - m(0)(2)*m(1)(1)*m(2)(0) + m(0)(2)*m(1)(0)*m(2)(1)
          - m(0)(0)*m(1)(2)*m(2)(1) - m(0)(1)*m(1)(0)*m(2)(2) + m(0)(0)*m(1)(1)*m(2)(2)

  var ret = make_mat4(m00, m01, m02, m03,
                      m10, m11, m12, m13,
                      m20, m21, m22, m23,
                      m30, m31, m32, m33)
  return (1.0f/det) * ret
end)

builtinFunctions.determinant = determinant
builtinFunctions.inverse = inverse

-- import builtin functions into global scope
for k,v in pairs(builtinFunctions) do
  _G[k] = v
  addBuiltinFunction(v)
end


length = terralib.overloadedfunction("length")
length:adddefinition(terra(x: vec2): float
  return sqrt(pow(x.x, 2) + pow(x.y, 2))
end)

length:adddefinition(terra(x: vec3): float
  return sqrt(pow(x.x, 2) + pow(x.y, 2) + pow(x.z, 2))
end)

length:adddefinition(terra(x: vec4): float
  return sqrt(pow(x.x, 2) + pow(x.y, 2) + pow(x.z, 2) + pow(x.w, 2))
end)

distance = terralib.overloadedfunction("distance")
distance:adddefinition(terra(a: vec2, b: vec2): float
  return sqrt(pow(b.x - a.x, 2) + pow(b.y - a.y, 2))
end)

distance:adddefinition(terra(a: vec3, b: vec3): float
  return sqrt(pow(b.x - a.x, 2) + pow(b.y - a.y, 2) + pow(b.z - a.z, 2))
end)

distance:adddefinition(terra(a: vec4, b: vec4): float
  return sqrt(pow(b.x - a.x, 2) + pow(b.y - a.y, 2) + pow(b.z - a.z, 2) + pow(b.w - a.w, 2))
end)

cross = terralib.overloadedfunction("cross")
cross:adddefinition(terra(left: vec3, right: vec3) : vec3
  return make_vec3(left.y*right.z - left.z*right.y, left.z*right.x - left.x*right.z, left.x*right.y - left.y*right.x)
end)

normalize = terralib.overloadedfunction("normalize")
normalize:adddefinition(terra(v: vec2): vec2
  var len = length(v)
  if len ~= 0.0f then
    return make_vec2(v.x/len, v.y/len)
  else
    return make_vec2(0.0)
  end
end)

normalize:adddefinition(terra(v: vec3): vec3
  var len = length(v)
  if len ~= 0.0f then
    return make_vec3(v.x/len, v.y/len, v.z/len)
  else
    return make_vec3(0.0)
  end
end)

normalize:adddefinition(terra(v:vec4): vec4
  var len = length(v)
  if len ~= 0.0f then
    return make_vec4(v.x/len, v.y/len, v.z/len, v.x/len)
  else
    return make_vec4(0.0)
  end
end)

texture = terralib.overloadedfunction("texture")
texture:adddefinition(terra(tex: sampler2D, uv: vec2): vec4
end)

texture:adddefinition(terra(tex: samplerCube, uv: vec3): vec4
end)

textureGrad = terralib.overloadedfunction("textureGrad")
textureGrad:adddefinition(terra(tex: sampler2D, uv: vec2, dPdx: vec2, dPdy: vec2 )
end)

textureLod = terralib.overloadedfunction("textureLod")
textureLod:adddefinition(terra(tex: sampler2D, uv: vec2, lod: float): vec4
end)

textureLod:adddefinition(terra(tex: samplerCube, uv: vec3, lod: float): vec4
end)


imageLoad = terralib.overloadedfunction("imageLoad")
imageLoad:adddefinition(terra(image : image2D, xy : ivec2): vec4 end)
imageLoad:adddefinition(terra(image : uimage2D, xy : ivec2): uvec4 end)

imageStore = terralib.overloadedfunction("imageStore")
imageStore:adddefinition(terra(image : image2D, xy : ivec2, color : vec4) end)
imageStore:adddefinition(terra(image : uimage2D, xy : ivec2, color : uvec4) end)

discard = terralib.overloadedfunction("discard")
discard:adddefinition(terra()
end)

groupMemoryBarrier = terralib.overloadedfunction("groupMemoryBarrier")
groupMemoryBarrier:adddefinition(terra()
end)


if usingGLSL then
  addBuiltinFunction(cross)
  addBuiltinFunction(length)
  addBuiltinFunction(distance)
  addBuiltinFunction(normalize)
  addBuiltinFunction(max)
  addBuiltinFunction(sin)
  addBuiltinFunction(cos)
  addBuiltinFunction(texture)
  addBuiltinFunction(textureGrad)
  addBuiltinFunction(textureLod)
  addBuiltinFunction(imageLoad)
  addBuiltinFunction(imageStore)
  addBuiltinFunction(discard)
  addBuiltinFunction(inverse)
  addBuiltinFunction(groupMemoryBarrier)

  addBuiltinOperator(uvec2.metamethods.__ne, "!=")

  addBuiltinFunction(make_mat3x3, "mat3x3")

  addMatrixMult(mat3x3.metamethods.__mul, "*")
  addMatrixMult(mat4.metamethods.__mul, "*")

  addBuiltinFunction(make_uvec2, "uvec2")
end

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
------------------------------ BULTIN VARIABLES --------------------------------
--------------------------------------------------------------------------------

-- TODO: separate by shader type
Builtin.systemInputs["FragCoord"] = global(vec4)
Builtin.systemInputs["InstanceID"] = global(uint)
Builtin.systemInputs["GlobalInvocationID"] = global(uvec3)
Builtin.systemInputs["LocalInvocationID"] = global(uvec3)
Builtin.systemInputs["LocalInvocationIndex"] = global(int)
Builtin.systemInputs["WorkGroupID"] = global(uvec3)

Builtin.systemOutputs["Position"] = global(vec4)
Builtin.systemOutputs["FragDepth"] = global(float)

if usingGLSL then
  Builtin.variables[Builtin.systemInputs.FragCoord] = "gl_FragCoord"
  Builtin.variables[Builtin.systemInputs.InstanceID] = "gl_InstanceID"
  Builtin.variables[Builtin.systemInputs.GlobalInvocationID] = "gl_GlobalInvocationID"
  Builtin.variables[Builtin.systemInputs.LocalInvocationID] = "gl_LocalInvocationID"
  Builtin.variables[Builtin.systemInputs.LocalInvocationIndex] = "gl_LocalInvocationIndex"
  Builtin.variables[Builtin.systemInputs.WorkGroupID] = "gl_WorkGroupID"

  Builtin.variables[Builtin.systemOutputs.Position] = "gl_Position"
  Builtin.variables[Builtin.systemOutputs.FragDepth] = "gl_FragDepth"
end

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


return Builtin
