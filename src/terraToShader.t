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

-- Import other source files
local Builtin = require("builtin")
local Utils = require "utilities"


local ffi = require("ffi")

-- prefixes for shader inputs and outputs and function outputs
local pf = "_MetaSdr_"
local pfSdrIn  = pf .. "sdrIn"
local pfSdrOut = pf .. "sdrOut"
local pfFnOut  = pf .. "fnOut"

-- rudimentary class system

local Class = {}

local function newClass(base)
  local class = {};
  if base then
    setmetatable(class, {__index = base})
  else
    setmetatable(class, {__index = Class})
  end
  return class
end

function Class:new(...)
  local obj = {};
  setmetatable(obj, {__index = self});
  obj:init(...);
  return obj;  
end

-- define "class" for source emit context
local EmitContext = newClass();

function EmitContext:init()
  self.gShaderFunctionDecls = ""   -- string of function declarations
  self.gFunctions = {}             -- GLSL functions (maps functions to function signature string)
  self.gFunctionsToDefine = {}     -- array of functions whose definitions still need to be emitted
  self.gCurFn = nil                -- current function being defined
  self.gCurFnDefn = {}             -- array of strings to output to implement the current function
  setmetatable(self.gCurFnDefn, {__index = table})

  self.nameToDecl = {}
  self.declToName = {}

  self.gStructs = {}
  self.gStructDecls = terralib.newlist()

  self:addReserved()
  self:addBuiltinStructs()
end

-- name uniquing stuff

-- reserved names
-- TODO populate this list from stuff in builtin*.t
function EmitContext:addReserved()
end

function EmitContext:tryRemap(key, maybeMap)
  local status, ret = pcall(function() return maybeMap[key] end)
  if status and ret then
    return ret
  end
  return key
end

local function replaceDisallowed(name)
  local newName = name

  newName = (newName:gsub("%$", "DS"))
  newName = (newName:gsub("<", "LA"))
  newName = (newName:gsub(">", "RA"))
  newName = (newName:gsub("%.", "DOT"))
  newName = (newName:gsub(" ", "SP"))
  newName = (newName:gsub("\\", "BS"))
  newName = (newName:gsub("%(", "LP"))
  newName = (newName:gsub("%)", "RP"))
  newName = (newName:gsub(":", "CO"))
  newName = (newName:gsub("/", "FWS"))

  return newName
end

function EmitContext:createUniqueName(name)
  local curName = replaceDisallowed(name)

  local suffix = ""
  for i=1,100 do
    local uniqueName =  curName .. suffix

    -- if name is not taken, use it and mark the name as in use by
    -- putting something (e.g., an empty table) in the nameToDecl table
    if self.nameToDecl[uniqueName] == nil then
      self.nameToDecl[uniqueName] = {}
      return uniqueName
    end

    suffix = "_" .. i
  end

  error("Unable to unique name '" .. curName ..
    "' in a reasonable amount of time\n")
end

-- TODO add replace for if user names contain my internal prefix
-- TODO handle double underscore
-- TODO separate getName vs. createName
--      Have getName error out saying that user is referring to something
--      that they don't have access to if that decl->name mapping doesn't already exist
-- Params:
--  node: AST node to name
--  name: (optional) name to use (and uniqueify if necessary) if node does not already have a name
function EmitContext:getName(node, name)
  -- decl is a builtin function
  if Builtin.isBuiltinFunction(node) then
    return self:tryRemap(Builtin.functions[node], self.builtinFunctionMap)
  end

  if Builtin.isBuiltinVariable(node) then
    local builtinVar = Builtin.variables[node];
    self:noteBuiltinVariableUsed(builtinVar);
    return self:tryRemap(builtinVar, self.builtinVariableRemap)
  end

  -- if decl already has name
  local ret = self.declToName[node]
  if ret ~= nil then
    return ret
  end

  if not node.name and not name then
    print(debug.traceback())
    Utils.printtable(node)
    error(string.format("Error-internal: no name for declaration %s\n", node))
  end

  -- generate a unique name for the decl, use this name
  -- in the mapping from decl to name and vice-versa
  -- then return it
  local curName = (name == nil and node.name or name)
  local uniqueName = self:createUniqueName(curName)

  self.declToName[node] = uniqueName
  self.nameToDecl[uniqueName] = node

  return uniqueName
end

function EmitContext:noteBuiltinVariableUsed(name)
end

-- type-related stuff

function EmitContext:addBuiltinStructs() 
  for k,_ in pairs(Builtin.structs) do
    local members = {}
    for _,v in ipairs(k.entries) do
      members[v.field] = v.field
    end

    local newStruct = { members = members }
    self.gStructs[k] = newStruct
  end
end

local function isTuple(t)
  return t.kind == "struct" and t.convertible == "tuple"
end

function EmitContext:emitSamplerFunctionArg(texture, shouldIncludeType)
  -- Nothing needed for GLSL. Will override for HLSL.
  return ""
end

local function isTextureSamplerType(t)
  return t == sampler2D or t == samplerCube
end


function EmitContext:emitDeclImpl(ty, name)
  if ty:isarray() then
    return self:getTypeName(ty.type) .. " " .. name .. "[" .. ty.N .. "]"
  end

  return self:getTypeName(ty) .. " " .. name
end

-- TODO struct methods
function EmitContext:emitStructDecl(t)
  local members = {}
  local declString = "struct " ..
    (isTuple(t) and self:getName(t, pf .. "tuple") or self:getName(t)) .. " {\n"

  for _,v in ipairs(t.entries) do
    local memberType = (isTuple(t) and v[2] or v.type)
    local curName = (isTuple(t) and v[1] or v.field)
    -- TODO unique member names
    local newName = curName

    members[curName] = newName
    declString = declString .. "  "
      .. self:emitDeclImpl(memberType, newName) .. ";\n"
  end

  declString = declString .. "};\n";
  local newStruct = { members = members }
  self.gStructs[t] = newStruct
  self.gStructDecls:insert(declString)
end

-- TODO function pointer types
function EmitContext:getTypeName(t)
  if Builtin.isBuiltinType(t) then
    return self:tryRemap(Builtin.types[t], self.builtinTypeMap);
  end

  if isTuple(t) and #t.entries == 0 then
    return "void"
  end

  if t.kind == "struct" and self.gStructs[t] == nil then
    self:emitStructDecl(t)
  end

  return self:getName(t)
end

--

function EmitContext:makeNewEnv(oldEnv, tableToSearch)
  local newEnv = {}
  setmetatable(newEnv, { __index = function(table, key)
    local status, ret = pcall(function() return oldEnv[key] end)
    if status and ret then
      return ret
    end

    return tableToSearch[key]
  end })

  return newEnv
end


-- TODO expand this function as necessary
function tryGetCompileTimeConstantValue(expr)
  if expr:is "literal" then
    if type(expr.value) == "userdata" then
      return tonumber(ffi.cast("uint64_t *",expr.value)[0])
    else
      return expr.value
    end
  elseif expr:is "operator" then
    local operands = expr.operands
    if expr.operator == "+" then
      return tryGetCompileTimeConstantValue(operands[1]) + tryGetCompileTimeConstantValue(operands[2])
    elseif expr.operator == "-" then
      if #operands == 1 then
        return -tryGetCompileTimeConstantValue(operands[1])
      end
      return tryGetCompileTimeConstantValue(operands[1]) - tryGetCompileTimeConstantValue(operands[2])
    end
  end

  return nil
end

function EmitContext:emitDecl(decl)
  return self:emitDeclImpl(decl.type, self:getName(decl))
end

function EmitContext:emitInsOutsUnifs(qualifier, builderData)
  local ret = ""
  for _,v in ipairs(builderData) do
    local origName = v.name
    local decl = v.decl

    local loc = v.location

    local newName = self:getName(decl, origName)

    if loc then
      ret = ret .. "layout (location = " .. loc .. ")\n"
    end
    ret = ret .. qualifier .. " " .. self:emitDecl(decl) .. ";\n\n"
  end

  return ret
end

function EmitContext:emitUniforms(builder)
  return self:emitInsOutsUnifs("uniform", builder.uniformsList)
end

function EmitContext:emitInputs(builder)
  return self:emitInsOutsUnifs("in", builder.inputsList)
end

function EmitContext:emitVaryings(builder)
  local vertexVaryings    = self:emitInsOutsUnifs("out", builder.varyingsList)
  local fragmentVaryings  = self:emitInsOutsUnifs("in", builder.varyingsList)
  return { vertexVaryings = vertexVaryings, fragmentVaryings = fragmentVaryings }
end

function EmitContext:emitOutputs(builder)
  return self:emitInsOutsUnifs("out", builder.outputsList)
end

function EmitContext:emitTextureSamplers(builder)
  local ret = ""
  for _,v in ipairs(builder.textureSamplersList) do
    local origName = v.name
    local decl = v.decl

    local binding = v.binding

    local newName = self:getName(decl, origName)

    if binding then
      ret = ret .. "layout (binding = " .. binding .. ")\n"
    end
    ret = ret .. "uniform" .. " " .. self:emitDecl(decl) .. ";\n\n"
  end

  return ret
end

function EmitContext:emitTextureImages(builder)
  local ret = ""
  for _,v in ipairs(builder.textureImagesList) do
    local origName = v.name
    local decl = v.decl

    local binding = v.binding

    local newName = self:getName(decl, origName)

    ret = ret .. "layout (" .. v.shaderTextureFormat

    if binding then
      ret = ret .. ", binding = " .. binding
    end

    ret = ret .. ")\n"

    if v.shaderMemoryMode then
      ret = ret .. v.shaderMemoryMode .. " "
    end

    ret = ret .. "uniform" .. " " .. self:emitDecl(decl) .. ";\n\n"
  end

  return ret
end

function EmitContext:emitBuffers(builder)
  local ret = ""
  for i, v in ipairs(builder.buffersList) do
    local origName = v.name
    local decl = v.decl

    local binding = v.binding

    local newName = self:getName(decl, origName)

    ret = ret .. "layout (std430"

    if binding then
      ret = ret .. ", binding = " .. binding
    end

    ret = ret .. ")\n"

    ret = ret .. "buffer Buffer" .. i .. " {\n"
    ret = ret .. "  " .. self:emitDecl(decl) .. ";\n"
    ret = ret .. "};\n\n"
  end

  return ret
end

--

function EmitContext:emitUniformBlocks(builder)
  local ret = ""
  for _, block in ipairs(builder.uniformBlocksList) do
    local origBlockName = block.name
    local newBlockName = self:createUniqueName(origBlockName)

    ret = ret .. "layout (std140"

    local binding = block.binding

    if binding then
      ret = ret .. ", binding = " .. binding
    end
    ret = ret .. ")\n"
    ret = ret .. "uniform " .. newBlockName .. "\n"
    ret = ret .. "{\n"

    for _, uniform in ipairs(block.uniformsList) do
      local newName = self:getName(uniform.decl, uniform.name)
      ret = ret .. "  " .. self:emitDecl(uniform.decl) .. ";\n"
    end

    ret = ret .. "};\n\n"
  end

  return ret
end

function EmitContext:emitTemps(tempsList)
  local ret = ""
  for _, v in ipairs(tempsList) do
    self:getName(v.decl, v.name)
    if v.isShared then
      ret = ret .. "shared "
    end
    ret = ret .. self:emitDecl(v.decl) .. ";\n"
  end
  return ret
end

function EmitContext:emitLiteral(expr)
  local ty = expr.type

  if ty:isintegral() then
    return expr.stringvalue
  elseif ty:isfloat() then
    if expr.type.name == "double" then
      return "double(" .. expr.value .. ")"
    else
      return string.format("%.9ff", expr.value);
    end
  elseif ty:islogical() then
    return tostring(expr.value)
  end

  Utils.unimpl("for type '" .. ty.type .. "'\n")
end

function EmitContext:emitExpr(expr)
  local function unimpl()
    Utils.printtable(expr)
    Utils.unimpl("for kind '" .. expr.kind .. "'")
  end


  if expr:is "literal" then
    return self:emitLiteral(expr)
  elseif expr:is "index" then
    -- TODO: handle case where `expr.value` is not a cast...
    return "(" .. self:emitExpr(expr.value.expression) .. ")[" .. self:emitExpr(expr.index) .. "]"
    -- TODO expr.value is actually a cast, so see if that is important or
    --      if it's ok to just bypass straight to the expression in all cases
--    return self:getName(expr.value.expression.definition)
--      .. "[" .. self:emitLiteral(expr.index) .. "]"
  elseif expr:is "constant" then
    -- TODO tonumber() converts to a double, so there might be issues with
    --      values that won't fit into a double
    if Builtin.isBuiltinType(expr.type) then
      return tonumber(expr.value)
    end
  elseif expr:is "var" then
    return self:getName(expr.symbol)
  elseif expr:is "globalvalueref" then
    return self:getName(expr.value)
  elseif expr:is "allocvar" then
    self:getName(expr.symbol, expr.name)
    return self:emitDecl(expr.symbol)
  elseif expr:is "apply" then
    return self:emitFunctionCall(expr)
  elseif expr:is "select" then
    local value = expr.value
    return self:emitExpr(value) .. "." ..
      self.gStructs[value.type].members[expr.fieldname]
  elseif expr:is "operator" then
    -- TODO add more operators
    local operands = expr.operands
    if expr.operator == "+" then
      return "(" .. self:emitExpr(operands[1]) .. " + " .. self:emitExpr(operands[2]) .. ")"
    elseif expr.operator == "-" then
      if #operands == 1 then
        return "("  .. "-" .. self:emitExpr(operands[1]) .. ")"
      end
      return "(" .. self:emitExpr(operands[1]) .. " - " .. self:emitExpr(operands[2]) .. ")"
    elseif expr.operator == "*" then
      return "(" .. self:emitExpr(operands[1]) .. " * " .. self:emitExpr(operands[2]) .. ")"
    elseif expr.operator == "/" then
      return "(" .. self:emitExpr(operands[1]) .. " / " .. self:emitExpr(operands[2]) .. ")"
    elseif expr.operator == "%" then
      return "(" .. self:emitExpr(operands[1]) .. " % " .. self:emitExpr(operands[2]) .. ")"
    elseif expr.operator == "<" then
      return "(" .. self:emitExpr(operands[1]) .. " < " .. self:emitExpr(operands[2]) .. ")"
    elseif expr.operator == "<=" then
      return "(" .. self:emitExpr(operands[1]) .. " <= " .. self:emitExpr(operands[2]) .. ")"
    elseif expr.operator == ">" then
      return "(" .. self:emitExpr(operands[1]) .. " > " .. self:emitExpr(operands[2]) .. ")"
    elseif expr.operator == ">=" then
      return "(" .. self:emitExpr(operands[1]) .. " >= " .. self:emitExpr(operands[2]) .. ")"
    elseif expr.operator == "and" then
      if operands[1].type.name == "bool" and operands[2].type.name == "bool" then
        return "(" .. self:emitExpr(operands[1]) .. " && " .. self:emitExpr(operands[2]) .. ")"
      else
        return "(" .. self:emitExpr(operands[1]) .. " & " .. self:emitExpr(operands[2]) .. ")"
      end
    elseif expr.operator == "or" then
      if operands[1].type.name == "bool" and operands[2].type.name == "bool" then
        return "(" .. self:emitExpr(operands[1]) .. " || " .. self:emitExpr(operands[2]) .. ")"
      else
        return "(" .. self:emitExpr(operands[1]) .. " | " .. self:emitExpr(operands[2]) .. ")"
      end
    elseif expr.operator == "==" then
      return "(" .. self:emitExpr(operands[1]) .. " == " .. self:emitExpr(operands[2]) .. ")"
    elseif expr.operator == "~=" then
      return "(" .. self:emitExpr(operands[1]) .. " != " .. self:emitExpr(operands[2]) .. ")"
    elseif expr.operator == "not" then
      if operands[1].type.name == "bool" then
        return "(!" .. self:emitExpr(operands[1]) .. ")"
      else
        return "(~" .. self:emitExpr(operands[1]) .. ")"
      end
    elseif expr.operator == "select" then
      return "((" .. self:emitExpr(operands[1]) .. ") ? (" .. self:emitExpr(operands[2]) .. ") : (" .. self:emitExpr(operands[3]) .. "))"
    elseif expr.operator == "@" then
      -- Special casing to handle vector and matrix indexing
      if #operands == 1 then
        local op = operands[1]
        if op:is "apply" and Builtin.isIndexOperator(op.value.value) then
          local args = op.arguments
          -- args[1] is a pointer in Terra code, but we want the actual vector/matrix
          -- here, not a pointer.  We can get this by using the first operand of args[1]
          return "(" .. self:emitExpr(args[1].operands[1]) .. "[" .. self:emitExpr(args[2]) .. "])"
        end
      end

      print(expr.filename .. ":" .. expr.linenumber ..
        " Error: Dereference operator '@' is unsupported in shader code")
      error("Callstack for above error")
    end
  elseif expr:is "structcast" then
    -- struct initialization
    if isTuple(expr.expression.type) and expr.expression:is "constructor" then
      local ret = self:getTypeName(expr.type) .. "("

      for i,v in ipairs(expr.expression.expressions) do
        if i > 1 then
          ret = ret .. ", "
        end
        ret = ret .. self:emitExpr(v)
      end
      return ret .. ")"
    end
    umimpl()
  elseif expr:is "cast" then
    -- TODO implement other types of casts

    -- cast to a builtin type
    if Builtin.isBuiltinType(expr.to) then
      return Builtin.types[expr.to] .. "(" .. self:emitExpr(expr.expression) .. ")"
    else
      unimpl()
    end
  elseif expr:is "letin" then
    if #expr.expressions ~= 1 then
      unimpl()
    end

    for _,v in ipairs(expr.statements) do
      self.gCurFnDefn:insert("\n// unindented code - start")
      self:emitStmt("", v)
      self.gCurFnDefn:insert("// unindented code - end\n")
    end

    return self:emitExpr(expr.expressions[1])
  end

  unimpl()
end

function EmitContext:emitStructInit(indent, structType, structAllocVar, initList)
  if not isTuple(structType) then
    Utils.unimpl("for non-tuple types")
  end


  -- I think Terra requires tuple initialization lists to initialize all elements
  -- Structs are different I think
  assert(#structType.entries == #initList,
    "more initialization items than elements in the struct")

  -- build an assign state with multiple items on each side
  local lhs = {}
  local rhs = initList

  for i,v in ipairs(structType.entries) do
    local selectInst = {}
    selectInst.kind = "select"
    selectInst.value = { kind = "var",
                         name = pfFnOut,
                         type = self.gCurFn.type.returntype,
                         definition = structAllocVar
                       }
    selectInst.field = v[1]
    selectInst.type = v[2]
    selectInst.lvalue = true
    lhs[i] = selectInst
  end

  local assign = { kind = "assignment", lhs = lhs, rhs = rhs}

  self:emitStmt(indent, assign)
end

function EmitContext:emitStmt(indent, stmt)
  local function unimpl()
    Utils.printtable(stmt)
    Utils.unimpl("for kind '" .. stmt.kind .. "'")
  end

  if stmt:is "allocvar" then
    self:getName(stmt.symbol, stmt.name)
    self.gCurFnDefn:insert(indent .. self:emitDecl(stmt.symbol) .. ";")
    return
  elseif stmt:is "apply" then
    self.gCurFnDefn:insert(indent .. self:emitFunctionCall(stmt) .. ";")
    return
  elseif stmt:is "break" or stmt:is "breakstat" then
    self.gCurFnDefn:insert(indent .. "break;")
    return
  elseif stmt:is "ifstat" then
    for i,branch in ipairs(stmt.branches) do
      local ifStmt = ""
      if i ~= 1 then
        ifStmt = ifStmt .. "else "
      end

      ifStmt = ifStmt .. "if(" .. self:emitExpr(branch.condition) .. ")"
      self.gCurFnDefn:insert(indent .. ifStmt)
      self:emitBlock(indent, branch.body)
    end

    if stmt.orelse ~= nil then
      self.gCurFnDefn:insert(indent .. "else")
      self:emitBlock(indent, stmt.orelse)
    end

    return
  elseif stmt:is "fornum" then
    local variable
    if stmt.variable:is "allocvar" then
      variable = self:getName(stmt.variable.symbol, stmt.variable.name)
    elseif stmt.variable:is "var" then
      variable = self:getName(stmt.variable.symbol)
    else
      error("Error-internal: for loop variable must be of kind allocvar or var")
    end

    local step = "1"
    local cmp  = " < "
    if stmt.step ~= nil then
      step = self:emitExpr(stmt.step)

      local val = tryGetCompileTimeConstantValue(stmt.step)

      if val == nil then
        error("Error-internal: Cannot determine whether for loop step is positive or negative")
      else
        if val == 0 then
          error("Error: For loop must have non-zero step value")
        elseif val < 0 then
          cmp = " > "
        else
          cmp = " < "
        end
      end
    end

    local forStmt = "for(" .. self:emitExpr(stmt.variable) ..
      " = " .. self:emitExpr(stmt.initial) .. "; "
    forStmt = forStmt .. variable .. cmp .. self:emitExpr(stmt.limit) .. "; "
    forStmt = forStmt .. variable .. " += " .. step .. ")"

    self.gCurFnDefn:insert(indent .. forStmt)
    self:emitBlock(indent, stmt.body)
    return
  elseif stmt:is "assignment" then
    if #stmt.lhs ~= #stmt.rhs then
      unimpl()
    end

    -- rhs must be fully evaluated before lhs
    -- so, if rhs uses vars assign in lhs, store rhs to temps
    local needTemps = false
    if #stmt.lhs > 1 then
      -- TODO further refine this test
      --      only need temps if something on lhs is referenced in rhs,
      --      but need to walk tree on rhs bc vars could be args to functions,
      --      or in constructors, etc.
      for i,v in pairs(stmt.lhs) do
        local lhs = stmt.lhs[i]
        if not lhs:is "allocvar" then
          needTemps = true
          break
        end
      end
    end

    for i,v in pairs(stmt.lhs) do
      if needTemps then
        -- TODO maybe move temp
        --unimpl()
      end

      local lhs = stmt.lhs[i]
      local rhs = stmt.rhs[i]

      local assign = indent .. self:emitExpr(lhs) .. " = " .. self:emitExpr(rhs) .. ";"
      self.gCurFnDefn:insert(assign)
    end
    return
  elseif stmt:is "returnstat" then
    self.gCurFnDefn:insert(indent .. "return " .. self:emitExpr(stmt.expression) .. ";")
    return
  elseif stmt:is "block" then
    for _,v in ipairs(stmt.statements) do
      self:emitStmt(indent, v)
    end
    return
  end

  unimpl()
end

function EmitContext:emitBlock(indent, body)
  assert(body:is "block",
    "Error-internal: Shader body must be a block.\n")

  local stmts = body.statements
  self.gCurFnDefn:insert(indent .. "{")
  for _,s in ipairs(stmts) do
    self:emitStmt(indent .. "  ", s)
  end
  self.gCurFnDefn:insert(indent .. "}")
end

-- If the function is not already declared, write the function declaration
-- to the shaderFunctionDecls string, insert this function into the list
-- of functions to define, and add the function to the gFunctions table
-- which is a mapping from function to the string representing the
-- function signature
--
-- Params
--  fn: a Terra function definition
function EmitContext:emitFunctionDecl(fn)
  local function unimpl(s)
    Utils.unimpl("for '" .. s .. "'")
  end

  -- if function is builtin
  if Builtin.isBuiltinFunction(fn) then
    return
  end

  if Builtin.isBuiltinOperator(fn) then
    return
  end

  local name = self:getName(fn)

  if fn.definition then
    fn = fn.definition
  end

  -- if function already declared
  if self.gFunctions[fn] ~= nil then
    return
  end

  local params = fn.parameters
  local returntype = fn.type.returntype

  local fnIns  = ""

  local textureSamplerArgs = terralib.newlist()

  if Utils.count(params) > 0 then
    for i,v in ipairs(params) do
      if i > 1 then
        fnIns = fnIns .. ", "
      end

      if isTextureSamplerType(v.type) then
        textureSamplerArgs:insert(v)
      end

      fnIns = fnIns .. self:emitDeclImpl(v.type, self:getName(v.symbol, v.name))
    end
  end

  for _, v in ipairs(textureSamplerArgs) do
    fnIns = fnIns .. self:emitSamplerFunctionArg(v, true)
  end

  local fnDecl = self:getTypeName(returntype) .. " "
    .. name .. "(" .. fnIns .. ")"

  self.gShaderFunctionDecls = self.gShaderFunctionDecls .. fnDecl .. ";\n"

  self.gFunctions[fn] = fnDecl
  table.insert(self.gFunctionsToDefine, fn)
end

function EmitContext:emitFunctionDefn(fn)
  self:emitFunctionDecl(fn)

  self.gCurFn = fn
  self.gCurFnDefn = {}
  setmetatable(self.gCurFnDefn, {__index = table})

  local fnDecl = self.gFunctions[fn]
  self.gCurFnDefn:insert(fnDecl)
  self:emitBlock("", fn.body)

  local ret = self.gCurFnDefn:concat("\n")

  return ret .. "\n"
end

function EmitContext:emitInnerProduct(left, right)
  -- for now, default to the GLSL version, which flips the arguments (since we are row-major)
  return "(" .. right .. " * " .. left .. ")"
end

-- A function call is represented as an "apply"
-- The called function is apply->value->value
function EmitContext:emitFunctionCall(apply)
  local function unimpl(s)
    Utils.unimpl("for " .. s)
  end

  local function unimplAt(what, where)
    Utils.unimpl(what .. " at " .. where.filename .. ":" .. where.linenumber);
  end

  assert(apply:is "apply", 
    "Error-internal: emitFunctionCall called on something other than 'apply'")

  if not apply.value:is "globalvalueref" or not apply.value.value.kind == "terrafunction" then
    unimplAt("function pointers with non-literal values", apply)
  end

  local fn = apply.value.value
  self:emitFunctionDecl(fn)

  if Builtin.isBuiltinOperator(fn) then

    if #apply.arguments == 1 then
      -- Note: This implementation assume all unary operators are prefix, which
      --       might not always be true in the future.
      local arg1 = self:emitExpr(apply.arguments[1])
      return "(" .. Builtin.operators[fn] .. arg1 .. ")"
    elseif #apply.arguments == 2 then
      local arg1 = self:emitExpr(apply.arguments[1])
      local arg2 = self:emitExpr(apply.arguments[2])

      -- special case for matrix multiplication
      if Builtin.isMatrixMult(fn) then
        return self:emitInnerProduct(arg1, arg2);
      end

      return "(" .. arg1 .. " " .. Builtin.operators[fn] .. " " .. arg2 .. ")"
    else
      assert(false,
        "Error-internal: emitFunctionCall - " ..
        "builtin operators must be called with one or two arguments")
    end
  end

  local textureSamplerArgs = terralib.newlist()

  -- TODO maybe move temp
  -- TODO tuples
  --      might need to box up tuples into appropriate struct type
  local argListString = ""
  local argStrings = {}
  local args = apply.arguments
  for i,v in ipairs(args) do
    if isTuple(v) then
      unimpl("tuple function parameters")
    end

    if i > 1 then
      argListString = argListString .. ", "
    end

    if isTextureSamplerType(v.type) then
      textureSamplerArgs:insert(v)
    end

    local argString = self:emitExpr(v);

    argStrings[i] = argString;
    argListString = argListString .. argString
  end

  if Builtin.isBuiltinFunction(fn) then
    local key = self:tryRemap(Builtin.functions[fn], self.builtinFunctionMap)
    if key and type(key) == "function" then
      -- callback to handle this...
      return key(self, argStrings, args)
    end
  end

  for _, v in ipairs(textureSamplerArgs) do
    argListString = argListString .. self:emitSamplerFunctionArg(v, false)
  end

  local ret = self:getName(fn) .. "(" .. argListString .. ")"

  return ret
end


local VertexStage = {}

function VertexStage:getCodeList(ctxt, builder)
  return builder.vertexCode
end

function VertexStage:getInputsList(ctxt, builder)
  return builder.inputsList
end

function VertexStage:getOutputsList(ctxt, builder)
  return builder.varyingsList
end

function VertexStage:getTempList(ctxt, builder)
  return builder.verttempsList
end

function VertexStage:emitLayoutInfo(ctxt, builder)
  return ""
end

local FragmentStage = {}

function FragmentStage:getCodeList(ctxt, builder)
  return builder.fragmentCode
end

function FragmentStage:getInputsList(ctxt, builder)
  return builder.varyingsList
end

function FragmentStage:getOutputsList(ctxt, builder)
  return builder.outputsList
end

function FragmentStage:getTempList(ctxt, builder)
  return builder.fragtempsList
end

function FragmentStage:emitLayoutInfo(ctxt, builder)
  return ""
end

local ComputeStage = {}

function ComputeStage:getCodeList(ctxt, builder)
  return builder.computeCode
end

function ComputeStage:getInputsList(ctxt, builder)
  return terralib.newlist()
end

function ComputeStage:getOutputsList(ctxt, builder)
  return terralib.newlist()
end

function ComputeStage:getTempList(ctxt, builder)
  return builder.computetempsList
end

function ComputeStage:emitLayoutInfo(ctxt, builder)
  if builder.numthreads == nil then
    return ""
  end
  return "layout(local_size_x = " .. builder.numthreads.x ..
              ", local_size_y = " .. builder.numthreads.y ..
              ", local_size_z = " .. builder.numthreads.z .. ") in;\n"
end


local TerraToShader = {}
TerraToShader.EmitContext = EmitContext
TerraToShader.VertexStage = VertexStage
TerraToShader.FragmentStage = FragmentStage
TerraToShader.ComputeStage = ComputeStage
TerraToShader.isTuple = isTuple
TerraToShader.newClass = newClass
TerraToShader.Builtin = Builtin
return TerraToShader
