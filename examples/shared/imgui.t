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

-- imgui.t

-- Parts of this file are taken from:
--     external/cimgui/imgui/imgui.h
-- which is provided under an MIT license:
--     external/cimgui/imgui/LICENSE
--

local C = terralib.includecstring([[
  //#include <math.h>
  //#include <stdio.h>
  //#include <stdlib.h>
  #include <string.h>
]])

local ImGui = terralib.includecstring([[
  #define OPAQUE_TYPE(T) typedef struct T T
  OPAQUE_TYPE(ImDrawCmd);
  OPAQUE_TYPE(ImDrawData);
  OPAQUE_TYPE(ImDrawList);
  OPAQUE_TYPE(ImDrawVert);
  OPAQUE_TYPE(ImFont);
  OPAQUE_TYPE(ImFontAtlas);
  OPAQUE_TYPE(ImFontConfig);
  OPAQUE_TYPE(ImGuiIO);
  OPAQUE_TYPE(ImGuiStorage);
  OPAQUE_TYPE(ImGuiStyle);
  OPAQUE_TYPE(ImGuiTextEditCallbackData);

  typedef unsigned int ImU32;
  typedef unsigned short ImWchar;
  typedef void* ImTextureID;
  typedef ImU32 ImGuiID;
  typedef int ImGuiCol;
  typedef int ImGuiStyleVar;
  typedef int ImGuiKey;
  typedef int ImGuiAlign;
  typedef int ImGuiColorEditMode;
  typedef int ImGuiMouseCursor;
  typedef int ImGuiWindowFlags;
  typedef int ImGuiSetCond;
  typedef int ImGuiInputTextFlags;
  typedef int ImGuiSelectableFlags;
  typedef int (*ImGuiTextEditCallback)(ImGuiTextEditCallbackData *data);
  typedef unsigned short ImDrawIdx;

  typedef struct ImVec2
  {
    float x, y;
  } ImVec2;

  typedef struct ImVec4
  {
    float x, y, z, w;
  } ImVec4;

  #define CIMGUI_DEFAULT(E) /* empty */

  #include "]] .. externalPath .. "cimgui/cimgui/cimgui.h" .. [["

  void igDummy();

  #define IMGUI_API CIMGUI_API
  OPAQUE_TYPE(SDL_Window);


struct ImDrawData
{
    bool            Valid;                  // Only valid after Render() is called and before the next NewFrame() is called.
    ImDrawList**    CmdLists;
    int             CmdListsCount;
    int             TotalVtxCount;          // For convenience, sum of all cmd_lists vtx_buffer.Size
    int             TotalIdxCount;          // For convenience, sum of all cmd_lists idx_buffer.Size
};

typedef unsigned short ImDrawIdx;

struct ImDrawVert
{
    ImVec2  pos;
    ImVec2  uv;
    ImU32   col;
};

typedef void (*ImDrawCallback)(const ImDrawList* parent_list, const ImDrawCmd* cmd);

struct ImDrawCmd
{
    unsigned int    ElemCount;              // Number of indices (multiple of 3) to be rendered as triangles. Vertices are stored in the callee ImDrawList's vtx_buffer[] array, indices in idx_buffer[].
    ImVec4          ClipRect;               // Clipping rectangle (x1, y1, x2, y2)
    ImTextureID     TextureId;              // User-provided texture ID. Set by user in ImfontAtlas::SetTexID() for fonts or passed to Image*() functions. Ignore if never using images or multiple fonts atlas.
    ImDrawCallback  UserCallback;           // If != NULL, call the function instead of rendering the vertices. clip_rect and texture_id will be set normally.
    void*           UserCallbackData;       // The draw callback code can access this.
};

  #include "]] .. externalPath .. "cimgui/imgui/examples/sdl_opengl_example/imgui_impl_sdl.h" .. [["
]], terralib.newlist{"-std=c11"})

local ImGuiTmp = {}
for k,v in pairs(ImGui) do
  local m = string.match(k, "^ig(.*)")
  if m then
    ImGuiTmp[m] = v;
  end
end

for k,v in pairs(ImGuiTmp) do
  ImGui[k] = v
end

local tmp = ImGui.Button
ImGui.Button = terralib.overloadedfunction("igButton")
ImGui.Button:adddefinition(tmp)
ImGui.Button:adddefinition(terra( label : rawstring ) : bool
  return ImGui.Button(label, ImGui.ImVec2{0,0});
end)



ImGuiEnums = terralib.includecstring([[
// enum constants need to be manually copied from imgui.h

// User fill ImGuiIO.KeyMap[] array with indices into the ImGuiIO.KeysDown[512] array
enum ImGuiKey_
{
    ImGuiKey_Tab,       // for tabbing through fields
    ImGuiKey_LeftArrow, // for text edit
    ImGuiKey_RightArrow,// for text edit
    ImGuiKey_UpArrow,   // for text edit
    ImGuiKey_DownArrow, // for text edit
    ImGuiKey_PageUp,
    ImGuiKey_PageDown,
    ImGuiKey_Home,      // for text edit
    ImGuiKey_End,       // for text edit
    ImGuiKey_Delete,    // for text edit
    ImGuiKey_Backspace, // for text edit
    ImGuiKey_Enter,     // for text edit
    ImGuiKey_Escape,    // for text edit
    ImGuiKey_A,         // for text edit CTRL+A: select all
    ImGuiKey_C,         // for text edit CTRL+C: copy
    ImGuiKey_V,         // for text edit CTRL+V: paste
    ImGuiKey_X,         // for text edit CTRL+X: cut
    ImGuiKey_Y,         // for text edit CTRL+Y: redo
    ImGuiKey_Z,         // for text edit CTRL+Z: undo
    ImGuiKey_COUNT
};

// Condition flags for ImGui::SetWindow***(), SetNextWindow***(), SetNextTreeNode***() functions
// All those functions treat 0 as a shortcut to ImGuiSetCond_Always
enum ImGuiSetCond_
{
    ImGuiSetCond_Always        = 1 << 0, // Set the variable
    ImGuiSetCond_Once          = 1 << 1, // Only set the variable on the first call per runtime session
    ImGuiSetCond_FirstUseEver  = 1 << 2, // Only set the variable if the window doesn't exist in the .ini file
    ImGuiSetCond_Appearing     = 1 << 3  // Only set the variable if the window is appearing after being inactive (or the first time)
};

enum ImGuiInputTextFlags_
{
    // Default: 0
    ImGuiInputTextFlags_CharsDecimal        = 1 << 0,   // Allow 0123456789.+-*/
    ImGuiInputTextFlags_CharsHexadecimal    = 1 << 1,   // Allow 0123456789ABCDEFabcdef
    ImGuiInputTextFlags_CharsUppercase      = 1 << 2,   // Turn a..z into A..Z
    ImGuiInputTextFlags_CharsNoBlank        = 1 << 3,   // Filter out spaces, tabs
    ImGuiInputTextFlags_AutoSelectAll       = 1 << 4,   // Select entire text when first taking mouse focus
    ImGuiInputTextFlags_EnterReturnsTrue    = 1 << 5,   // Return 'true' when Enter is pressed (as opposed to when the value was modified)
    ImGuiInputTextFlags_CallbackCompletion  = 1 << 6,   // Call user function on pressing TAB (for completion handling)
    ImGuiInputTextFlags_CallbackHistory     = 1 << 7,   // Call user function on pressing Up/Down arrows (for history handling)
    ImGuiInputTextFlags_CallbackAlways      = 1 << 8,   // Call user function every time
    ImGuiInputTextFlags_CallbackCharFilter  = 1 << 9,   // Call user function to filter character. Modify data->EventChar to replace/filter input, or return 1 to discard character.
    ImGuiInputTextFlags_AllowTabInput       = 1 << 10,  // Pressing TAB input a '\t' character into the text field
    ImGuiInputTextFlags_CtrlEnterForNewLine = 1 << 11,  // In multi-line mode, allow exiting edition by pressing Enter. Ctrl+Enter to add new line (by default adds new lines with Enter).
    ImGuiInputTextFlags_NoHorizontalScroll  = 1 << 12,  // Disable following the cursor horizontally
    ImGuiInputTextFlags_AlwaysInsertMode    = 1 << 13,  // Insert mode
    ImGuiInputTextFlags_ReadOnly            = 1 << 14,  // Read-only mode
    // [Internal]
    ImGuiInputTextFlags_Multiline           = 1 << 20   // For internal use by InputTextMultiline()
};

// Flags for ImGui::Begin()
enum ImGuiWindowFlags_
{
    // Default: 0
    ImGuiWindowFlags_NoTitleBar             = 1 << 0,   // Disable title-bar
    ImGuiWindowFlags_NoResize               = 1 << 1,   // Disable user resizing with the lower-right grip
    ImGuiWindowFlags_NoMove                 = 1 << 2,   // Disable user moving the window
    ImGuiWindowFlags_NoScrollbar            = 1 << 3,   // Disable scrollbar (window can still scroll with mouse or programatically)
    ImGuiWindowFlags_NoScrollWithMouse      = 1 << 4,   // Disable user scrolling with mouse wheel
    ImGuiWindowFlags_NoCollapse             = 1 << 5,   // Disable user collapsing window by double-clicking on it
    ImGuiWindowFlags_AlwaysAutoResize       = 1 << 6,   // Resize every window to its content every frame
    ImGuiWindowFlags_ShowBorders            = 1 << 7,   // Show borders around windows and items
    ImGuiWindowFlags_NoSavedSettings        = 1 << 8,   // Never load/save settings in .ini file
    ImGuiWindowFlags_NoInputs               = 1 << 9,   // Disable catching mouse or keyboard inputs
    ImGuiWindowFlags_MenuBar                = 1 << 10,  // Has a menu-bar
    ImGuiWindowFlags_HorizontalScrollbar    = 1 << 11,  // Enable horizontal scrollbar (off by default). You need to use SetNextWindowContentSize(ImVec2(width,0.0f)); prior to calling Begin() to specify width. Read code in imgui_demo in the "Horizontal Scrolling" section.
    ImGuiWindowFlags_NoFocusOnAppearing     = 1 << 12,  // Disable taking focus when transitioning from hidden to visible state
    ImGuiWindowFlags_NoBringToFrontOnFocus  = 1 << 13,  // Disable bringing window to front when taking focus (e.g. clicking on it or programatically giving it focus)
    // [Internal]
    ImGuiWindowFlags_ChildWindow            = 1 << 20,  // Don't use! For internal use by BeginChild()
    ImGuiWindowFlags_ChildWindowAutoFitX    = 1 << 21,  // Don't use! For internal use by BeginChild()
    ImGuiWindowFlags_ChildWindowAutoFitY    = 1 << 22,  // Don't use! For internal use by BeginChild()
    ImGuiWindowFlags_ComboBox               = 1 << 23,  // Don't use! For internal use by ComboBox()
    ImGuiWindowFlags_Tooltip                = 1 << 24,  // Don't use! For internal use by BeginTooltip()
    ImGuiWindowFlags_Popup                  = 1 << 25,  // Don't use! For internal use by BeginPopup()
    ImGuiWindowFlags_Modal                  = 1 << 26,  // Don't use! For internal use by BeginPopupModal()
    ImGuiWindowFlags_ChildMenu              = 1 << 27   // Don't use! For internal use by BeginMenu()
};

]])

for k,v in pairs(ImGuiEnums) do
  ImGui[k] = v;
  local m = string.match(k, "~ImGui(.*)")
  if m then
    ImGui[m] = v;
  end
end

local SDL = require "sdl"


local Gfx = require "gfx"

local gfxDevice = global(&Gfx.Device);
local gfxContext = global(&Gfx.Context);

local g_FontTexture = global(Gfx.Texture);

local g_pVB = global(Gfx.Buffer);
local g_pIB = global(Gfx.Buffer);

local g_VertexBufferSize = global(int);
local g_IndexBufferSize = global(int);

local g_pVertexConstantBuffer = global(Gfx.Buffer);

local g_pInputLayout = global(Gfx.VertexLayout);

struct VERTEX_CONSTANT_BUFFER
{
  mvp : float[4][4];
}

import "pipelinelang"

local pipeline ImGuiPipeline
{
  uniform Uniforms
  {
    mvp : mat4
  }

  textureSampler t_col : sampler2D

  input pos : vec2
  input uv : vec2
  input col : vec4

  varying v_uv : vec2
  varying v_col : vec4

  out o_col : vec4

  vertex code
    Position = mvp * make_vec4(pos, 0, 1);
    v_uv = uv;
    v_col = col
  end

  fragment code
    o_col = v_col * texture(t_col, v_uv);
  end
}

local ImGuiPipelineMetaData = ImGuiPipeline.metadata;
local ImGuiPipelineVertGLSL = ImGuiPipeline.vertexSourceCode;
local ImGuiPipelineFragGLSL = ImGuiPipeline.fragmentSourceCode;

local g_program = global(Gfx.ShaderProgram);


local gDepthStencilState  = global(Gfx.DepthStencilState);
local gRasterizerState    = global(Gfx.RasterizerState);
local gSamplerState       = global(Gfx.SamplerState);
local gBlendState         = global(Gfx.BlendState);


-- This is the main rendering function that you have to implement and provide to ImGui (via setting up 'RenderDrawListsFn' in the ImGuiIO structure)
-- If text or lines are blurry when integrating ImGui in your engine:
-- - in your Render function, try translating your projection matrix by (0.5f,0.5f) or (0.375f,0.375f)
local terra ImGui_ImplSdl_RenderDrawLists( draw_data : &ImGui.ImDrawData )

  var width : float;
  var height : float;
  ImGui.GetDisplaySize(&width, &height);

  -- Create and grow vertex/index buffers if needed
  if ((g_pVB == Gfx.NilBuffer) or (g_VertexBufferSize < draw_data.TotalVtxCount)) then

    if g_pVB ~= Gfx.NilBuffer then
      gfxDevice:deleteBuffer(g_pVB);
      g_pVB = Gfx.NilBuffer;
    end

    g_VertexBufferSize = draw_data.TotalVtxCount + 5000;

    g_pVB = gfxDevice:beginBuildBuffer()
      :setSize(g_VertexBufferSize * [terralib.sizeof(ImGui.ImDrawVert)])
      :setUsage(Gfx.Usage.Dynamic)
      :setBindVertexBuffer()
      :setCPUAccess(Gfx.CPUAccess.Write)
      :endBuild();
  end

  if ((g_pIB == Gfx.NilBuffer) or (g_IndexBufferSize < draw_data.TotalIdxCount)) then

    if g_pIB ~= Gfx.NilBuffer then
      gfxDevice:deleteBuffer(g_pIB);
      g_pIB = Gfx.NilBuffer;
    end

    g_IndexBufferSize = draw_data.TotalIdxCount + 10000;

    g_pIB = gfxDevice:beginBuildBuffer()
      :setSize(g_IndexBufferSize * [terralib.sizeof(ImGui.ImDrawIdx)])
      :setUsage(Gfx.Usage.Dynamic)
      :setBindIndexBuffer()
      :setCPUAccess(Gfx.CPUAccess.Write)
      :endBuild();
  end


  var vtx_dst = [&ImGui.ImDrawVert]( gfxContext:mapBuffer(g_pVB, Gfx.MapMode.WriteDiscard,
    g_VertexBufferSize * [terralib.sizeof(ImGui.ImDrawVert)], Gfx.CPUAccess.Write) );
  var idx_dst = [&ImGui.ImDrawIdx]( gfxContext:mapBuffer(g_pIB, Gfx.MapMode.WriteDiscard,
      g_IndexBufferSize * [terralib.sizeof(ImGui.ImDrawIdx)], Gfx.CPUAccess.Write) );

  for n = 0,draw_data.CmdListsCount do
    var cmd_list = draw_data.CmdLists[n];

    var vtxBufferSize = ImGui.ImDrawList_GetVertexBufferSize(cmd_list);
    var idxBufferSize = ImGui.ImDrawList_GetIndexBufferSize(cmd_list);
    var vtx_src = ImGui.ImDrawList_GetVertexPtr(cmd_list, 0);
    var idx_src = ImGui.ImDrawList_GetIndexPtr(cmd_list, 0);

    C.memcpy(vtx_dst, vtx_src, vtxBufferSize * [terralib.sizeof(ImGui.ImDrawVert)]);
    C.memcpy(idx_dst, idx_src, idxBufferSize * [terralib.sizeof(ImGui.ImDrawIdx)]);
    vtx_dst = vtx_dst + vtxBufferSize;
    idx_dst = idx_dst + idxBufferSize;
  end

  gfxContext:unmapBuffer(g_pVB);
  gfxContext:unmapBuffer(g_pIB);

  -- Set up orthographic projection matrix into our constant buffer

  if g_pVertexConstantBuffer == Gfx.NilBuffer then
    g_pVertexConstantBuffer = gfxDevice:beginBuildBuffer()
      :setSize([terralib.sizeof(VERTEX_CONSTANT_BUFFER)])
      :setUsage(Gfx.Usage.Dynamic)
      :setBindUniformBuffer()
      :setCPUAccess(Gfx.CPUAccess.Write)
      :endBuild();
  end

  var uniform_dst = [&VERTEX_CONSTANT_BUFFER]( gfxContext:mapBuffer(g_pVertexConstantBuffer, Gfx.MapMode.WriteDiscard,
    [terralib.sizeof(VERTEX_CONSTANT_BUFFER)], Gfx.CPUAccess.Write) );
  var L = 0.0;
  var R = width;
  var B = height;
  var T = 0.0;
  var mvp = Gfx.FixupMatrix * make_mat4(
    2.0/(R-L),  0.0,        0.0,  (R+L)/(L-R),
    0.0,        2.0/(T-B),  0.0,  (T+B)/(B-T),
    0.0,        0.0,        0.5,  0.5,
    0.0,        0.0,        0.0,  1.0);

  C.memcpy(&uniform_dst.mvp, &mvp.rows[0], [terralib.sizeof(float[4][4])]);
  gfxContext:unmapBuffer(g_pVertexConstantBuffer);

  -- Set up viewport
  gfxContext:setViewport(0, 0, width, height)

  gfxContext:setVertexLayout(g_pInputLayout);

  var stride = [terralib.sizeof(ImGui.ImDrawVert)];
  var offset = 0;
  gfxContext:setVertexBuffer(0, g_pVB, stride, offset);
  gfxContext:setIndexBuffer(g_pIB, Gfx.IndexFormat.UInt16);


  gfxContext:setShaderProgram(g_program);

  gfxContext:setUniformBuffer(0, g_pVertexConstantBuffer);

  gfxContext:setDepthStencilState(gDepthStencilState);
  gfxContext:setRasterizerState(gRasterizerState);
  gfxContext:setBlendState(gBlendState);

  -- Render command lists
  var vtx_offset = 0;
  var idx_offset = 0;
  for n = 0,draw_data.CmdListsCount do
    var cmd_list = draw_data.CmdLists[n];

    for cmd_i = 0,ImGui.ImDrawList_GetCmdSize(cmd_list) do
      var pcmd = ImGui.ImDrawList_GetCmdPtr(cmd_list, cmd_i);
      if (pcmd.UserCallback ~= nil) then
        pcmd.UserCallback(cmd_list, pcmd);
      else
        gfxContext:setTextureSampler(0, 0, [Gfx.Texture]([uint64](pcmd.TextureId)), gSamplerState);

        gfxContext:setScissorRect( pcmd.ClipRect.x, pcmd.ClipRect.y, pcmd.ClipRect.z, pcmd.ClipRect.w, width, height );

        gfxContext:drawIndexed(Gfx.PrimitiveType.Triangles, pcmd.ElemCount, idx_offset, vtx_offset);
      end
      idx_offset = idx_offset + pcmd.ElemCount;
    end
    vtx_offset = vtx_offset + ImGui.ImDrawList_GetVertexBufferSize(cmd_list);
  end

end

local terra ImGui_ImplSdl_CreateDeviceObjects() : bool
  -- Build texture
  var pixels : &uint8;
  var width : int;
  var height : int;

  ImGui.IO_Fonts_GetTexDataAsRGBA32(&pixels, &width, &height);

  -- Create texture
  g_FontTexture = gfxDevice:beginBuildTexture2D(width, height)
    :setFormat(Gfx.TextureFormat.RGBA8)
    :setMipLevelCount(1)
    :setInitDataForLevel(0, pixels, 4*width)
    :endBuild();

  -- Store our identifier
  ImGui.IO_Fonts_SetTexID([&int8](g_FontTexture));

  -- Cleanup (don't clear the input data if you want to append new fonts later)
  ImGui.IO_Fonts_ClearInputData();
  ImGui.IO_Fonts_ClearTexData();

  --

  g_pInputLayout = gfxDevice:beginBuildVertexLayout()
    :addAttribute(Gfx.AttributeFormat.Vec2,   0, [terralib.offsetof(ImGui.ImDrawVert, "pos")])
    :addAttribute(Gfx.AttributeFormat.Vec2,   0, [terralib.offsetof(ImGui.ImDrawVert, "uv")])
    :addAttribute(Gfx.AttributeFormat.RGBA8,  0, [terralib.offsetof(ImGui.ImDrawVert, "col")])
    :endBuild();

  g_program = gfxDevice:createShaderProgram(ImGuiPipelineVertGLSL, ImGuiPipelineFragGLSL);


  gDepthStencilState = gfxDevice:beginBuildDepthStencilState()
    :setEnableDepthTest(false)
    :setEnableDepthWrite(false)
    :endBuild();

  gRasterizerState = gfxDevice:beginBuildRasterizerState()
    :setEnableScissor(true)
    :endBuild();

  gSamplerState = gfxDevice:beginBuildSamplerState()
    :setFilter(Gfx.Filter.MinMagLinear_MipPoint)
    :endBuild();

  gBlendState = gfxDevice:beginBuildBlendState()
    :setEnableBlend(true)
    :setBlendFunc(Gfx.BlendFactor.SrcAlpha, Gfx.BlendFactor.InvSrcAlpha)
    :endBuild();

  return true;
end

local terra ImGui_ImplSdl_InvalidateDeviceObjects()
  if g_FontTexture ~= Gfx.NilTexture then

    gfxDevice:deleteTexture(g_FontTexture);
--    GL.glDeleteTextures(1, &g_FontTexture);

    ImGui.IO_Fonts_SetTexID(nil);
    g_FontTexture = Gfx.NilTexture;
  end
end

local ImGui_ImplSdl_Init = ImGui.ImGui_ImplSdl_Init;
local terra ImGui_ImplSdl_InitExt( window : &SDL.SDL_Window, inGfx : &Gfx.Device )
  ImGui_ImplSdl_Init(window);

  gfxDevice = inGfx;
  gfxContext = gfxDevice:getImmediateContext();
end

local ImGui_ImplSdl_Shutdown = ImGui.ImGui_ImplSdl_Shutdown;
local terra ImGui_ImplSdl_ShutdownExt()
  ImGui_ImplSdl_InvalidateDeviceObjects();
  ImGui_ImplSdl_Shutdown();
end

local ImGui_ImplSdl_NewFrame = ImGui.ImGui_ImplSdl_NewFrame;

local terra ImGui_ImplSdl_NewFrameExt( window : &SDL.SDL_Window )
  if g_FontTexture == Gfx.NilTexture then
    ImGui_ImplSdl_CreateDeviceObjects();
  end

  ImGui_ImplSdl_NewFrame(window);
end

local ImGui_Render = ImGui.Render;

local terra ImGui_RenderExt()
  ImGui.Render();
  var drawData = ImGui.igGetDrawData();
  ImGui_ImplSdl_RenderDrawLists(drawData);
end

ImGui.ImGui_ImplSdl_Init = ImGui_ImplSdl_InitExt;
ImGui.ImGui_ImplSdl_Shutdown = ImGui_ImplSdl_ShutdownExt;
ImGui.ImGui_ImplSdl_NewFrame = ImGui_ImplSdl_NewFrameExt;
ImGui.Render = ImGui_RenderExt;

return ImGui;
