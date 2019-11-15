
#include "../imgui/imgui.h"
#include "cimgui.h"

CIMGUI_API ImGuiIO* igGetIO()
{
    return &ImGui::GetIO();
}

CIMGUI_API ImGuiStyle* igGetStyle()
{
    return &ImGui::GetStyle();
}

CIMGUI_API ImDrawData* igGetDrawData()
{
    return ImGui::GetDrawData();
}

CIMGUI_API void igNewFrame()
{
    ImGui::NewFrame();
}

CIMGUI_API void igRender()
{
    ImGui::Render();
}

CIMGUI_API void igShutdown()
{
    ImGui::Shutdown();
}

CIMGUI_API void igShowUserGuide()
{
    ImGui::ShowUserGuide();
}

CIMGUI_API void igShowStyleEditor(ImGuiStyle* ref)
{
    ImGui::ShowStyleEditor(ref);
}

CIMGUI_API void igShowTestWindow(bool* opened)
{
    ImGui::ShowTestWindow(opened);
}

IMGUI_API void igShowMetricsWindow(bool* opened)
{
    ImGui::ShowMetricsWindow(opened);
}

// Window

CIMGUI_API bool igBegin(CONST char* name, bool* p_opened, ImGuiWindowFlags flags)
{
    return ImGui::Begin(name, p_opened, flags);
}

CIMGUI_API bool igBegin2(CONST char* name, bool* p_opened, CONST ImVec2 size_on_first_use, float bg_alpha, ImGuiWindowFlags flags)
{
    return ImGui::Begin(name, p_opened, size_on_first_use, bg_alpha, flags);
}

CIMGUI_API void igEnd()
{
    ImGui::End();
}

CIMGUI_API bool igBeginChild(CONST char* str_id, CONST ImVec2 size, bool border, ImGuiWindowFlags extra_flags)
{
    return ImGui::BeginChild(str_id, size, border, extra_flags);
}

CIMGUI_API bool igBeginChildEx(ImGuiID id, CONST ImVec2 size, bool border, ImGuiWindowFlags extra_flags)
{
    return ImGui::BeginChild(id, size, border, extra_flags);
}

CIMGUI_API void igEndChild()
{
    ImGui::EndChild();
}

CIMGUI_API void igGetContentRegionMax(ImVec2* out)
{
    *out = ImGui::GetContentRegionMax();
}

CIMGUI_API void	igGetContentRegionAvail(struct ImVec2* out)
{
	*out = ImGui::GetContentRegionAvail();
}

CIMGUI_API float igGetContentRegionAvailWidth()
{
    return ImGui::GetContentRegionAvailWidth();
}

CIMGUI_API void igGetWindowContentRegionMin(ImVec2* out)
{
    *out = ImGui::GetWindowContentRegionMin();
}

CIMGUI_API void igGetWindowContentRegionMax(ImVec2* out)
{
    *out = ImGui::GetWindowContentRegionMax();
}

CIMGUI_API float igGetWindowContentRegionWidth()
{
    return ImGui::GetWindowContentRegionWidth();
}

CIMGUI_API ImDrawList* igGetWindowDrawList()
{
    return ImGui::GetWindowDrawList();
}

CIMGUI_API ImFont* igGetWindowFont()
{
    return ImGui::GetWindowFont();
}

CIMGUI_API float igGetWindowFontSize()
{
    return ImGui::GetWindowFontSize();
}

CIMGUI_API void igSetWindowFontScale(float scale)
{
    ImGui::SetWindowFontScale(scale);
}

CIMGUI_API void igGetWindowPos(ImVec2* out)
{
    *out = ImGui::GetWindowPos();
}

CIMGUI_API void igGetWindowSize(ImVec2* out)
{
    *out = ImGui::GetWindowSize();
}

CIMGUI_API float igGetWindowWidth()
{
    return ImGui::GetWindowWidth();
}

CIMGUI_API float igGetWindowHeight()
{
    return ImGui::GetWindowHeight();
}

CIMGUI_API bool igIsWindowCollapsed()
{
    return ImGui::IsWindowCollapsed();
}

CIMGUI_API void igSetNextWindowPos(CONST ImVec2 pos, ImGuiSetCond cond)
{
    ImGui::SetNextWindowPos(pos, cond);
}

CIMGUI_API void igSetNextWindowPosCenter(ImGuiSetCond cond)
{
    ImGui::SetNextWindowPosCenter(cond);
}

CIMGUI_API void igSetNextWindowSize(CONST ImVec2 size, ImGuiSetCond cond)
{
    ImGui::SetNextWindowSize(size, cond);
}

CIMGUI_API void igSetNextWindowContentSize(CONST ImVec2 size)
{
    ImGui::SetNextWindowContentSize(size);
}

CIMGUI_API void igSetNextWindowContentWidth(float width)
{
    ImGui::SetNextWindowContentWidth(width);
}

CIMGUI_API void igSetNextWindowCollapsed(bool collapsed, ImGuiSetCond cond)
{
    ImGui::SetNextWindowCollapsed(collapsed,cond);
}

CIMGUI_API void igSetNextWindowFocus()
{
    ImGui::SetNextWindowFocus();
}

CIMGUI_API void igSetWindowPos(CONST ImVec2 pos, ImGuiSetCond cond)
{
    ImGui::SetWindowPos(pos,cond);
}

CIMGUI_API void igSetWindowSize(CONST ImVec2 size, ImGuiSetCond cond)
{
    ImGui::SetWindowSize(size, cond);
}

CIMGUI_API void igSetWindowCollapsed(bool collapsed, ImGuiSetCond cond)
{
    ImGui::SetWindowCollapsed(collapsed,cond);
}

CIMGUI_API void igSetWindowFocus()
{
    ImGui::SetWindowFocus();
}

CIMGUI_API void igSetWindowPosByName(CONST char* name, CONST ImVec2 pos, ImGuiSetCond cond)
{
    ImGui::SetWindowPos(name,pos,cond);
}

CIMGUI_API void igSetWindowSize2(CONST char* name, CONST ImVec2 size, ImGuiSetCond cond)
{
    ImGui::SetWindowSize(name, size, cond);
}

CIMGUI_API void igSetWindowCollapsed2(CONST char* name, bool collapsed, ImGuiSetCond cond)
{
    ImGui::SetWindowCollapsed(name, collapsed, cond);
}

CIMGUI_API void igSetWindowFocus2(CONST char* name)
{
    ImGui::SetWindowFocus(name);
}

CIMGUI_API float igGetScrollX()
{
    return ImGui::GetScrollX();
}

CIMGUI_API float			igGetScrollY()
{
    return ImGui::GetScrollY();
}

CIMGUI_API float igGetScrollMaxX()
{
    return ImGui::GetScrollMaxX();
}

CIMGUI_API float			igGetScrollMaxY()
{
    return ImGui::GetScrollMaxY();
}

CIMGUI_API void igSetScrollX(float scroll_x)
{
    return ImGui::SetScrollX(scroll_x);
}

CIMGUI_API void             igSetScrollY(float scroll_y)
{
    return ImGui::SetScrollY(scroll_y);
}

CIMGUI_API void				igSetScrollHere(float center_y_ratio)
{
    ImGui::SetScrollHere(center_y_ratio);
}

CIMGUI_API void             igSetScrollFromPosY(float pos_y, float center_y_ratio)
{
    return ImGui::SetScrollFromPosY(pos_y,center_y_ratio);
}

CIMGUI_API void				igSetKeyboardFocusHere(int offset)
{
    ImGui::SetKeyboardFocusHere(offset);
}

CIMGUI_API void				igSetStateStorage(ImGuiStorage* tree)
{
    ImGui::SetStateStorage(tree);
}

CIMGUI_API ImGuiStorage*	igGetStateStorage()
{
    return ImGui::GetStateStorage();
}

// Parameters stacks (shared)
CIMGUI_API void				igPushFont(ImFont* font)
{
    ImGui::PushFont(font);
}

CIMGUI_API void				igPopFont()
{
    return ImGui::PopFont();
}

CIMGUI_API void				igPushStyleColor(ImGuiCol idx, CONST ImVec4 col)
{
    return ImGui::PushStyleColor(idx, col);
}

CIMGUI_API void				igPopStyleColor(int count)
{
    return ImGui::PopStyleColor(count);
}

CIMGUI_API void				igPushStyleVar(ImGuiStyleVar idx, float val)
{
    return ImGui::PushStyleVar(idx, val);
}

CIMGUI_API void				igPushStyleVarVec(ImGuiStyleVar idx, CONST ImVec2 val)
{
    return ImGui::PushStyleVar(idx, val);
}

CIMGUI_API void				igPopStyleVar(int count)
{
    return ImGui::PopStyleVar(count);
}

// Parameters stacks (current window)
CIMGUI_API void				igPushItemWidth(float item_width)
{
    return ImGui::PushItemWidth(item_width);
}

CIMGUI_API void				igPopItemWidth()
{
    return ImGui::PopItemWidth();
}

CIMGUI_API float			igCalcItemWidth()
{
    return ImGui::CalcItemWidth();
}

CIMGUI_API void				igPushAllowKeyboardFocus(bool v)
{
    return ImGui::PushAllowKeyboardFocus(v);
}

CIMGUI_API void				igPopAllowKeyboardFocus()
{
    return ImGui::PopAllowKeyboardFocus();
}

CIMGUI_API void				igPushTextWrapPos(float wrap_pos_x)
{
    return ImGui::PushTextWrapPos(wrap_pos_x);
}

CIMGUI_API void				igPopTextWrapPos()
{
    return ImGui::PopTextWrapPos();
}

CIMGUI_API void             igPushButtonRepeat(bool repeat)
{
    return ImGui::PushButtonRepeat(repeat);
}

CIMGUI_API void             igPopButtonRepeat()
{
    return ImGui::PopButtonRepeat();
}

// Tooltip
CIMGUI_API void				igSetTooltip(CONST char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    ImGui::SetTooltipV(fmt, args);
    va_end(args);
}

CIMGUI_API void				igSetTooltipV(CONST char* fmt, va_list args)
{
    ImGui::SetTooltipV(fmt, args);
}

CIMGUI_API void				igBeginTooltip()
{
    return ImGui::BeginTooltip();
}

CIMGUI_API void				igEndTooltip()
{
    return ImGui::EndTooltip();
}

// Popup
CIMGUI_API void				igOpenPopup(const char* str_id)
{
    return ImGui::OpenPopup(str_id);
}

CIMGUI_API bool				igBeginPopup(const char* str_id)
{
    return ImGui::BeginPopup(str_id);
}

CIMGUI_API bool             igBeginPopupModal(CONST char* name, bool* p_opened, ImGuiWindowFlags extra_flags)
{
    return ImGui::BeginPopupModal(name, p_opened, extra_flags);
}

CIMGUI_API bool             igBeginPopupContextItem(CONST char* str_id, int mouse_button)
{
    return ImGui::BeginPopupContextItem(str_id, mouse_button);
}

CIMGUI_API bool             igBeginPopupContextWindow(bool also_over_items, CONST char* str_id, int mouse_button)
{
    return ImGui::BeginPopupContextWindow(also_over_items, str_id, mouse_button);
}

CIMGUI_API bool             igBeginPopupContextVoid(CONST char* str_id, int mouse_button)
{
    return ImGui::BeginPopupContextVoid(str_id, mouse_button);
}

CIMGUI_API void				igEndPopup()
{
    return ImGui::EndPopup();
}

CIMGUI_API void				igCloseCurrentPopup()
{
    return ImGui::CloseCurrentPopup();
}

// Layout
CIMGUI_API void				igBeginGroup()
{
    return ImGui::BeginGroup();
}

CIMGUI_API void				igEndGroup()
{
    return ImGui::EndGroup();
}

CIMGUI_API void				igSeparator()
{
    return ImGui::Separator();
}

CIMGUI_API void				igSameLine(float local_pos_x, float spacing_w)
{
    return ImGui::SameLine(local_pos_x, spacing_w);
}

CIMGUI_API void				igSpacing()
{
    return ImGui::Spacing();
}

CIMGUI_API void             igDummy(CONST ImVec2* size)
{
    return ImGui::Dummy(*size);
}

CIMGUI_API void				igIndent()
{
    return ImGui::Indent();
}

CIMGUI_API void				igUnindent()
{
    return ImGui::Unindent();
}

CIMGUI_API void				igColumns(int count, CONST char* id, bool border)
{
    return ImGui::Columns(count, id, border);
}

CIMGUI_API void				igNextColumn()
{
    return ImGui::NextColumn();
}

CIMGUI_API int				igGetColumnIndex()
{
    return ImGui::GetColumnIndex();
}

CIMGUI_API float			igGetColumnOffset(int column_index)
{
    return ImGui::GetColumnOffset(column_index);
}

CIMGUI_API void				igSetColumnOffset(int column_index, float offset_x)
{
    return ImGui::SetColumnOffset(column_index, offset_x);
}

CIMGUI_API float			igGetColumnWidth(int column_index)
{
    return ImGui::GetColumnWidth(column_index);
}

CIMGUI_API int				igGetColumnsCount()
{
    return ImGui::GetColumnsCount();
}

CIMGUI_API void	igGetCursorPos(ImVec2* pOut)
{
    *pOut = ImGui::GetCursorPos();
}

CIMGUI_API float			igGetCursorPosX()
{
    return ImGui::GetCursorPosX();
}

CIMGUI_API float			igGetCursorPosY()
{
    return ImGui::GetCursorPosY();
}

CIMGUI_API void				igSetCursorPos(CONST ImVec2 local_pos)
{
    return ImGui::SetCursorPos(local_pos);
}

CIMGUI_API void				igSetCursorPosX(float x)
{
    return ImGui::SetCursorPosX(x);
}

CIMGUI_API void				igSetCursorPosY(float y)
{
    return ImGui::SetCursorPosY(y);
}

CIMGUI_API void			igGetCursorStartPos(ImVec2* pOut)
{
    *pOut = ImGui::GetCursorStartPos();
}

CIMGUI_API void igGetCursorScreenPos(ImVec2* pOut)
{
    *pOut = ImGui::GetCursorScreenPos();
}

CIMGUI_API void				igSetCursorScreenPos(CONST ImVec2 pos)
{
    return ImGui::SetCursorScreenPos(pos);
}

CIMGUI_API void				igAlignFirstTextHeightToWidgets()
{
    return ImGui::AlignFirstTextHeightToWidgets();
}

CIMGUI_API float			igGetTextLineHeight()
{
    return ImGui::GetTextLineHeight();
}

CIMGUI_API float			igGetTextLineHeightWithSpacing()
{
    return ImGui::GetTextLineHeightWithSpacing();
}

CIMGUI_API float            igGetItemsLineHeightWithSpacing()
{
    return ImGui::GetItemsLineHeightWithSpacing();
}

// ID scopes
// If you are creating widgets in a loop you most likely want to push a unique identifier so ImGui can differentiate them
// You can also use "##extra" within your widget name to distinguish them from each others (see 'Programmer Guide')
CIMGUI_API void				igPushIdStr(CONST char* str_id)
{
    return ImGui::PushID(str_id);
}

CIMGUI_API void				igPushIdStrRange(CONST char* str_begin, CONST char* str_end)
{
    return ImGui::PushID(str_begin, str_end);
}


CIMGUI_API void				igPushIdPtr(CONST void* ptr_id)
{
    return ImGui::PushID(ptr_id);
}

CIMGUI_API void				igPushIdInt(int int_id)
{
    return ImGui::PushID(int_id);
}

CIMGUI_API void				igPopId()
{
    return ImGui::PopID();
}

CIMGUI_API ImGuiID			igGetIdStr(CONST char* str_id)
{
    return ImGui::GetID(str_id);
}

CIMGUI_API ImGuiID			igGetIdStrRange(CONST char* str_begin, CONST char* str_end)
{
    return ImGui::GetID(str_begin, str_end);
}

CIMGUI_API ImGuiID			igGetIdPtr(CONST void* ptr_id)
{
    return ImGui::GetID(ptr_id);
}

// Widgets
CIMGUI_API void				igText(CONST char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    ImGui::TextV(fmt,args);
    va_end(args);
}

CIMGUI_API void				igTextV(CONST char* fmt, va_list args)
{
    ImGui::TextV(fmt, args);
}

CIMGUI_API void				igTextColored(CONST ImVec4 col, CONST char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    ImGui::TextColoredV(col, fmt, args);
    va_end(args);
}

CIMGUI_API void				igTextColoredV(CONST ImVec4 col, CONST char* fmt, va_list args)
{
    ImGui::TextColoredV(col,fmt,args);
}

CIMGUI_API void             igTextDisabled(CONST char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    ImGui::TextDisabledV(fmt, args);
    va_end(args);
}

CIMGUI_API void             igTextDisabledV(CONST char* fmt, va_list args)
{
    return ImGui::TextDisabledV(fmt,args);
}

CIMGUI_API void				igTextWrapped(CONST char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    ImGui::TextWrappedV(fmt, args);
    va_end(args);
}

CIMGUI_API void				igTextWrappedV(CONST char* fmt, va_list args)
{
    ImGui::TextWrappedV(fmt, args);
}

CIMGUI_API void				igTextUnformatted(CONST char* text, CONST char* text_end)
{
    return ImGui::TextUnformatted(text, text_end);
}

CIMGUI_API void				igLabelText(CONST char* label, CONST char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    ImGui::LabelTextV(label, fmt, args);
    va_end(args);
}

CIMGUI_API void				igLabelTextV(CONST char* label, CONST char* fmt, va_list args)
{
    ImGui::LabelTextV(label, fmt, args);
}

CIMGUI_API void				igBullet()
{
    return ImGui::Bullet();
}

CIMGUI_API void				igBulletText(CONST char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    ImGui::BulletTextV(fmt, args);
    va_end(args);
}

CIMGUI_API void				igBulletTextV(CONST char* fmt, va_list args)
{
    ImGui::BulletTextV(fmt, args);
}

CIMGUI_API bool				igButton(CONST char* label, CONST ImVec2 size)
{
    return ImGui::Button(label, size);
}

CIMGUI_API bool				igSmallButton(CONST char* label)
{
    return ImGui::SmallButton(label);
}

CIMGUI_API bool				igInvisibleButton(CONST char* str_id, CONST ImVec2 size)
{
    return ImGui::InvisibleButton(str_id, size);
}

CIMGUI_API void				igImage(ImTextureID user_texture_id, CONST ImVec2 size, CONST ImVec2 uv0, CONST ImVec2 uv1, CONST ImVec4 tint_col, CONST ImVec4 border_col)
{
    return ImGui::Image(user_texture_id, size, uv0, uv1, tint_col, border_col);
}

CIMGUI_API bool				igImageButton(ImTextureID user_texture_id, CONST ImVec2 size, CONST ImVec2 uv0, CONST ImVec2 uv1, int frame_padding, CONST ImVec4 bg_col, CONST ImVec4 tint_col)
{
    return ImGui::ImageButton(user_texture_id, size, uv0, uv1, frame_padding, bg_col, tint_col);
}

CIMGUI_API bool				igCollapsingHeader(CONST char* label, CONST char* str_id, bool display_frame, bool default_open)
{
    return ImGui::CollapsingHeader(label, str_id, display_frame, default_open);
}

CIMGUI_API bool				igCheckbox(CONST char* label, bool* v)
{
    return ImGui::Checkbox(label, v);
}

CIMGUI_API bool				igCheckboxFlags(CONST char* label, unsigned int* flags, unsigned int flags_value)
{
    return ImGui::CheckboxFlags(label, flags, flags_value);
}

CIMGUI_API bool				igRadioButtonBool(CONST char* label, bool active)
{
    return ImGui::RadioButton(label, active);
}

CIMGUI_API bool				igRadioButton(CONST char* label, int* v, int v_button)
{
    return ImGui::RadioButton(label, v, v_button);
}

CIMGUI_API bool				igCombo(CONST char* label, int* current_item, CONST char** items, int items_count, int height_in_items)
{
    return ImGui::Combo(label, current_item, items, items_count, height_in_items);
}

CIMGUI_API bool				igCombo2(CONST char* label, int* current_item, CONST char* items_separated_by_zeros, int height_in_items)
{
    return ImGui::Combo(label, current_item, items_separated_by_zeros, height_in_items);
}

CIMGUI_API bool				igCombo3(CONST char* label, int* current_item, bool(*items_getter)(void* data, int idx, CONST char** out_text), void* data, int items_count, int height_in_items)
{
    return ImGui::Combo(label, current_item, items_getter, data, items_count, height_in_items);
}

CIMGUI_API bool				igColorButton(CONST ImVec4 col, bool small_height, bool outline_border)
{
    return ImGui::ColorButton(col, small_height, outline_border);
}

CIMGUI_API bool				igColorEdit3(CONST char* label, float col[3])
{
    return ImGui::ColorEdit3(label, col);
}

CIMGUI_API bool				igColorEdit4(CONST char* label, float col[4], bool show_alpha)
{
    return ImGui::ColorEdit4(label, col, show_alpha);
}

CIMGUI_API void				igColorEditMode(ImGuiColorEditMode mode)
{
    return ImGui::ColorEditMode(mode);
}

CIMGUI_API void				igPlotLines(CONST char* label, CONST float* values, int values_count, int values_offset, CONST char* overlay_text, float scale_min, float scale_max, ImVec2 graph_size, int stride)
{
    return ImGui::PlotLines(label, values, values_count, values_offset, overlay_text, scale_min, scale_max, graph_size, stride);
}

CIMGUI_API void				igPlotLines2(CONST char* label, float(*values_getter)(void* data, int idx), void* data, int values_count, int values_offset, CONST char* overlay_text, float scale_min, float scale_max, ImVec2 graph_size)
{
    return ImGui::PlotLines(label, values_getter, data, values_count, values_offset, overlay_text, scale_min, scale_max, graph_size);
}

CIMGUI_API void				igPlotHistogram(CONST char* label, CONST float* values, int values_count, int values_offset, CONST char* overlay_text, float scale_min, float scale_max, ImVec2 graph_size, int stride)
{
    return ImGui::PlotHistogram(label,values, values_count, values_offset, overlay_text, scale_min, scale_max, graph_size, stride);
}

CIMGUI_API void				igPlotHistogram2(CONST char* label, float(*values_getter)(void* data, int idx), void* data, int values_count, int values_offset, CONST char* overlay_text, float scale_min, float scale_max, ImVec2 graph_size)
{
    return ImGui::PlotHistogram(label, values_getter, data, values_count, values_offset, overlay_text, scale_min, scale_max, graph_size);
}

// Widgets: Sliders (tip: ctrl+click on a slider to input text)
CIMGUI_API bool				igSliderFloat(CONST char* label, float* v, float v_min, float v_max, CONST char* display_format, float power)
{
    return ImGui::SliderFloat(label, v, v_min, v_max, display_format, power);
}

CIMGUI_API bool				igSliderFloat2(CONST char* label, float v[2], float v_min, float v_max, CONST char* display_format, float power)
{
    return ImGui::SliderFloat(label, v, v_min, v_max, display_format, power);
}

CIMGUI_API bool				igSliderFloat3(CONST char* label, float v[3], float v_min, float v_max, CONST char* display_format, float power)
{
    return ImGui::SliderFloat3(label, v, v_min, v_max, display_format, power);
}

CIMGUI_API bool				igSliderFloat4(CONST char* label, float v[4], float v_min, float v_max, CONST char* display_format, float power)
{
    return ImGui::SliderFloat4(label, v, v_min, v_max, display_format, power);
}

CIMGUI_API bool				igSliderAngle(CONST char* label, float* v_rad, float v_degrees_min, float v_degrees_max)
{
    return ImGui::SliderAngle(label, v_rad, v_degrees_min, v_degrees_max);
}

CIMGUI_API bool				igSliderInt(CONST char* label, int* v, int v_min, int v_max, CONST char* display_format)
{
    return ImGui::SliderInt(label, v, v_min, v_max, display_format);
}

CIMGUI_API bool				igSliderInt2(CONST char* label, int v[2], int v_min, int v_max, CONST char* display_format)
{
    return ImGui::SliderInt2(label, v, v_min, v_max, display_format);
}

CIMGUI_API bool				igSliderInt3(CONST char* label, int v[3], int v_min, int v_max, CONST char* display_format)
{
    return ImGui::SliderInt3(label, v, v_min, v_max, display_format);
}

CIMGUI_API bool				igSliderInt4(CONST char* label, int v[4], int v_min, int v_max, CONST char* display_format)
{
    return ImGui::SliderInt4(label, v, v_min, v_max, display_format);
}

CIMGUI_API bool				igVSliderFloat(CONST char* label, CONST ImVec2 size, float* v, float v_min, float v_max, CONST char* display_format, float power)
{
    return ImGui::VSliderFloat(label, size, v, v_min, v_max, display_format, power);
}

CIMGUI_API bool				igVSliderInt(CONST char* label, CONST ImVec2 size, int* v, int v_min, int v_max, CONST char* display_format)
{
    return ImGui::VSliderInt(label, size, v, v_min, v_max, display_format);
}

// Widgets: Drags (tip: ctrl+click on a drag box to input text)
CIMGUI_API bool          	igDragFloat(CONST char* label, float* v, float v_speed, float v_min, float v_max, CONST char* display_format, float power)
{
    return ImGui::DragFloat(label, v, v_speed, v_min, v_max, display_format, power);
}

CIMGUI_API bool          	igDragFloat2(CONST char* label, float v[2], float v_speed, float v_min, float v_max, CONST char* display_format, float power)
{
    return ImGui::DragFloat2(label, v, v_speed, v_min, v_max, display_format, power);
}

CIMGUI_API bool          	igDragFloat3(CONST char* label, float v[3], float v_speed, float v_min, float v_max, CONST char* display_format, float power)
{
    return ImGui::DragFloat3(label, v, v_speed, v_min, v_max, display_format, power);
}

CIMGUI_API bool          	igDragFloat4(CONST char* label, float v[4], float v_speed, float v_min, float v_max, CONST char* display_format, float power)
{
    return ImGui::DragFloat4(label, v, v_speed, v_min, v_max, display_format, power);
}

CIMGUI_API bool             igDragFloatRange2(const char* label, float* v_current_min, float* v_current_max, float v_speed, float v_min, float v_max, const char* display_format, const char* display_format_max, float power)
{
    return ImGui::DragFloatRange2(label,v_current_min,v_current_max,v_speed,v_min,v_max,display_format,display_format_max,power);
}

CIMGUI_API bool          	igDragInt(CONST char* label, int* v, float v_speed, int v_min, int v_max, CONST char* display_format)
{
    return ImGui::DragInt(label, v, v_speed, v_min, v_max, display_format);
}

CIMGUI_API bool          	igDragInt2(CONST char* label, int v[2], float v_speed, int v_min, int v_max, CONST char* display_format)
{
    return ImGui::DragInt2(label, v, v_speed, v_min, v_max, display_format);
}

CIMGUI_API bool          	igDragInt3(CONST char* label, int v[3], float v_speed, int v_min, int v_max, CONST char* display_format)
{
    return ImGui::DragInt3(label, v, v_speed, v_min, v_max, display_format);
}

CIMGUI_API bool          	igDragInt4(CONST char* label, int v[4], float v_speed, int v_min, int v_max, CONST char* display_format)
{
    return ImGui::DragInt4(label, v, v_speed, v_min, v_max, display_format);
}

CIMGUI_API bool             igDragIntRange2(const char* label, int* v_current_min, int* v_current_max, float v_speed, int v_min, int v_max, const char* display_format, const char* display_format_max)
{
    return ImGui::DragIntRange2(label,v_current_min,v_current_max,v_speed,v_min,v_max,display_format,display_format_max);
}

// Widgets: Input
CIMGUI_API bool				igInputText(CONST char* label, char* buf, size_t buf_size, ImGuiInputTextFlags flags, ImGuiTextEditCallback callback, void* user_data)
{
    return ImGui::InputText(label, buf, buf_size, flags, callback, user_data);
}

CIMGUI_API bool             igInputTextMultiline(CONST char* label, char* buf, size_t buf_size, CONST ImVec2 size, ImGuiInputTextFlags flags, ImGuiTextEditCallback callback, void* user_data)
{
    return ImGui::InputTextMultiline(label, buf, buf_size, size, flags, callback, user_data);
}

CIMGUI_API bool				igInputFloat(CONST char* label, float* v, float step, float step_fast, int decimal_precision, ImGuiInputTextFlags extra_flags)
{
    return ImGui::InputFloat(label, v, step, step_fast, decimal_precision, extra_flags);
}

CIMGUI_API bool				igInputFloat2(CONST char* label, float v[2], int decimal_precision, ImGuiInputTextFlags extra_flags)
{
    return ImGui::InputFloat2(label, v, decimal_precision, extra_flags);
}

CIMGUI_API bool				igInputFloat3(CONST char* label, float v[3], int decimal_precision, ImGuiInputTextFlags extra_flags)
{
    return ImGui::InputFloat3(label, v, decimal_precision, extra_flags);
}

CIMGUI_API bool				igInputFloat4(CONST char* label, float v[4], int decimal_precision, ImGuiInputTextFlags extra_flags)
{
    return ImGui::InputFloat4(label, v, decimal_precision, extra_flags);
}

CIMGUI_API bool				igInputInt(CONST char* label, int* v, int step, int step_fast, ImGuiInputTextFlags extra_flags)
{
    return ImGui::InputInt(label, v, step, step_fast, extra_flags);
}

CIMGUI_API bool				igInputInt2(CONST char* label, int v[2], ImGuiInputTextFlags extra_flags)
{
    return ImGui::InputInt2(label, v, extra_flags);
}

CIMGUI_API bool				igInputInt3(CONST char* label, int v[3], ImGuiInputTextFlags extra_flags)
{
    return ImGui::InputInt3(label, v, extra_flags);
}

CIMGUI_API bool				igInputInt4(CONST char* label, int v[4], ImGuiInputTextFlags extra_flags)
{
    return ImGui::InputInt4(label, v, extra_flags);
}


// Widgets: Trees
CIMGUI_API bool				igTreeNode(CONST char* str_label_id)
{
    return ImGui::TreeNode(str_label_id);
}

CIMGUI_API bool				igTreeNodeStr(CONST char* str_id, CONST char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    bool res = ImGui::TreeNodeV(str_id, fmt, args);
    va_end(args);

    return res;
}

CIMGUI_API bool				igTreeNodePtr(CONST void* ptr_id, CONST char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    bool res = ImGui::TreeNodeV(ptr_id, fmt, args);
    va_end(args);

    return res;
}

CIMGUI_API bool				igTreeNodeStrV(CONST char* str_id, CONST char* fmt, va_list args)
{
    return ImGui::TreeNodeV(str_id,fmt,args);
}

CIMGUI_API bool				igTreeNodePtrV(CONST void* ptr_id, CONST char* fmt, va_list args)
{
    return ImGui::TreeNodeV(ptr_id, fmt, args);
}

CIMGUI_API void				igTreePushStr(CONST char* str_id)
{
    return ImGui::TreePush(str_id);
}

CIMGUI_API void				igTreePushPtr(CONST void* ptr_id)
{
    return ImGui::TreePush(ptr_id);
}

CIMGUI_API void				igTreePop()
{
    return ImGui::TreePop();
}

CIMGUI_API void				igSetNextTreeNodeOpened(bool opened, ImGuiSetCond cond)
{
    return ImGui::SetNextTreeNodeOpened(opened, cond);
}

// Widgets: Selectable / Lists
CIMGUI_API bool             igSelectable(CONST char* label, bool selected, ImGuiSelectableFlags flags, CONST ImVec2 size)
{
    return ImGui::Selectable(label, selected, flags, size);
}

CIMGUI_API bool             igSelectableEx(CONST char* label, bool* p_selected, ImGuiSelectableFlags flags, CONST ImVec2 size)
{
    return ImGui::Selectable(label, p_selected, flags, size);
}

CIMGUI_API bool				igListBox(CONST char* label, int* current_item, CONST char** items, int items_count, int height_in_items)
{
    return ImGui::ListBox(label, current_item, items, items_count, height_in_items);
}

CIMGUI_API bool				igListBox2(CONST char* label, int* current_item, bool(*items_getter)(void* data, int idx, CONST char** out_text), void* data, int items_count, int height_in_items)
{
    return ImGui::ListBox(label, current_item, items_getter,data,items_count,height_in_items);
}

CIMGUI_API bool				igListBoxHeader(CONST char* label, CONST ImVec2 size)
{
    return ImGui::ListBoxHeader(label, size);
}

CIMGUI_API bool				igListBoxHeader2(CONST char* label, int items_count, int height_in_items)
{
    return ImGui::ListBoxHeader(label,items_count,height_in_items);
}

CIMGUI_API void				igListBoxFooter()
{
    return ImGui::ListBoxFooter();
}

CIMGUI_API bool             igBeginMainMenuBar()
{
    return ImGui::BeginMainMenuBar();
}

CIMGUI_API void             igEndMainMenuBar()
{
    return ImGui::EndMainMenuBar();
}

CIMGUI_API bool             igBeginMenuBar()
{
    return ImGui::BeginMenuBar();
}

CIMGUI_API void             igEndMenuBar()
{
    return ImGui::EndMenuBar();
}

CIMGUI_API bool             igBeginMenu(CONST char* label, bool enabled)
{
    return ImGui::BeginMenu(label, enabled);
}

CIMGUI_API void             igEndMenu()
{
    return ImGui::EndMenu();
}

CIMGUI_API bool             igMenuItem(CONST char* label, CONST char* shortcut, bool selected, bool enabled)
{
    return ImGui::MenuItem(label, shortcut, selected, enabled);
}

CIMGUI_API bool             igMenuItemPtr(CONST char* label, CONST char* shortcut, bool* p_selected, bool enabled)
{
    return ImGui::MenuItem(label, shortcut, p_selected, enabled);
}

// Widgets: Value() Helpers. Output single value in "name: value" format (tip: freely declare your own within the ImGui namespace!)
CIMGUI_API void				igValueBool(CONST char* prefix, bool b)
{
    ImGui::Value(prefix, b);
}

CIMGUI_API void				igValueInt(CONST char* prefix, int v)
{
    ImGui::Value(prefix, v);
}

CIMGUI_API void				igValueUInt(CONST char* prefix, unsigned int v)
{
    ImGui::Value(prefix, v);
}

CIMGUI_API void				igValueFloat(CONST char* prefix, float v, CONST char* float_format)
{
    ImGui::Value(prefix,v,float_format);
}

CIMGUI_API void				igColor(CONST char* prefix, CONST ImVec4 v)
{
    ImGui::Color(prefix,v);
}

CIMGUI_API void				igColor2(CONST char* prefix, unsigned int v)
{
    ImGui::Color(prefix,v);
}

// Logging: all text output from interface is redirected to tty/file/clipboard. Tree nodes are automatically opened.
CIMGUI_API void				igLogToTTY(int max_depth)
{
    ImGui::LogToTTY(max_depth);
}

CIMGUI_API void				igLogToFile(int max_depth, CONST char* filename)
{
    ImGui::LogToFile(max_depth,filename);
}

CIMGUI_API void				igLogToClipboard(int max_depth)
{
    ImGui::LogToClipboard(max_depth);
}

CIMGUI_API void				igLogFinish()
{
    ImGui::LogFinish();
}

CIMGUI_API void				igLogButtons()
{
    ImGui::LogButtons();
}

CIMGUI_API void				igLogText(CONST char* fmt, ...)
{
    char buffer[256];
    va_list args;
    va_start(args, fmt);
    snprintf(buffer, 256, fmt, args);
    va_end(args);

    ImGui::LogText("%s",buffer);
}

// Utilities
CIMGUI_API bool				igIsItemHovered()
{
    return ImGui::IsItemHovered();
}

CIMGUI_API bool				igIsItemHoveredRect()
{
    return ImGui::IsItemHoveredRect();
}

CIMGUI_API bool				igIsItemActive()
{
    return ImGui::IsItemActive();
}

CIMGUI_API bool             igIsItemVisible()
{
    return ImGui::IsItemVisible();
}

CIMGUI_API bool             igIsAnyItemHovered()
{
    return ImGui::IsAnyItemHovered();
}

CIMGUI_API bool				igIsAnyItemActive()
{
    return ImGui::IsAnyItemActive();
}

CIMGUI_API void igGetItemRectMin(ImVec2* pOut)
{
    *pOut = ImGui::GetItemRectMin();
}

CIMGUI_API void igGetItemRectMax(ImVec2* pOut)
{
    *pOut = ImGui::GetItemRectMax();
}

CIMGUI_API void	igGetItemRectSize(ImVec2* pOut)
{
    *pOut = ImGui::GetItemRectSize();
}

CIMGUI_API bool             igIsWindowHovered()
{
    return ImGui::IsWindowHovered();
}

CIMGUI_API bool				igIsWindowFocused()
{
    return ImGui::IsWindowFocused();
}

CIMGUI_API bool				igIsRootWindowFocused()
{
    return ImGui::IsRootWindowFocused();
}

CIMGUI_API bool				igIsRootWindowOrAnyChildFocused()
{
    return ImGui::IsRootWindowOrAnyChildFocused();
}

CIMGUI_API bool				igIsRectVisible(CONST ImVec2 item_size)
{
    return ImGui::IsRectVisible(item_size);
}

CIMGUI_API bool             igIsKeyDown(int key_index)
{
    return ImGui::IsKeyDown(key_index);
}

CIMGUI_API bool				igIsKeyPressed(int key_index, bool repeat)
{
    return ImGui::IsKeyPressed(key_index,repeat);
}

CIMGUI_API bool             igIsKeyReleased(int key_index)
{
    return ImGui::IsKeyReleased(key_index);
}

CIMGUI_API bool             igIsMouseDown(int button)
{
    return ImGui::IsMouseDown(button);
}

CIMGUI_API bool				igIsMouseClicked(int button, bool repeat)
{
    return ImGui::IsMouseClicked(button, repeat);
}

CIMGUI_API bool				igIsMouseDoubleClicked(int button)
{
    return ImGui::IsMouseDoubleClicked(button);
}

CIMGUI_API bool             igIsMouseReleased(int button)
{
    return ImGui::IsMouseReleased(button);
}

CIMGUI_API bool				igIsMouseHoveringWindow()
{
    return ImGui::IsMouseHoveringWindow();
}

CIMGUI_API bool				igIsMouseHoveringAnyWindow()
{
    return ImGui::IsMouseHoveringAnyWindow();
}

CIMGUI_API bool				igIsMouseHoveringRect(CONST ImVec2 pos_min, CONST ImVec2 pos_max, bool clip)
{
    return ImGui::IsMouseHoveringRect(pos_min,pos_max,clip);
}

CIMGUI_API bool				igIsMouseDragging(int button, float lock_threshold)
{
    return ImGui::IsMouseDragging(button,lock_threshold);
}
CIMGUI_API bool				igIsPosHoveringAnyWindow(CONST ImVec2 pos)
{
    return ImGui::IsPosHoveringAnyWindow(pos);
}

CIMGUI_API void igGetMousePos(ImVec2* pOut)
{
    *pOut = ImGui::GetMousePos();
}

CIMGUI_API void igGetMousePosOnOpeningCurrentPopup(ImVec2* pOut)
{
    *pOut = ImGui::GetMousePosOnOpeningCurrentPopup();
}

CIMGUI_API void igGetMouseDragDelta(ImVec2* pOut, int button, float lock_threshold)
{
    *pOut = ImGui::GetMouseDragDelta(button,lock_threshold);
}

CIMGUI_API void igResetMouseDragDelta(int button)
{
    ImGui::ResetMouseDragDelta(button);
}

CIMGUI_API ImGuiMouseCursor igGetMouseCursor()
{
    return ImGui::GetMouseCursor();
}

CIMGUI_API void				igSetMouseCursor(ImGuiMouseCursor type)
{
    ImGui::SetMouseCursor(type);
}

CIMGUI_API void igCaptureKeyboardFromApp()
{
    return ImGui::CaptureKeyboardFromApp();
}

CIMGUI_API void igCaptureMouseFromApp()
{
    return ImGui::CaptureMouseFromApp();
}

CIMGUI_API void* igMemAlloc(size_t sz)
{
    return ImGui::MemAlloc(sz);
}

CIMGUI_API void igMemFree(void* ptr)
{
    return ImGui::MemFree(ptr);
}

CIMGUI_API const char* igGetClipboardText()
{
    return ImGui::GetClipboardText();
}

CIMGUI_API void igSetClipboardText(const char* text)
{
    return ImGui::SetClipboardText(text);
}

CIMGUI_API float			igGetTime()
{
    return ImGui::GetTime();
}

CIMGUI_API int				igGetFrameCount()
{
    return ImGui::GetFrameCount();
}

CIMGUI_API CONST char*		igGetStyleColName(ImGuiCol idx)
{
    return ImGui::GetStyleColName(idx);
}

CIMGUI_API void igCalcItemRectClosestPoint(ImVec2* pOut, CONST ImVec2 pos, bool on_edge, float outward)
{
    *pOut = ImGui::CalcItemRectClosestPoint(pos,on_edge,outward);
}

CIMGUI_API void igCalcTextSize(ImVec2* pOut, CONST char* text, CONST char* text_end, bool hide_text_after_double_hash, float wrap_width)
{
    *pOut = ImGui::CalcTextSize(text, text_end, hide_text_after_double_hash, wrap_width);
}

CIMGUI_API void				igCalcListClipping(int items_count, float items_height, int* out_items_display_start, int* out_items_display_end)
{
    ImGui::CalcListClipping(items_count,items_height,out_items_display_start,out_items_display_end);
}

CIMGUI_API bool				igBeginChildFrame(ImGuiID id, CONST ImVec2 size, ImGuiWindowFlags extra_flags)
{
    return ImGui::BeginChildFrame(id, size, extra_flags);
}

CIMGUI_API void				igEndChildFrame()
{
    ImGui::EndChildFrame();
}

CIMGUI_API void igColorConvertU32ToFloat4(ImVec4* pOut, ImU32 in)
{
    *pOut = ImGui::ColorConvertU32ToFloat4(in);
}

CIMGUI_API ImU32			igColorConvertFloat4ToU32(CONST ImVec4 in)
{
    return ImGui::ColorConvertFloat4ToU32(in);
}

CIMGUI_API void				igColorConvertRGBtoHSV(float r, float g, float b, float* out_h, float* out_s, float* out_v)
{
    ImGui::ColorConvertRGBtoHSV(r, g, b, *out_h, *out_s, *out_v);
}

CIMGUI_API void				igColorConvertHSVtoRGB(float h, float s, float v, float* out_r, float* out_g, float* out_b)
{
    ImGui::ColorConvertHSVtoRGB(h,s,v,*out_r,*out_g,*out_b);
}

CIMGUI_API CONST char*		igGetVersion()
{
    return ImGui::GetVersion();
}

CIMGUI_API void*			igGetInternalState()
{
    return ImGui::GetInternalState();
}

CIMGUI_API size_t			igGetInternalStateSize()
{
    return ImGui::GetInternalStateSize();
}

CIMGUI_API void	igSetInternalState(void* state, bool construct)
{
    ImGui::SetInternalState(state,construct);
}

CIMGUI_API void ImGuiIO_AddInputCharacter(unsigned short c)
{
    ImGui::GetIO().AddInputCharacter(c);
}

CIMGUI_API void ImGuiIO_AddInputCharactersUTF8(CONST char* utf8_chars)
{
    return ImGui::GetIO().AddInputCharactersUTF8(utf8_chars);
}

// METASHADER

CIMGUI_API bool igWantCaptureMouse()
{
    return ImGui::GetIO().WantCaptureMouse;
}

CIMGUI_API void igIO_Fonts_GetTexDataAsAlpha8(unsigned char** out_pixels, int* out_width, int* out_height)
{
    ImGui::GetIO().Fonts->GetTexDataAsAlpha8(out_pixels, out_width, out_height);
}

CIMGUI_API void igIO_Fonts_GetTexDataAsRGBA32(unsigned char** out_pixels, int* out_width, int* out_height)
{
    ImGui::GetIO().Fonts->GetTexDataAsRGBA32(out_pixels, out_width, out_height);
}

CIMGUI_API void igIO_Fonts_SetTexID(void* texID)
{
    ImGui::GetIO().Fonts->TexID = texID;
}

CIMGUI_API void igIO_Fonts_ClearInputData()
{
    ImGui::GetIO().Fonts->ClearInputData();
}

CIMGUI_API void igIO_Fonts_ClearTexData()
{
    ImGui::GetIO().Fonts->ClearTexData();    
}

CIMGUI_API void igScaleClipRectsByFramebufferHeight(ImDrawData* draw_data)
{
    ImGuiIO& io = ImGui::GetIO();
    float fb_height = io.DisplaySize.y * io.DisplayFramebufferScale.y;
    draw_data->ScaleClipRects(io.DisplayFramebufferScale);
}

CIMGUI_API void igGetDisplaySize(float *x, float *y)
{
    *x = ImGui::GetIO().DisplaySize.x;
    *y = ImGui::GetIO().DisplaySize.y;
}