#
# Cross Platform Makefile
# Compatible with Ubuntu 14.04.1 and Mac OS X

OBJS = cimgui.o
OBJS += fontAtlas.o 
OBJS += drawList.o
#OBJS += test.o
OBJS += ../imgui/imgui.o
OBJS += ../imgui/imgui_draw.o
OBJS += ../imgui/imgui_demo.o
OBJS += ../imgui/examples/sdl_opengl_example/imgui_impl_sdl.o

UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S), Linux) #LINUX
	ECHO_MESSAGE = "Linux"

	OUTPUTNAME = cimgui.so
	OUTPUTNAMESTATIC = cimgui.a
	CXXFLAGS = -I../imgui/
	CXXFLAGS += -I../../SDL2/include/
	CXXFLAGS += -Wall
	CXXFLAGS += -shared -fPIC
	CFLAGS = $(CXXFLAGS)
endif

ifeq ($(UNAME_S), Darwin) #APPLE
	ECHO_MESSAGE = "Mac OS X"

	OUTPUTNAME = cimgui.dylib
	CXXFLAGS = -I/usr/local/include
	CXXFLAGS += -Wall
	LINKFLAGS = -dynamiclib
	CFLAGS = $(CXXFLAGS)
endif

.cpp.o:
	$(CXX) $(CXXFLAGS) -c -o $@ $<

all:imgui_example
	@echo Build complete for $(ECHO_MESSAGE)

imgui_example:$(OBJS)
	$(CXX) -o $(OUTPUTNAME) $(OBJS) $(CXXFLAGS) $(LINKFLAGS)
	ar rcs $(OUTPUTNAMESTATIC) $(OBJS)

clean:
	rm $(OBJS)
