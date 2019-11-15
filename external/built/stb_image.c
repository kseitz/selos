// Use this file to compile stb_image.h to a DLL and/or LIB.
// In stb_image.h, change the #define STBIDEF line to include __declspec(dllexport)
// before compiling.

// Use the following command line to compile to a DLL:
// cl.exe /DSTB_IMAGE_IMPLEMENTATION /I..\stb stb_image.c /link /out:x64\stbi.dll /DLL

// Use the following command line to compile to a SO:
// gcc -I../stb/ -DSTB_IMAGE_IMPLEMENTATION -shared -o x64/libSTBI.so -fPIC stb_image.c

// Use the following command line to compile to a Linux .a
// gcc -I../stb/ -DSTB_IMAGE_IMPLEMENTATION -c -o stb_image.o stb_image.c
// ar rcs x64/libSTBI.a  stb_image.o

#include "stb_image.h"
