# Selos

This repository contains source code supporting the SIGGRAPH Asia 2019 paper "Staged Metaprogramming for Shader System Development" by Kerry A. Seitz, Jr., Tim Foley, Serban D. Porumbescu, and John D. Owens.

The definitive version of the paper is published in ACM Transactions on Graphics, Vol. 38, No. 6, Article 202 (November 2019). DOI: [https://doi.org/10.1145/3355089.3356554](https://doi.org/10.1145/3355089.3356554)

The author's version of the paper can be downloaded at [https://escholarship.org/uc/item/2f8448n2](https://escholarship.org/uc/item/2f8448n2)


Getting Started
===============

**Note:** This code is currently tested only on 64-bit Windows.  All pre-built binaries are built for 64-bit Windows only.


Required Software
=================

1) Microsoft Visual Studio 2013: [https://visualstudio.microsoft.com/vs/older-downloads/](https://visualstudio.microsoft.com/vs/older-downloads/)

   **Note:** Also known as Microsoft Visual Studio 12.0

2) Terra and its prerequisites: [terralang.org](http://terralang.org)

   Direct link to download: [terra-Windows-x86\_64-332a506.zip](https://github.com/terralang/terra/releases/download/release-2016-03-25/terra-Windows-x86_64-332a506.zip)

   **Note 1:** Our code currently works with Terra release 2016-03-25: [https://github.com/zdevito/terra/releases/tag/release-2016-03-25](https://github.com/zdevito/terra/releases/tag/release-2016-03-25)

   **Note 2:** The pre-build binaries are sufficient.  However, if you choose to build Terra from source:

   * Be sure to check out the commit corresponding to the release specified above [332a506](https://github.com/zdevito/terra/commit/332a506f25a4d576c40adac4b5d21fd92c37d0cc)

   * We recommend building LLVM and Clang in x64 Release mode for easiest compatibility with Terra.  Make sure you get the version of LLVM and Clang that Terra specifies.  Also, LLVM's CMAKE\_INSTALL\_PREFIX path may default to a privileged location, so you may need to manually modify that CMake argument for proper installation.  Instructions for building LLVM and Clang can be found here: [llvm.org/docs/GettingStarted.html](http://llvm.org/docs/GettingStarted.html)

   * You do not need to manually build LuaJIT.  The Terra build process will build it automatically.


3) SDL 2.0: [libsdl.org](https://www.libsdl.org/)

   Direct link to download: [SDL2-devel-2.0.8-VC.zip](https://www.libsdl.org/release/SDL2-devel-2.0.8-VC.zip)

   **Note 1:** Download the "Development Libraries" for SDL2 in order to obtain both the binaries and the required headers.

   **Note 2:** The SDL2 binary download should be sufficient, but you can build from source if desired.

   **Note 3:** This code was tested using SDL 2.0.8, which can be downloaded at [libsdl.org/release](https://www.libsdl.org/release/) (specifically SDL2-devel-2.0.8-VC.zip)

4) UE4 Sun Temple scene (version 1): [https://escholarship.org/uc/item/2f8448n2#supplemental](https://escholarship.org/uc/item/2f8448n2#supplemental)

   Direct link to download (~800 MB): [SunTemple\_v1.zip](https://escholarship.org/content/qt2f8448n2/supp/SunTemple_v1.zip)

   **Note:** This code was tested with version 1 of the UE4 Sun Temple scene. The performance analysis in the paper also uses version 1. However, this version is no longer available from the ORCA: Open Research Content Archive website, so please download it from the links provided above.

Installing
==========

1) Clone the Selos repository. We'll call the directory into which you cloned the repository: `$SELOS`

   ```Shell
   # Make sure to clone with --recursive
   git clone --recursive https://github.com/kseitz/selos.git
   ```

   **Note:** If you didn't clone with the `--recursive` flag, then you need to manually clone the submodules:

   ```Shell
   cd $SELOS
   git submodule update --init --recursive
   ```

2) Copy your SDL download to `$SELOS/external/SDL2`

   **Note:** You should have the following SDL files:

   ```Shell
   $SELOS/external/SDL2/include/SDL.h
   $SELOS/external/SDL2/include/SDL_opengl.h
   $SELOS/external/SDL2/lib/x64/SDL2.dll
   ```

3) Copy your UE Sun Temple (version 1) download to `$SELOS/examples/assets/models`

   **Note:** You should have the following Sun Temple file and folder:

   ```Shell
   $SELOS/examples/assets/models/SunTemple_v1/SunTemple.fbx
   $SELOS/examples/assets/models/SunTemple_v1/Textures/
   ```

4) Add the following directories to your `PATH` environment variable:

   ```Shell
   $SELOS/external/built/bin/x64
   $SELOS/external/SDL2/lib/x64
   ```

   **Note:** We also recommend adding the path to `terra.exe` to your `PATH` environment variable.


Running the Deferred Renderer
=============================

1) Compile the shaders and the runtime application:

   ```Shell
   cd $SELOS/examples/deferredRenderer
   terra compile.t
   ```

2) Run the application:

   ```Shell
   cd $SELOS/examples/deferredRenderer/build
   runGame-OpenGL.exe
   ```

   **Note:** By default, `compile.t` generates GLSL shaders and compiles the runtime application to use the OpenGL implementation. To generate HLSL shaders and compile the application to use the Direct3D 11 implementation, use the `--d3d11` argument:

   ```Shell
   cd $SELOS/examples/deferredRenderer
   terra compile.t --d3d11
   ```

   ```Shell
   cd $SELOS/examples/deferredRenderer/build
   runGame-D3D11.exe
   ```

You can also compile the shaders and the runtime application separately:

```Shell
cd $SELOS/examples/deferredRenderer
terra compileShaders.t
terra compileRuntime.t
```

This functionality is useful if, for example, you want to hot reload shaders while the application is running. You can modify the logic of a shader, run `terra compileShaders.t`, and then click the `Reload Shader Cache` GUI button. However, if the interface to a shader changes (e.g., if you add or remove a parameter), then the application must be recompiled.

Both `compileShaders.t` and `compileRuntime.t` also support the `--d3d11` argument.


Running the Specialization Design Space Exploration
===================================================

To run the specialization design space exploration case study as presented in the paper, use the `-a` argument when launching the runtime application:

```Shell
cd $SELOS/examples/deferredRenderer
runGame-OpenGL.exe -a
```

or

```Shell
cd $SELOS/examples/deferredRenderer
runGame-D3D11.exe -a
```

**Note** This will take a long time to complete. You can adjust some testing parameters in `$SELOS/examples/deferredRenderer/constants.t` to decrease testing time at the cost of accuracy.

Results are written to `$SELOS/examples/deferredRenderer/build/output/{D3D11,OpenGL]/`, including partial results as the test is running.

You can also run individual parts of the test by specifying how many features you want to allow to be specialized. Use the `-sN` argument for this purpose, where `N` is the number of features to specialize (0 <= `N` <= 6). For example, if you want to allow only 3 features to be specialized at a time:

   ```Shell
   cd $SELOS/examples/deferredRenderer
   runGame-OpenGL.exe -s3
   ```

  or

   ```Shell
   cd $SELOS/examples/deferredRenderer
   runGame-D3D11.exe -s3
   ```


Using the GLSL/HLSL Backends with Other Projects
================================================

**DISCLAIMER:** The below information has not been updated recently. Thus, it may not be completely consistent with the current implementation. For example, Selos supports compute shaders, but the documentation below only discusses vertex and fragment shaders. However, the information should still be a useful starting point for anyone interested in using the GLSL and HLSL backends for Terra separate from Selos. Please feel free to post an issue on the Selos GitHub repo if you would like further assistance.

**Note:** Below is some brief documentation for how to use the GLSL and HLSL Terra backends.  This documentation will be useful if you want to use the backends with external projects.  The Selos Shader Intermediate Representation (SIR) handles interfacing with the backends, so you will likely not need to read this documentation if you are using the SIR for your code.


The GLSL and HLSL backends can be used directly through the functions returned by the files `$SELOS/src/terraToGLSL.t` and `$SELOS/src/terraToHLSL.t` (respectively).  These functions take a single table as an input, and each entry in the table should be a Lua array (e.g., created from `terralib.newlist()`).  The entires required, as well as the format for each array's entries, are detailed below:

  * uniformBlocksList
    ```Lua
    entry = {
      name = <name (string)>,
      binding = <binding (integer; optional)>,
      uniformsList = <Lua array of uniforms in this block, with each entry formatted as:
        entry = {
          name = <name>,
          decl = <a Terra global declaration for this item>
        }
      >
    }
    ```

  * textureSamplersList
    ```Lua
    entry = {
      name    = <name (string)>,
      binding = <binding (integer; optional)>,
      decl    = <a Terra global declaration for this item>
    }
    ```
  
  * uniformsList, inputsList, varyingsList, and outputsList
    ```Lua
    entry = {
      name     = <name (string)>,
      location = <location (integer; optional)>,
      decl     = <a Terra global declaration for this item>
    }
    ```
  
  * vertexCode and fragmentCode
    ```Lua
    entry = {
      <a Terra quote>
    }
    ```


  **Note:** `inputsList` is for vertex shader inputs; `outputsList` is for fragment shader outputs; and `varyingsList` is for vertex shader outputs and fragment shader inputs.


These functions return a table with two string entires named `vertexSourceCode` and `fragmentSourceCode` for the vertex and fragment, respectively, GLSL/HLSL for the shader.

We provide GLSL/HLSL builtin types and functions in `$SELOS/src/builtin.t`.  The backends will automatically replace uses of these builtins with the corresponding GLSL/HLSL builtins (if applicable).

Some system-provided inputs and outputs (e.g., equivalents to `gl_FragCoord` and `gl_Position`) are also specified in `$SELOS/src/builtin.t` in the `Builtin.systemInputs` and `Builtin.systemOutputs` tables, respectively.  See the bottom of this file to see which inputs/outputs we provide and how they are named.
