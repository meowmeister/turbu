How to download and install the TURBU source package from SVN:

It takes a few steps to configure the TURBU package:

1. Main project code
2. DLLs
3. Dependencies
4. JVCL
5. Patches
6. Build
----------------------

1. Main project code
----------------------
Check out the /trunk/turbu folder to someplace inside your RAD Studio projects folder.  (NOTE: TURBU will only compile if you have Delphi XE.  It uses some RTTI and Generics tricks that won't work under D2010 or earlier.)


2. DLLs
----------------------
Check out the /trunk/dlls folder.  All the DLLs in this folder need to go in the \turbu\bin folder where the compiler places the EXE.

3. Dependencies
----------------------
The TURBU project uses the JEDI-SDL header library and a handful of other libraries and components.  Check out everything in the Classes and Components folders.  The Turbu_components package should be installed into your IDE, and the following folders added to Delphi's search path:

\components\TURBU
\classes\FastMM4
\classes\findfile
\clases\sdl custom
\classes\jedi-sdl\SDL\Pas
\classes\jedi-sdl\SDL_image\Pas
\classes\jedi-sdl\SDL_mixer\Pas

4. JVCL
----------------------
The TURBU project requires the JEDI VCL.  It's a fairly large download and gets updated often enough that I'm not even going to bother trying to mirror it here.  You can download it at http://sourceforge.net/project/showfiles.php?group_id=45786 and install it.

5. Build
----------------------
All ready!  You can open the Turbu.groupproj file found in the /trunk/turbu folder.  It contains several projects.  The BPLs are plugins that the editor needs to run.  Turbu.exe is the main editor.  Building it will automatically build the plugins first.  Testing.exe is a test harness for easier debugging of the editor. Mapshow.exe is the remains of the old Map Viewer.  It currently does not compile, and wouldn't run properly if it did.

The first thing you want to do is build Turbu.exe from within the groupproj.  It will build the dependencies properly, then the editor.

The testing.exe project is designed for easy debugging, so it avoids dependencies on the engine packages by statically linking them into the test framework.  It also doesn't have an "open project" dialog box.  If you run it, you have to use the "Set Default Project" option in the File menu to set up an RPG Maker 2000 or 2003 project to convert.  Then use "test conversion" to import the project to TURBU format. The testing framework will perform all its tests on this converted project, unless you set a new default project.


Have fun with the code, and good luck in your efforts to contribute to the project!

Mason Wheeler
Head developer, Project TURBU