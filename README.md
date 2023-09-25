# danspack
Packs a program and virtual filesystem into an executable Lua file, with DEFLATE-ANS for compression. The virtual filesystem is overlaid on top of the real filesystem while the program runs, allowing the files to remain in memory while still allowing access to the user's files. This program is used for the [Herobrine](https://github.com/MCJack123/Herobrine) self-contained executable.

## Usage
1. Create a folder for the program, which will become the virtual root filesystem.
2. Place the main program at `/init.lua` in the folder.
3. Place any required supporting files in the folder as well, such as libraries or other resources. (Tip: Libraries placed at `/rom/modules/main` in the folder will be available from `require` without modifying `package.path`.)
4. Run `danspack <folder> <output.lua>` to generate the program.

## Notes
- Programs are run in the current directory, which may break relative paths. Make sure to use absolute paths anywhere you need to load files.
- The filesystem overlay is very limited. Writing to virtual files is not supported, and some operations that write to the filesystem may not even attempt to access the virtual filesystem. In addition, virtual files will always override real files.
