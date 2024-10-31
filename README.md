# danspack
Packs a program and virtual filesystem into an executable Lua file, with multiple options for compression. The virtual filesystem is overlaid on top of the real filesystem while the program runs, allowing the files to remain in memory while still allowing access to the user's files. This program is used for the [Herobrine](https://github.com/MCJack123/Herobrine) and [Astronaut](https://github.com/MCJack123/Astronaut) self-contained executables.

## Usage
1. Create a folder for the program, which will become the virtual root filesystem.
2. Place the main program at `/init.lua` in the folder.
3. Place any required supporting files in the folder as well, such as libraries or other resources. (Tip: Libraries placed at `/rom/modules/main` in the folder will be available from `require` without modifying `package.path`.)
4. Run `danspack <folder> <output.lua>` to generate the program.

## Compression Options
danspack comes with the following compression filters:
- `none`: No compression
- `deflate`: RFC 1951 DEFLATE, provided by [LibDeflate](https://github.com/SafeteeWoW/LibDeflate)
- `deflate-ans`: DEFLATE-ANS, a variant of DEFLATE that uses asymmetrical numeral systems for faster decompression (experimental)
- `luz`: The [Luz](https://github.com/MCJack123/Luz) Lua compression algorithm

The default configuration will compress Lua files with Luz, and all other files with DEFLATE. You can add your own configuration with a file called `.index` at the root of the folder - its syntax is a Lua (not glob!) pattern for the file path in double quotes, followed by the filter to use. Entries are processed in order, so entries lower take precedence over higher ones. The default config looks like this:

```
".*" deflate
".*%.lua" luz
```

## Notes
- Programs are run in the root directory, which may break relative paths. Make sure to use absolute paths anywhere you need to load files.
- The filesystem overlay is very limited. Writing to virtual files is not supported, and some operations that write to the filesystem may not even attempt to access the virtual filesystem. In addition, virtual files will always override real files.
- Since it overwrites the global filesystem, programs running in other multishell tabs may experience issues. Avoid running programs made with danspack alongside other programs.
