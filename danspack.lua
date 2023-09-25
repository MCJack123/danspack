local dans = require "deflate-ans"
local function pack(path)
    if fs.isDir(path) then
        local t = {}
        for _, k in ipairs(fs.list(path)) do t[k] = pack(fs.combine(path, k)) end
        return t
    else
        local file = assert(fs.open(path, "rb"))
        local data = file.readAll()
        file.close()
        return data
    end
end
local args = {...}
if #args < 2 then error("Usage: danspack <directory> <output.lua>") end
assert(fs.exists(shell.resolve(fs.combine(args[1], "init.lua"))), "Directory is missing init.lua")
local file = assert(fs.open(fs.combine(fs.getDir(shell.getRunningProgram()), "loader.min.lua"), "rb"))
local data = file.readAll()
file.close()
local orig = textutils.serialize(pack(shell.resolve(args[1])), {compact = true}):gsub("\\\n", "\\n")
local cmp = dans.deflate(orig, 9)
file = assert(fs.open(shell.resolve(args[2]), "wb"))
file.write("local disk,size='" .. cmp:gsub("\\", "\\\\"):gsub("\n", "\\n"):gsub("\r", "\\r"):gsub("%z", "\\000"):gsub("'", "\\'") .. "'," .. #cmp .. " ")
file.write(data)
file.close()