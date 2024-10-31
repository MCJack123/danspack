local args = {...}
if #args < 2 then error("Usage: danspack <directory> <output.lua>") end
assert(fs.exists(fs.combine(shell.resolve(args[1]), "init.lua")), "Directory is missing init.lua")
local file = assert(fs.open(fs.combine(fs.getDir(shell.getRunningProgram()), "loader.min.lua"), "rb"))
local loader = file.readAll()
file.close()
file = fs.open(fs.combine(args[1], ".index"), "r")
local index = {}
if file then
    for line in file.readLine do
        local pat, filter = line:match '"([^"]*)"%s+(%S+)'
        if pat then index[#index+1] = {pattern = pat, filter = filter} end
    end
    file.close()
else
    index = {
        {pattern = ".*", filter = "deflate"},
        {pattern = ".+%.lua", filter = "luz"}
    }
end
local filters = "term.setTextColor(colors.lime)term.setBackgroundColor(colors.black)term.setCursorPos(1,1)term.clear()term.write('PRE-LOAD INIT...')local filters={"
local added_filters = {}
local function pack(path)
    if fs.isDir(path) then
        local t = {}
        for _, k in ipairs(fs.list(path)) do if k ~= ".index" then t[k] = pack(fs.combine(path, k)) end end
        return t
    else
        local file = assert(fs.open(path, "rb"))
        local data = file.readAll()
        file.close()
        local filter
        for _, v in ipairs(index) do
            if path:match(v.pattern) then filter = v.filter end
        end
        if not filter then error("No filter defined for " .. path) end
        if filter == "none" then return data end
        local compress = require("filters." .. filter .. ".compress")
        if not added_filters[filter] then
            file = assert(fs.open(shell.resolve("filters/" .. filter .. "/decompress.lua"), "r"))
            local data = file.readAll()
            file.close()
            filters = filters .. '["' .. filter .. '"]=(function()' .. data .. ' end)(),'
            added_filters[filter] = true
        end
        return "[filter:" .. filter .. "]" .. compress(data, 9)
    end
end
local orig = textutils.serialize(pack(shell.resolve(args[1])), {compact = true}):gsub("\\\n", "\\n"):gsub('"%[filter:([^%]]+)%]', 'filters["%1"]"')
file = assert(fs.open(shell.resolve(args[2]), "wb"))
file.write(filters .. "}")
file.write("local disk=" .. orig .. "\n")
file.write(loader)
file.close()