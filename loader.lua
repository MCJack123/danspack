if "ä" ~= "\xE4" then
    local file = assert(fs.open(shell.getRunningProgram(), "rb"))
    local data = file.readAll()
    file.close()
    return assert(load(data, "@" .. shell.getRunningProgram(), nil, _ENV))()
end
if #disk ~= size then error("File corrupted (expected " .. size .. ", got " .. #disk .. ")") end
local expect = require "cc.expect"

local bit32_band, bit32_rshift, bit32_lshift, bit32_bor, math_frexp, math_max, math_floor, table_sort, table_concat, string_char = bit32.band, bit32.rshift, bit32.lshift, bit32.bor, math.frexp, math.max, math.floor, table.sort, table.concat, string.char
local function log2(n) local _, r = math_frexp(n) return r-1 end
local function code(n) return n == 0 and 0 or 2^(n-1) end

local staticLs, staticDistLs, clOrd = {R = 9}, {R = 5}, {16, 17, 18}
for i = 0, 143 do staticLs[i+1] = {i, 2} end
for i = 144, 255 do staticLs[i+1] = {i, 1} end
for i = 256, 279 do staticLs[i+1] = {i, 4} end
for i = 280, 287 do staticLs[i+1] = {i, 2} end
for i = 0, 31 do staticDistLs[i+1] = {i, 1} end
for i = 0, 7 do clOrd[#clOrd+1], clOrd[#clOrd+2] = (8 - i) % 8, 8 + i end

-- DECODE/INFLATE

local function makeReader(str)
    local partial, bits, pos = 0, 0, 1
    local function readbits(n)
        if not n then n = bits % 8 end
        if n == 0 then return 0 end
        while bits < n do pos, bits, partial = pos + 1, bits + 8, bit32_lshift(partial, 8) + str:byte(pos) end
        local retval = bit32_band(bit32_rshift(partial, bits-n), 2^n-1)
        bits = bits - n
        return retval
    end
    return readbits
end

local function generateDecodeTable(Ls)
    local R, L = Ls.R, 2^Ls.R
    local X, step, decodingTable, next, symbol = 0, 0.625 * L + 3, {R = R}, {}, {}
    for i = 1, #Ls do
        local p = Ls[i]
        next[p[1]] = p[2]
        for _ = 1, p[2] do X, symbol[X] = (X + step) % L, p[1] end
    end
    for X = 0, L - 1 do
        local s = symbol[X]
        local t = {s = s, n = R - log2(next[s])}
        t.X, decodingTable[X], next[s] = bit32_lshift(next[s], t.n) - L, t, 1 + next[s]
    end
    return decodingTable
end

local function ansdecode(readbits, nsym, decodingTable, distDecodingTable)
    local X, XL, i, retval = readbits(decodingTable.R), distDecodingTable and readbits(distDecodingTable.R), 1, {}
    while i <= nsym do
        local t = decodingTable[X]
        local s = t.s
        if s == 256 then return retval
        elseif s > 256 then
            local lencode = s - 257
            local ebits = math_max(math_floor(lencode / 4) - 1, 0)
            if ebits > 0 then lencode = 2 + bit32_bor(readbits(ebits), bit32_lshift(bit32_band(lencode, 3) + 4, ebits))
            else lencode = lencode + 2 end
            local tL = distDecodingTable[XL]
            local distcode = tL.s
            ebits = math_max(math_floor(distcode / 2) - 1, 0)
            if ebits > 0 then distcode = 1 + bit32_bor(readbits(ebits), bit32_lshift(bit32_band(distcode, 1) + 2, ebits))
            else distcode = distcode + 1 end
            for j = 0, lencode do retval[i+j] = retval[i+j-distcode] end
            i, XL = i + lencode + 1, tL.X + readbits(tL.n)
        elseif not distDecodingTable and s > 15 then
            local len
            if s == 16 then s, len = retval[i-1], 3 + readbits(2)
            elseif s == 17 then s, len = 0, 3 + readbits(3)
            else s, len = 0, 11 + readbits(7) end
            for j = 0, len-1 do retval[i+j] = s end
            i = i + len
        else
            retval[i], i = s, i + 1
        end
        X = t.X + readbits(t.n)
    end
    return retval
end

local function inflate(data)
    local reader, retval = makeReader(data), ""
    repeat
        local final, typ = reader(1) == 1, reader(2)
        if typ == 0 then
            -- uncompressed
            reader()
            local size = reader(16)
            assert(bit32.bxor(reader(16), size) == 0xFFFF, "invalid chunk")
            for _ = 1, size do retval = retval .. string_char(reader(8)) end
        elseif typ == 1 or typ == 2 then
            -- ANS
            local Ls, distLs
            if typ == 1 then
                Ls, distLs = staticLs, staticDistLs
            else
                local hLit, hDist, hcLen, clLs = reader(5) + 257, reader(5) + 1, reader(4) + 4, {R = 0}
                for i = 1, hcLen do
                    local c = code(reader(3))
                    clLs.R, clLs[i] = clLs.R + c, {clOrd[i], c}
                end
                clLs.R = log2(clLs.R)
                table_sort(clLs, function(a, b) return a[1] < b[1] end)
                local trees = ansdecode(reader, hLit + hDist, generateDecodeTable(clLs))
                Ls, distLs = {R = 0}, {R = 0}
                for i = 1, hLit do Ls[i] = {i - 1, code(trees[i])} end
                for i = 1, hDist do distLs[i] = {i - 1, code(trees[i+hLit])} end
                for i = 1, hLit do Ls.R = Ls.R + Ls[i][2] end
                for i = 1, hDist do distLs.R = distLs.R + distLs[i][2] end
                Ls.R, distLs.R = log2(Ls.R), log2(distLs.R)
            end
            local sym = ansdecode(reader, 1e10, generateDecodeTable(Ls), generateDecodeTable(distLs))
            for i = 1, #sym do sym[i] = string_char(sym[i]) end
            retval = retval .. table_concat(sym)
        else error("invalid chunk") end
    until final
    return retval
end

local function getPath(tab, path)
    for p in fs.combine(path):gmatch("[^/]+") do
        tab = tab[p]
        if tab == nil then return nil end
    end
    return tab
end

local function aux_find(parts, t)
    if #parts == 0 then return type(t) == "table" and "" or t elseif type(t) ~= "table" then return nil end
    local parts2 = {}
    for i,v in ipairs(parts) do parts2[i] = v end
    local name = table.remove(parts2, 1)
    local retval = {}
    if t then for k, v in pairs(t) do if k:match("^" .. name:gsub("([%%%.])", "%%%1"):gsub("%*", "%.%*") .. "$") then retval[k] = aux_find(parts2, v) end end end
    return retval
end

local function combineKeys(t, prefix)
    prefix = prefix or ""
    if t == nil then return {} end
    local retval = {}
    for k,v in pairs(t) do
        if type(v) == "string" then table.insert(retval, prefix .. k)
        else for _,w in ipairs(combineKeys(v, prefix .. k .. "/")) do table.insert(retval, w) end end
    end
    return retval
end

local data = inflate(disk)
disk = textutils.unserialize(data)
sleep(0)

local fs = fs
_G.fs = {
    list = function(path)
        expect(1, path, "string")
        local tab = getPath(disk, path)
        if type(tab) ~= "table" then return fs.list(path) end
        local retval = {}
        for k in pairs(tab) do retval[#retval+1] = k end
        local parts = {}
        for p in fs.combine(path):gmatch("[^/]+") do parts[#parts+1] = p end
        table.sort(retval)
        return retval
    end,
    exists = function(path)
        expect(1, path, "string")
        return getPath(disk, path) ~= nil or fs.exists(path)
    end,
    isDir = function(path)
        expect(1, path, "string")
        local tab = getPath(disk, path)
        return type(tab) == "table" or fs.isDir(path)
    end,
    isReadOnly = function(path)
        expect(1, path, "string")
        return getPath(disk, path) ~= nil or fs.isReadOnly(path)
    end,
    getName = fs.getName,
    getDrive = function(path)
        expect(1, path, "string")
        if getPath(disk, path) ~= nil then return "mem" end
        return fs.getDrive(path)
    end,
    getSize = function(path)
        expect(1, path, "string")
        local tab = getPath(disk, path)
        if tab == nil then return fs.getSize(path) end
        if type(tab) == "table" then return 0
        else return #tab end
    end,
    getFreeSpace = fs.getFreeSpace,
    makeDir = fs.makeDir,
    move = fs.move,
    copy = fs.copy,
    delete = fs.delete,
    combine = fs.combine,
    open = function(path, mode)
        expect(1, path, "string")
        expect(2, mode, "string")
        local tab = getPath(disk, path)
        if tab == nil then return fs.open(path, mode) end
        if mode == "r" then
            if type(tab) ~= "string" then return nil, "Is a directory" end
            local oldtab = tab
            tab = ""
            for _, c in utf8.codes(oldtab) do tab = tab .. (c > 255 and "?" or string.char(c)) end
            tab = tab:gsub("\r\n", "\n")
            local pos = 1
            local closed = false
            return {
                readLine = function(withTrailing)
                    if closed then error("file is already closed", 2) end
                    if pos > #tab then return end
                    local str, endPos = tab:match(withTrailing and "([^\n]*\n?)()" or "([^\n]*)\n?()", pos)
                    pos = str and endPos or #tab + 1
                    return str
                end,
                readAll = function()
                    if closed then error("file is already closed", 2) end
                    if #tab == 0 and pos == 1 then
                        pos = 2
                        return ""
                    end
                    if pos > #tab then return end
                    local oldPos = pos
                    pos = #tab + 1
                    return tab:sub(oldPos)
                end,
                read = function(count)
                    if closed then error("file is already closed", 2) end
                    if pos > #tab then return end
                    expect(1, count, "number", "nil")
                    count = count or 1
                    local oldPos = pos
                    pos = pos + count
                    return tab:sub(oldPos, pos - 1)
                end,
                close = function()
                    if closed then error("file is already closed", 2) end
                    closed = true
                end
            }
        elseif mode == "w" or mode == "a" then
            return nil, "Permission denied"
        elseif mode == "rb" then
            if type(tab) ~= "string" then return nil, "Is a directory" end
            local pos = 1
            local closed = false
            return {
                readLine = function(withTrailing)
                    if closed then error("file is already closed", 2) end
                    if pos > #tab then return end
                    local str, endPos = tab:match(withTrailing and "([^\n]*\n?)()" or "([^\n]*)\n?()", pos)
                    pos = str and endPos or #tab + 1
                    return str
                end,
                readAll = function()
                    if closed then error("file is already closed", 2) end
                    if #tab == 0 and pos == 1 then
                        pos = 2
                        return ""
                    end
                    if pos > #tab then return end
                    local oldPos = pos
                    pos = #tab + 1
                    return tab:sub(oldPos)
                end,
                read = function(count)
                    expect(1, count, "number", "nil")
                    if closed then error("file is already closed", 2) end
                    if pos > #tab then return end
                    if count == nil then
                        pos = pos + 1
                        return tab:byte(pos - 1)
                    else
                        local oldPos = pos
                        pos = pos + count
                        return tab:sub(oldPos, pos - 1)
                    end
                end,
                close = function()
                    if closed then error("file is already closed", 2) end
                    closed = true
                end,
                seek = function(whence, offset)
                    if closed then error("file is already closed", 2) end
                    expect(1, whence, "string", "nil")
                    expect(2, offset, "number", "nil")
                    whence = whence or "cur"
                    offset = offset or 0
                    if whence == "set" then pos = offset + 1
                    elseif whence == "cur" then pos = pos + offset
                    elseif whence == "end" then pos = #tab - offset
                    else error("bad argument #1 (invalid option " .. whence .. ")", 2) end
                    return pos
                end
            }
        elseif mode == "wb" or mode == "ab" then
            return nil, "Permission denied"
        else return nil, "Invalid mode" end
    end,
    find = function(wildcard)
        expect(1, wildcard, "string")
        local parts = {}
        for p in wildcard:gmatch("[^/]+") do parts[#parts+1] = p end
        local retval = fs.find(wildcard)
        for _,v in ipairs(combineKeys(aux_find(parts, disk))) do retval[#retval+1] = v end
        table.sort(retval)
        return retval
    end,
    getDir = fs.getDir,
    attributes = function(path)
        expect(1, path, "string")
        local tab = getPath(disk, path)
        if not tab then return fs.attributes(path) end
        return {
            size = type(tab) == "table" and 0 or #tab,
            isDir = type(tab) == "table",
            isReadOnly = false,
            created = 0,
            modified = 0
        }
    end,
    getCapacity = fs.getCapacity,
    isDriveRoot = fs.isDriveRoot
}

local ok, err = pcall(shell.run, "/init.lua")
_G.fs = fs
if not ok then printError(err) end
