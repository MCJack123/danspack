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
        if n <= 0 then return 0 end
        while bits < n do assert(pos <= #str) pos, bits, partial = pos + 1, bits + 8, bit32_lshift(partial, 8) + str:byte(pos) end
        local retval = bit32_band(bit32_rshift(partial, bits-n), 2^n-1)
        bits = bits - n
        return retval
    end
    return readbits
end

local function generateDecodeTable(Ls)
    local R, L = Ls.R, 2^Ls.R
    local X, step, decodingTable, next, symbol = 0, L < 16 and 3 or 0.625 * L + 3, {R = R}, {}, {}
    if R < 0 then return decodingTable end
    for i = 1, #Ls do
        local p = Ls[i]
        next[p[1]] = p[2]
        for _ = 1, p[2] do while symbol[X] do X = (X + 1) % L end X, symbol[X] = (X + step) % L, p[1] end
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
    print("inflate dans", #data)
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
        elseif typ == 3 then
            -- LZSS
            local chars = {}
            while true do
                local len = reader(9)
                if len == 511 then break
                elseif len >= 256 then
                    len = len - 253
                    local dist = reader(15)
                    for _ = 1, len do chars[#chars+1] = chars[#chars - dist] end
                else
                    chars[#chars+1] = string.char(len)
                end
            end
            retval = retval .. table.concat(chars)
        else error("invalid chunk") end
    until final
    return retval
end

return inflate
