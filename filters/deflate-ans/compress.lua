-- DEFLATE-ANS: A variant of DEFLATE that uses asymmetrical numeral systems
-- instead of Huffman coding, increasing decompression speed with similar
-- compression ratios.
--
-- The block format matches RFC 1951 DEFLATE, with some minor adjustments:
-- * Bit fields are always stored most significant bit first, and bytes are
--   stored most significant byte first. This is for convenience in the bit
--   decoder.
-- * Huffman-encoded blocks are replaced with tANS-encoded blocks. These blocks
--   start with an initial X value (which is R bits long), followed by the codes
--   for each subsequent symbol. The symbols are encoded in reverse order by the
--   encoder, so that the decoder can decode each symbol in sequence. The first
--   code, which brings X from the final symbol to the origin, is omitted.
-- * Because of ANS's statefulness, LZ codes are stored slightly differently in
--   the stream. The symbol and distance code streams are interleaved, with the
--   initial X of the symbol stream being followed immediately by the initial X
--   of the distance code stream. However, the distance code stream is only
--   advanced when a length code is encountered in the symbol stream. Any code
--   for a length code is immediately followed by the extra bits for the length
--   and distance codes, and then the code for the next distance code. The
--   current state of the distance stream should be used, instead of reading a
--   new code before interpreting. Since encoding is reversed, the encoder would
--   encode the next symbol code first, then the next distance code, then the
--   current distance extra bits, then the current length extra bits.
-- * Dictionary code lengths are now encoded as exponents instead of direct code
--   lengths. This is essentially the inverse of normal (less likely symbols
--   have lower lengths). This allows quicker computation of the R and L
--   parameters, which are not encoded directly. Codes of 0 continue to indicate
--   an unused slot, with code length 1 indicating 2^0 (1), 2 = 2^1 (2), etc.
--   These numbers are then set into the Ls array directly.
-- * Dictionaries are stored as a single ANS block with one initial X. The
--   contents of the block remain the same as RFC 1951 - this is just to adapt
--   the statefulness of ANS to the stateless Huffman codes without extra data.
--
-- Here is an example of a DEFLATE-ANS block:
--
-- [1|final: 1]
-- [2|type: 1 (static ANS)]
-- (for static blocks, symbolR = 9 and distanceR = 5; therefore symbolL = 512 and distanceL = 32)
-- [9|initial symbol X: 'H']
-- [5|initial distance X: 4]
-- [8|symbol code: 'e']
-- [8|symbol code: 'l']
-- [8|symbol code: 'l']
-- [8|symbol code: 'o']
-- [8|symbol code: ' ']
-- [8|symbol code: 259 (len=5)]
-- (no length bits)
-- [1|distance bits: 1 (dist=6)]
-- [5|distance code: (undefined)]
-- [7|symbol code: 256 (end)]
--
-- Licensed under the zlib license.
--
-- Copyright (C) 2023 JackMacWindows
--
-- This software is provided 'as-is', without any express or implied
-- warranty.  In no event will the authors be held liable for any damages
-- arising from the use of this software.
--
-- Permission is granted to anyone to use this software for any purpose,
-- including commercial applications, and to alter it and redistribute it
-- freely, subject to the following restrictions:
--
-- 1. The origin of this software must not be misrepresented; you must not
--    claim that you wrote the original software. If you use this software
--    in a product, an acknowledgment in the product documentation would be
--    appreciated but is not required.
-- 2. Altered source versions must be plainly marked as such, and must not be
--    misrepresented as being the original software.
-- 3. This notice may not be removed or altered from any source distribution.

local bit32_band, bit32_rshift, bit32_lshift, bit32_bor, math_frexp, math_max, math_min, math_floor, table_sort, table_concat, string_char = bit32.band, bit32.rshift, bit32.lshift, bit32.bor, math.frexp, math.max, math.min, math.floor, table.sort, table.concat, string.char
local function log2(n) local _, r = math_frexp(n) return r-1 end
local function code(n) return n == 0 and 0 or 2^(n-1) end
local function print(...) end local function write(...) end

local staticLs, staticDistLs, clOrd = {R = 9}, {R = 5}, {16, 17, 18}
for i = 0, 143 do staticLs[i+1] = {i, 2} end
for i = 144, 255 do staticLs[i+1] = {i, 1} end
for i = 256, 279 do staticLs[i+1] = {i, 4} end
for i = 280, 287 do staticLs[i+1] = {i, 2} end
for i = 0, 31 do staticDistLs[i+1] = {i, 1} end
for i = 0, 7 do clOrd[#clOrd+1], clOrd[#clOrd+2] = (8 - i) % 8, 8 + i end

-- ENCODE/DEFLATE

local function bitstream()
    return setmetatable({data = "", partial = 0, len = 0}, {__call = function(self, bits, len)
        if not bits then bits, len = 0, 8 - self.len end
        --print(bits, len)
        assert(bits < 2^len, debug.traceback())
        self.partial = bit32.bor(bit32.lshift(self.partial, len), bits)
        self.len = self.len + len
        while self.len >= 8 do
            local byte = bit32.extract(self.partial, self.len - 8, 8)
            self.data = self.data .. string.char(byte)
            self.len = self.len - 8
        end
    end})
end

local function encoder(Ls, symbols)
    local R = Ls.R
    if R == 0 then
        if symbols then return 0 end
        while coroutine.yield(0, 0) do end
        return 0, 0
    end
    local L = 2^R
    -- prepare encoding
    local k, nb, start, next, symbol, order = {}, {}, {}, {}, {}, {}
    local X, step = 0, L < 16 and 3 or 0.625 * L + 3
    local sumLs = 0
    for _, p in ipairs(Ls) do
        local s, v = p[1], p[2]
        k[s] = R - log2(v)
        nb[s] = bit32_lshift(k[s], R+1) - bit32_lshift(v, k[s])
        start[s] = sumLs - v
        next[s] = v
        order[#order+1] = s
        for _ = 1, v do
            while symbol[X] ~= nil do X = (X + 1) % L end
            symbol[X] = s
            X = (X + step) % L
        end
        sumLs = sumLs + v
    end
    -- create encoding table
    local encodingTable = {}
    for x = L, 2*L - 1 do
        local s = symbol[x - L]
        encodingTable[start[s] + next[s]] = x
        next[s] = next[s] + 1
    end
    -- encode symbols
    local x = L
    local bitcount, nproc = 0, 0
    local f
    if symbols then
        local pos = #symbols
        function f()
            local s = symbols[pos]
            pos = pos - 1
            return s
        end
    else f = coroutine.yield end
    local s = f()
    while s do
        local nbBits = bit32_rshift(x + nb[s], R + 1)
        local nexts = f(bit32_band(x, 2^nbBits-1), nbBits)
        bitcount = bitcount + nbBits
        nproc = nproc + 1
        x = encodingTable[start[s] + bit32_rshift(x, nbBits)]
        s = nexts
    end
    -- write out
    if not symbols then
        print("Size of block:", bitcount + R, nproc)
        return x - L, R
    else
        return bitcount + R
    end
end

local function coroutine_resume(coro, ...)
    local res = table.pack(coroutine.resume(coro, ...))
    if not res[1] then error(debug.traceback(coro, res[2]), 2) end
    return table.unpack(res, 1, res.n)
end

local function encodeLZ(symbols, Ls, distLs, out)
    local bitbuf = {}
    local symcoder = coroutine.create(encoder)
    coroutine_resume(symcoder, Ls)
    local lzcoder = coroutine.create(encoder)
    coroutine_resume(lzcoder, distLs)
    for i = #symbols, 1, -1 do
        local s = symbols[i]
        if type(s) == "table" then
            bitbuf[#bitbuf+1] = {select(2, coroutine_resume(symcoder, s[1].code + 257))}
            bitbuf[#bitbuf+1] = {select(2, coroutine_resume(lzcoder, s[2].code))}
            bitbuf[#bitbuf+1] = {s[2].extra, s[2].bits}
            bitbuf[#bitbuf+1] = {s[1].extra, s[1].bits}
        else
            bitbuf[#bitbuf+1] = {select(2, coroutine_resume(symcoder, s))}
        end
    end
    bitbuf[#bitbuf+1] = {select(2, coroutine_resume(lzcoder))}
    bitbuf[#bitbuf+1] = {select(2, coroutine_resume(symcoder))}
    for i = #bitbuf, 2, -1 do out(bitbuf[i][1], bitbuf[i][2]) end
end

local function sizeLZ(symbols, Ls, distLs)
    if not Ls or not distLs then return math.huge end
    local newsym, distsym = {}, {}
    local bits = 0
    for i, s in ipairs(symbols) do
        if type(s) == "table" then
            newsym[i] = s[1].code + 257
            distsym[#distsym+1] = s[2].code
            bits = bits + s[1].bits + s[2].bits
        else
            newsym[i] = s
        end
    end
    return bits + encoder(Ls, newsym) + encoder(distLs, distsym)
end

local function encodeDict(symbols, Ls, out)
    local bitbuf = {}
    local symcoder = coroutine.create(encoder)
    coroutine.resume(symcoder, Ls)
    for i = #symbols, 1, -1 do
        local s = symbols[i]
        if type(s) == "table" then
            bitbuf[#bitbuf+1] = {select(2, coroutine.resume(symcoder, s[1]))}
            if s[1] == 16 then bitbuf[#bitbuf+1] = {s[2], 2}
            elseif s[1] == 17 then bitbuf[#bitbuf+1] = {s[2], 3}
            elseif s[1] == 18 then bitbuf[#bitbuf+1] = {s[2], 7} end
        else
            bitbuf[#bitbuf+1] = {select(2, coroutine.resume(symcoder, s))}
        end
    end
    bitbuf[#bitbuf+1] = {select(2, coroutine.resume(symcoder))}
    for i = #bitbuf, 1, -1 do out(bitbuf[i][1], bitbuf[i][2]) end
end

local function sizeDict(symbols, Ls)
    if not Ls then return math.huge end
    local newsym = {}
    local bits = 0
    for i, s in ipairs(symbols) do
        if type(s) == "table" then
            newsym[i] = s[1]
            if s[1] == 16 then bits = bits + 2
            elseif s[1] == 17 then bits = bits + 3
            elseif s[1] == 18 then bits = bits + 7 end
        else
            newsym[i] = s
        end
    end
    return bits + encoder(Ls, newsym)
end

local function distcode(i)
    if i == 0 or i == 1 then return {code = i, extra = 0, bits = 0, raw = i} end
    local ebits = math.max(select(2, math.frexp(i)) - 2, 0)
    local mask = 2^ebits
    return {code = ebits * 2 + (bit32.btest(i, mask) and 3 or 2), extra = bit32.band(i, mask-1), bits = ebits, raw = i}
end

local function lencode(i)
    if i >= 0 and i < 4 then return {code = i, extra = 0, bits = 0, raw = i} end
    local ebits = math.max(select(2, math.frexp(i)) - 3, 0)
    local mask = 2^ebits
    return {code = ebits * 4 + (bit32.btest(i, mask) and 5 or 4) + (bit32.btest(i, mask*2) and 2 or 0), extra = bit32.band(i, mask-1), bits = ebits, raw = i}
end

local function lz77(tokens, maxdist)
    maxdist = math_min(maxdist or 1024, 32768)
    local retval = {}
    local lookback = {}
    local i = 1
    while i <= #tokens do
        local v = tokens[i]
        if lookback[v] then
            local lblist = lookback[v]
            local max, pos = 0
            for n = #lblist, 1, -1 do
                local l = lblist[n]
                if i - l > maxdist then break end
                for j = 1, math_min(#tokens - i, 129) do
                    local lj = (j - 1) % (i - l) + 1
                    if tokens[i+j] == tokens[l+lj] then
                        if j > max then max, pos = j, l end
                    else break end
                end
            end
            if max >= 2 then
                local len = lencode(max - 2)
                local dist = distcode(i - pos - 1)
                retval[#retval+1] = {len, dist}
                for j = 0, max do
                    v = tokens[i+j]
                    lookback[v][#lookback[v]+1] = i+j
                end
                i = i + max + 1
                v = nil
            end
        end
        if v then
            retval[#retval+1] = v
            lookback[v] = lookback[v] or {}
            lookback[v][#lookback[v]+1] = i
            i = i + 1
        end
    end
    return retval
end

local function leafcomp(h) return function(a, b)
    if a.height > h then return true end
    if b.height > h then return false end
    return a.weight > b.weight
end end
local function sortcodes(symbolMap)
    if symbolMap then return function(a, b) if a.bits == b.bits then return symbolMap[a.symbol] < symbolMap[b.symbol] else return a.bits < b.bits end end
    else return function(a, b) if a.bits == b.bits then return a.symbol < b.symbol else return a.bits < b.bits end end end
end

local function loadcodes(node, codes, map, partial)
    if node.data then
        partial.symbol = node.data
        map[node.data] = partial
        codes[#codes+1] = partial
    else
        loadcodes(node[1], codes, map, {bits = partial.bits + 1, code = partial.code * 2})
        loadcodes(node[2], codes, map, {bits = partial.bits + 1, code = partial.code * 2 + 1})
    end
end

local function makeLs(freq, maxHeight)
    maxHeight = maxHeight or 15
    -- make initial Ls
    local Ls = {R = 0}
    for k, v in pairs(freq) do Ls[#Ls+1] = {k, v} end
    table_sort(Ls, function(a, b) return a[1] < b[1] end)
    -- make Huffman tree
    local queue = {}
    for i, v in ipairs(Ls) do if v[2] > 0 then queue[#queue+1] = {data = v[1], weight = v[2], height = 1} end end
    if #queue < 2 then
        local retval = {R = 0}
        for i, v in ipairs(Ls) do if v[1] == queue[1].data then retval[i] = {v[1], 1, 0} end end
        return retval
    end
    local comp = leafcomp(maxHeight - 1)
    table_sort(queue, comp)
    while #queue > 1 do
        local a, b = queue[#queue-1], queue[#queue]
        local n = {weight = a.weight + b.weight, height = math_max(a.height + 1, b.height + 1), a, b}
        queue[#queue] = nil
        queue[#queue] = n
        table_sort(queue, comp)
    end
    -- make canonical codebook
    local codes, map = {}, {}
    loadcodes(queue[1], codes, map, {bits = 0, code = 0})
    table_sort(codes, sortcodes())
    codes[1].code = 0
    for i = 2, #codes do codes[i].code = bit32.lshift(codes[i-1].code + 1, codes[i].bits - codes[i-1].bits) end
    local lengths = {}
    for i, v in ipairs(Ls) do
        lengths[i] = map[v[1]] and map[v[1]].bits or 0
        Ls.R = math_max(Ls.R, lengths[i])
    end
    -- make Huffman Ls
    local LsH = {}
    local huffSum = 0
    for i, v in ipairs(lengths) do assert(v > 0 and v <= maxHeight) LsH[i] = {Ls[i][1], 2^(Ls.R-v), Ls.R-v} huffSum = huffSum + 2^(Ls.R-v) end
    LsH.R = select(2, math.frexp(huffSum))-1
    return LsH
end

local function deflate(data, level, forceStatic)
    if data == "" then return "\x80\x00\x00\xFF\xFF" end
    level = level or 5
    local out = bitstream()
    for i = 1, #data, 65536 do
        print(#out.data, out.len)
        local final = i + 65536 > #data
        local chunk = {}
        for j = 0, math_min(#data - i, 65535) do
            chunk[j+1] = data:byte(i+j)
        end
        chunk[#chunk+1] = 256
        local lzchunk = level == 0 and chunk or lz77(chunk, 2^(level + 6))
        -- extract probabilities
        local freq, distfreq = {}, {}
        for _, v in ipairs(lzchunk) do
            if type(v) == "table" then
                local s = v[1].code + 257
                freq[s] = (freq[s] or 0) + 1
                distfreq[v[2].code] = (distfreq[v[2].code] or 0) + 1
            else
                freq[v] = (freq[v] or 0) + 1
            end
        end
        local dynamicLs = makeLs(freq)
        local dynamicDistLs = makeLs(distfreq)
        -- create dynamic distance codes
        local lengths = {}
        for j = 0, 285 do
            local found = false
            for _, v in ipairs(dynamicLs) do
                if v[1] == j then
                    found = true
                    lengths[j] = v[3] + 1
                    break
                end
            end
            if not found then lengths[j] = 0 end
        end
        while #lengths > 256 and lengths[#lengths] == 0 do lengths[#lengths] = nil end
        local hLit = #lengths - 256
        for j = 0, 31 do
            local found = false
            for _, v in ipairs(dynamicDistLs) do
                if v[1] == j then
                    found = true
                    lengths[#lengths+1] = v[3] + 1
                    break
                end
            end
            if not found then lengths[#lengths+1] = 0 end
        end
        while #lengths > 257 and lengths[#lengths] == 0 do lengths[#lengths] = nil end
        local hDist = #lengths - hLit - 257
        local lencodes = {}
        do
            local j = 0
            while j <= #lengths do
                if lengths[j] == lengths[j+1] and lengths[j+1] == lengths[j+2] then
                    if lengths[j] == 0 then
                        local len = 3
                        while lengths[j+len] == 0 and len < 138 do len = len + 1 end
                        if len >= 11 then lencodes[#lencodes+1] = {18, len - 11}
                        else lencodes[#lencodes+1] = {17, len - 3} end
                        j = j + len
                    elseif lengths[j+2] == lengths[j+3] then
                        local len = 4
                        while lengths[j+len] == lengths[j] and len < 7 do len = len + 1 end
                        lencodes[#lencodes+1] = lengths[j]
                        lencodes[#lencodes+1] = {16, len - 4}
                        j = j + len
                    else
                        lencodes[#lencodes+1] = lengths[j]
                        lencodes[#lencodes+1] = lengths[j]
                        lencodes[#lencodes+1] = lengths[j]
                        j = j + 3
                    end
                else
                    lencodes[#lencodes+1] = lengths[j]
                    j = j + 1
                end
            end
        end
        freq = {}
        for _, v in ipairs(lencodes) do
            if type(v) == "table" then freq[v[1]] = (freq[v[1]] or 0) + 1
            else freq[v] = (freq[v] or 0) + 1 end
        end
        local lencodeLs = makeLs(freq, 7)
        local lencodelen = {}
        for j, v in ipairs(clOrd) do
            local l = 0
            for _, w in ipairs(lencodeLs) do
                if w[1] == v then
                    l = w[3] + 1
                    break
                end
            end
            write(l .. " ")
            lencodelen[j] = l
        end
        print()
        while #lencodelen > 4 and lencodelen[#lencodelen] == 0 do lencodelen[#lencodelen] = nil end
        -- get sizes for each type
        local uncompSize = #chunk * 8 + 8
        local lzssSize = #lzchunk * 9
        for _, v in ipairs(lzchunk) do if type(v) == "table" then lzssSize = lzssSize + 15 end end
        local staticSize = sizeLZ(lzchunk, staticLs, staticDistLs)
        local dynamicSize = forceStatic and math.huge or (sizeLZ(lzchunk, dynamicLs, dynamicDistLs) + sizeDict(lencodes, lencodeLs) + #lencodelen * 3)
        local minSize = math_min(uncompSize, lzssSize, staticSize, dynamicSize)
        print("Sizes:", uncompSize, lzssSize, staticSize, dynamicSize)
        -- encode
        out(final and 1 or 0, 1)
        if minSize == uncompSize then
            print("Writing uncompressed block", uncompSize)
            out(0, 2)
            out()
            out(#chunk - 1, 16)
            out(bit32.band(bit32.bnot(#chunk - 1), 0xFFFF), 16)
            out.data = out.data .. data:sub(i, i + 65535)
        elseif minSize == lzssSize then
            print("Writing LZSS block", lzssSize)
            out(3, 2)
            for _, s in ipairs(lzchunk) do
                if type(s) == "table" then
                    out(s[1].raw + 256, 9)
                    out(s[2].raw, 15)
                elseif s == 256 then
                    out(511, 9)
                else
                    out(s, 9)
                end
            end
        else -- ANS
            local Ls, distLs
            if minSize == staticSize then
                print("Writing static block", staticSize)
                out(1, 2)
                Ls, distLs = staticLs, staticDistLs
            else
                print("Writing dynamic block", dynamicSize)
                out(2, 2)
                Ls, distLs = dynamicLs, dynamicDistLs
                out(hLit, 5)
                out(hDist, 5)
                out(#lencodelen - 4, 4)
                for _, v in ipairs(lencodelen) do out(v, 3) end
                encodeDict(lencodes, lencodeLs, out)
            end
            encodeLZ(lzchunk, Ls, distLs, out)
        end
    end
    out()
    return out.data
end

return deflate
