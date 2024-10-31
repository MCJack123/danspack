local LibDeflate = require "filters.deflate.LibDeflate"

return function(data, level) return LibDeflate:CompressDeflate(data, {level = level}) end