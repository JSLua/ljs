-- Copyright (C) 2025  `zyxwvu` Shi <i@shiyc.cn>
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, see <https://www.gnu.org/licenses/>.

local jsrt = assert(..., "aux library not loaded as a part of JSRT")

string["repeat"] = string.rep
string.toLowerCase = string.lower
string.toUpperCase = string.upper

function string:charAt(index)
    if index < 0 then return "" end
    return self:sub(index + 1, index + 1)
end

function string:charCodeAt(index)
    if index < 0 then return 0.0 / 0.0 end
    return self:byte(index + 1)
end

function string:slice(start, _end)
    if start >= 0 then start = start + 1 end
    if _end and _end < 0 then _end = _end - 1 end
    return self:sub(start, _end)
end

function string:substring(start, _end)
    if _end ~= nil and start > _end then
        start, _end = _end, start  -- Swap if start > _end
    end
    return self:sub(start + 1, _end)
end

function string:startsWith(match)
    return self:sub(1, #match) == match
end

function string:endsWith(match)
    return self:sub(-#match) == match
end

function string:includes(match, pos)
    if pos == nil or pos < 0 then pos = 0 end
    return self:find(match, pos + 1, true) ~= nil
end

function string:padStart(length)
    if #self >= length then return self end
    return string.rep(" ", length - #self) .. self
end

function string:padEnd(length)
    if #self >= length then return self end
    return self .. string.rep(" ", length - #self)
end

function string:trimStart() return self:gsub("^%s*", "") end
function string:trimEnd() return self:gsub("%s*$", "") end
string.trimLeft = string.trimStart
string.trimRight = string.trimEnd

string.matchAll = jsrt.generator(function (str, re)
    if type(re) == "string" then re = RegExp(nil, re, "g") end
    local yield, offset = coroutine.yield, 0
    while true do
        local match = re:exec(str, offset)
        if match == nil then break end
        offset = match.lastIndex
        yield(match)
    end
end)

if encodeURIComponent == nil then
    local gsub, format, byte = string.gsub, string.format, string.byte
    function encodeURIComponent(_, uriComponent)
        return gsub(uriComponent, "[^A-Za-z%d%-_.!~*'()]", function(x)
            return format("%%%02X", byte(x))
        end)
    end

    local char, tonumber = string.char, _G.tonumber
    function decodeURIComponent(_, uriComponent)
        return gsub(uriComponent, "%%(%x%x)", function(x)
            return char(tonumber(x, 16))
        end)
    end
end

if Math == nil then
    Math = {
        E = 2.718281828459045,
        LN10 = 2.302585092994046,
        LN2 = 0.6931471805599453,
        LOG10E = 0.4342944819032518,
        LOG2E = 1.4426950408889634,
        PI = 3.141592653589793,
        SQRT1_2 = 0.7071067811865476,
        SQRT2 = 1.4142135623730951,
    }

    local math = _G.math
    function Math:abs(x) return math.abs(x) end
    function Math:acos(x) return math.acos(x) end
    function Math:asin(x) return math.asin(x) end
    function Math:atan(x) return math.atan(x) end
    function Math:atan2(y, x) return math.atan2(y, x) end
    function Math:ceil(x) return math.ceil(x) end
    function Math:cos(x) return math.cos(x) end
    function Math:cosh(x) return math.cosh(x) end
    function Math:exp(x) return math.exp(x) end
    function Math:expm1(x) return math.exp(x) - 1 end
    function Math:floor(x) return math.floor(x) end
    function Math:log(x) return math.log(x) end
    function Math:log10(x) return math.log10(x) end
    function Math:log1p(x) return math.log(1 + x) end
    function Math:max(...) return math.max(...) end
    function Math:min(...) return math.min(...) end
    function Math:pow(x, y) return math.pow(x, y) end
    function Math:random() return math.random() end
    function Math:sin(x) return math.sin(x) end
    function Math:sinh(x) return math.sinh(x) end
    function Math:sqrt(x) return math.sqrt(x) end
    function Math:tan(x) return math.tan(x) end
    function Math:tanh(x) return math.tanh(x) end

    local tonumber = _G.tonumber
    function parseFloat(_, x) return tonumber(x) end

    NaN = 0.0 / 0.0
    Infinity = 1.0 / 0.0
end

if Object == nil then
    local function assign(t1, t2, ...)
        if t2 == nil then return t1 end
        for k, v in next, t2 do t1[k] = v end
        return assign(t1, ...)
    end

    Object = {}
    function Object:is(a, b) return a == b or (a ~= a and b ~= b) end
    function Object:assign(...) return assign(...) end
    function Object:create(o)
        return setmetatable(assign({}, o), getmetatable(o))
    end

    function Object:entries(o)
        local ret = {}
        for k, v in next, o do ret[#ret + 1] = { k, v } end
        return ret
    end

    function Object:keys(o)
        self = {}; for k in next, o do self[#self + 1] = k end
        return self
    end

    function Object:values(o)
        self = {}; for k, v in next, o do self[#self + 1] = v end
        return self
    end

    setmetatable(Object, {
        __call = function(self, new, o)
            if o == nil then return new or {} end
            return o
        end
    })
end

if Array == nil then
    local arr_mt, T = { __index = {} }, table
    function arr_mt.__index:push(val) self[#self + 1] = val end
    function arr_mt.__index:pop()
        local i = #self; local v = self[i]; self[i] = nil; return v end
    function arr_mt.__index:unshift(val) T.insert(self, 1, val) end
    function arr_mt.__index:shift()
        local v = self[1]; T.remove(self, 1); return v end
    function arr_mt.__index:join(s) return T.concat(self, s or ",") end
    function arr_mt:__tostring() return self:join() end
    function Array(_, ...) return setmetatable({...}, arr_mt) end
end

if JSON.stringify(JSON) == JSON then
    local createBuffer = require("string.buffer").new
    local stringify = JSON.stringify

    local function encodeValue(buffer, value, space, indent, detect)
        value = stringify(value)
        if type(value) == "string" then  -- Encoded by the C part
            return buffer:put(value)
        end
        assert(detect[value] == nil, "Converting circular structure to JSON")

        local inner_indent, first = indent .. space, next(value)
        detect[value] = true
        if first == 1 or first == nil then
            buffer:put("[")
            for i, v in ipairs(value) do
                if i ~= 1 then buffer:put(",") end
                buffer:put(inner_indent)
                encodeValue(buffer, v, space, inner_indent, detect)
            end
            buffer:put(indent, "]")
        else
            local colon = space ~= "" and ": " or ":"
            buffer:put("{")
            for k, v in next, value do
                if k ~= first then buffer:put(",") end
                buffer:put(inner_indent, stringify(tostring(k)), colon)
                encodeValue(buffer, v, space, inner_indent, detect)
            end
            buffer:put(indent, "}")
        end
        detect[value] = nil
    end

    function JSON:stringify(value, replacer, space)
        if replacer ~= nil then error("replacer not supported by LJS") end
        if type(space) == "number" then space = (" "):rep(space) end
        local buffer = createBuffer(256)
        encodeValue(buffer, value, space or "", space and "\n" or "", {})
        return buffer:tostring()
    end
end

if Promise ~= nil then
    function Promise.prototype:catch(fun)
        return self["then"](self, nil, fun)  -- Use then() internally
    end

    local pcall, reject = _G.pcall, Promise.reject
    debug.getregistry()["JS:__resolver"] = function(resolve, ...)
        local ok, ret = pcall(...)
        if ok == false then ret = reject(nil, ret) end
        return resolve(nil, ret)
    end

    getmetatable(Promise).constructor = function()
        return error("Promise is not allowed to be extended")
    end
end

if console == nil then
    console = {}

    function console:log(fmt, ...)
        if type(fmt) ~= "string" then return print(fmt) end
        io.stdout:write(string.format(fmt, ...), "\n")
    end

    function console:error(fmt, ...)
        io.stderr:write(string.format(fmt, ...), "\n")
    end

    console.info = console.log
    console.warn = console.error
end

function eval(_, code) return jsrt.loadString(code)() end

local env_mt, dummy_function = { __index = _G }, function(o) end
package.preload["jsrt.class"] = function ()
    local function class_tostring() return 'class {}' end

    local function shared_ctor(self, obj, ...)
        if obj == nil then error("Constructor requires 'new'") end
        self = getmetatable(self)
        self.constructor(setmetatable(obj, self), ...)
        return obj
    end

    local function tostring_helper(obj)
        local method = obj.toString  -- Forward to .toString()
        if method == nil then return "{}" end
        return method(obj)
    end

    return function (ctor, prototype)
        local ancestor, metatable = prototype[0], {
            __index = prototype,
            __tostring = tostring_helper,
            constructor = prototype.constructor,
        }
        prototype.constructor = ctor
        if ancestor ~= nil then
            local super_ctor = getmetatable(ancestor).constructor
            prototype[0] = nil
            if metatable.constructor == nil then
                metatable.constructor = super_ctor
            else setfenv(metatable.constructor,  -- Add super(...)
                setmetatable({ super = super_ctor }, env_mt))
            end
            setmetatable(prototype, { __index = ancestor.prototype })
        elseif metatable.constructor == nil then
            metatable.constructor = dummy_function
        end

        ctor.prototype = prototype
        return setmetatable(ctor, {
            __call = shared_ctor,
            __tostring = class_tostring,
            __index = ancestor,
            __metatable = metatable
        })
    end
end

jsrt.class = require("jsrt.class")

function jsrt.extend_builtins()

    -- Enable concatenation of strings using +
    getmetatable("").__add = function(a, b) return a .. b end
    -- Display nil as null at print
    debug.setmetatable(nil, { __tostring = function() return "null" end })

    local unpack = unpack or table.unpack
    -- This is effective over all functions
    debug.setmetatable(dummy_function, { __index = {
        apply = function (fun, this, args) return fun(this, unpack(args)) end,
        bind = function (fun, this)  -- currently only accepts `this`
            return function(_, ...) return fun(this, ...) end
        end,
        call = function (fun, this, ...) return fun(this, ...) end,
    } })

end

table.insert(package.loaders or package.searchers, 2, function (mod_name)
    local filename = package.searchpath(mod_name, jsrt.path, "/")
    if filename ~= nil then
        local mod = string.endsWith(filename, ".js") and
            jsrt.loadFile(filename) or assert(loadfile(filename, "b"))
        return function()
            local module = { exports = {}, filename = filename }
            setfenv(mod, setmetatable({
                jsrt = jsrt, bit = bit, tostring = tostring,
                module = module, exports = module.exports
            }, env_mt))(module.exports)
            return module.exports
        end
    end
end)

local lua_require = require
function require(_module, module)
    return lua_require(_module or module)
end