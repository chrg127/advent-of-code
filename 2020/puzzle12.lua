local east, north, eastw, northw  = 0, 0, 10, 1

local lookup = {
    ["N"] = 1, ["E"] = 2, ["S"] = 3, ["W"] = 4, ["L"] = 5, ["R"] = 6,
    ["F"] = 2, -- start facing east
}
local movetab = {
    [1] = function (n) north = north + n end, -- move north (^)
    [2] = function (n) east = east + n end,   -- move east (>)
    [3] = function (n) north = north - n end, -- move south
    [4] = function (n) east = east - n end,   -- move west (<)
    [5] = function (n)
        local i = n/90
        while i ~= 0 do
            local x = lookup["F"]
            lookup["F"] = x == 1 and 4 or x-1
            i = i-1
        end
    end,
    [6] = function (n)
        local i = n/90
        while i ~= 0 do
            local x = lookup["F"]
            lookup["F"] = x == 4 and 1 or x+1
            i = i-1
        end
    end,
}

local movetab2 = {
    ["N"] = function (n) northw = northw + n end, -- move north (^)
    ["E"] = function (n) eastw = eastw + n end,   -- move east (>)
    ["S"] = function (n) northw = northw - n end, -- move south
    ["W"] = function (n) eastw = eastw - n end,   -- move west (<)
    ["R"] = function (n)
        local i = n/90
        while i ~= 0 do
            eastw, northw = northw, eastw
            northw = northw * -1
            i = i-1
        end
    end,
    ["L"] = function (n)
        local i = n/90
        while i ~= 0 do
            eastw, northw = northw, eastw
            eastw = eastw * -1
            i = i-1
        end
    end,
    ["F"] = function (n)
        north = north + northw * n
        east  = east  + eastw * n
    end
}

for line in io.lines("input12.txt") do
    -- part 1:
    --movetab[lookup[string.sub(line, 1, 1)]](string.sub(line, 2, #line))
    -- part 2:
    movetab2[string.sub(line, 1, 1)](string.sub(line, 2, #line))
end
print("east:" .. east .. " north:" .. north .. " res:" .. math.abs(east) + (math.abs(north)))
