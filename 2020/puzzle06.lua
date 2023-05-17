    function uniq(s)
        local hashtab = {}
        local res     = {}
        for i=1, #s do
            if not hashtab[s:sub(i,i)] then
                res[#res+1] = s:sub(i,i)
                hashtab[s:sub(i,i)] = true
            end
        end
        return res
    end

    function sum_all(t)
        local sum = 0
        for i=1, #t do sum = sum + t[i] end
        return sum
    end

    function samechars(strtab)
        local chartab = {}
        for k,s in ipairs(strtab) do
            for i=1, string.len(s) do
                c = s:sub(i,i)
                chartab[c] = chartab[c] == nil and 1 or chartab[c] + 1
            end
        end
        nsame = 0
        for k,v in pairs(chartab) do
            if v == #strtab then nsame = nsame + 1 end
        end
        return nsame
    end


    function process_part1(lines)
        local qstr = ""
        local vtab = {}
        for line in lines do
            if line ~= "" then
                qstr = qstr .. line
            else
                vtab[#vtab+1] = #uniq(qstr)
                qstr = ""
            end
        end
        if qstr ~= "" then vtab[#vtab+1] = (#uniq(qstr)) end
        print("sum part 1: "..sum_all(vtab))
    end

    function process_part2(lines)
        local strtab = {}
        local vtab = {}
        for line in lines do
            if line == "" then
                vtab[#vtab+1] = samechars(strtab)
                strtab = {}
            else
                strtab[#strtab+1] = line
            end
        end
        if strtab ~= {} then vtab[#vtab+1] = samechars(strtab) end
        print("sum part 2: "..sum_all(vtab))
    end

    process_part1(io.lines("input6.txt"))
    process_part2(io.lines("input6.txt"))
