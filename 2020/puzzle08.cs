using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

namespace NS {
    class Puzzle {
        static List<int> opArr = new List<int>();
        static int accum = 0, pc = 0;

        static Dictionary<string, int> opDict = new Dictionary<string, int> { { "nop", 0 }, { "acc", 1 }, { "jmp", 2 } };
        static Tuple<int, int> parseLine(string[] splitted) { return new Tuple<int, int>(opDict[splitted[0]], int.Parse(splitted[1])); }

        static void exec(List<int> instructions, List<bool> executed)
        {
            executed[pc] = true;
            switch (instructions[pc]) {
            case 0: pc++; break;
            case 1: accum += opArr[pc]; pc++; break;
            case 2: pc += opArr[pc]; break;
            }
        }
        
        static bool test(List<int> instructions, List<bool> executed)
        {
            accum = pc = 0;
            while (pc < instructions.Count()) {
                if (executed[pc]) return false;
                exec(instructions, executed);
            }
            Console.WriteLine(accum);
            return true;
        }
        
        static void part1(List<int> instrArr, List<bool> executed)
        {
            while (!executed[pc])
                exec(instrArr, executed);
            Console.WriteLine(accum);
        }

        static void part2(List<int> instrArr, List<bool> executed)
        {
            int i = 0;
            int last_op = instrArr[i];
            // brute force solution
            while (!test(instrArr, new List<bool>(executed))) {
                instrArr[i] = last_op;
                while (++i < instrArr.Count() && instrArr[i] == 1)
                    ;
                if (i == instrArr.Count()) return;
                last_op = instrArr[i];
                instrArr[i] = (instrArr[i] == 0) ? 2 : 0;
            }
        }

        static void Main(string[] args)
        {
            List<int>  instrArr = new List<int>();
            List<bool> executed = new List<bool>(instrArr.Count());
            File.ReadAllLines("input08.txt").ToList().Select(line => parseLine(line.Replace('+', '0').Split(' '))).ToList().ForEach(t => {
                    executed.Add(false); instrArr.Add(t.Item1); opArr.Add(t.Item2); });
            part1(instrArr, executed);
        }
    }
}
