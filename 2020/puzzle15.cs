using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

namespace PuzzleNS {
    class Puzzle {
        static int solve(int target, List<int> nums)
        {
            Dictionary<int, int> tab = new Dictionary<int, int>();
            for (int i = 0; i < nums.Count()-1; i++)
                tab.Add(nums[i], i+1);
            int last_num = nums[nums.Count()-1];
            for (int t = nums.Count()+1; t <= target; t++) {
                int val, tmp = last_num;
                last_num = tab.TryGetValue(last_num, out val) ? t-1-val : 0;
                tab[tmp] = t-1;
            }
            return last_num;
        }

        static void Main(string[] args)
        {
            List<int> nums = File.ReadAllLines("input15.txt")[0].Split(',').ToList().Select(x => int.Parse(x)).ToList();
            Console.WriteLine("part 1: {0}", solve(2020, nums));
            Console.WriteLine("part 2: {0}", solve(30000000, nums));
        }
    }
}
