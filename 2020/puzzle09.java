import java.util.*;
import java.io.*;

class puzzle9 {
    public static List<Long> nums = new ArrayList<>();

    public static long part1(BufferedReader file, int preamble) throws Exception {
        String line;
        int first = preamble-1, last = 0;
        while ((line = file.readLine()) != null)
            nums.add(Long.valueOf(line));

        for (int i = preamble; i < nums.size(); last++, first++, i++) {
            long n = nums.get(i);
            boolean valid = false;
            for (int x = last; x <= first; x++) {
                for (int y = last; y <= first; y++)
                    if (n == nums.get(x) + nums.get(y))
                        valid = true;
            }
            if (!valid) return n;
        }
        return -1;
    }

    public static long part2(long n) {
        long sum = 0;
        int i = 0, j = 0;
        for ( ; j < nums.size() && sum != n; ) {
            if (sum < n) sum += nums.get(++j);
            else if (sum > n) sum -= nums.get(++i);
        }
        List<Long> range = new ArrayList<>(nums.size());
        for (int x = i+1; x < j; x++)
            range.add(nums.get(x));
        Collections.sort(range);
        return range.get(0)+range.get(range.size()-1);
    }

    public static void main(String[] args) throws Exception {
        try (var f = new BufferedReader(new FileReader(new File("input9.txt")))) {
            long res1 = part1(f, 25);
            System.out.println("part 1: " + res1);
            System.out.println("part 2: " + part2(res1));
        }
    }
}
