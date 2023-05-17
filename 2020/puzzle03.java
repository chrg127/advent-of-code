import java.io.*;
import java.util.*;

public class puzzle3 {
    public static int part1(BufferedReader f) throws Exception {
        String line;
        int pos = 3, numTrees = 0;
        line = f.readLine();
        while ((line = f.readLine()) != null) {
            numTrees += line.charAt(pos) == '#' ? 1 : 0;
            pos = (pos + 3) % line.length();
        }
        return numTrees;
    }

    public static long part2(BufferedReader f) throws Exception {
        String line;
        final int[] st  = { 1, 3, 5, 7, 1 };
        int[] pos = { 1, 3, 5, 7, 1 };
        long[] res = { 0, 0, 0, 0, 0 };
        int skip = 1;
        line = f.readLine();
        while ((line = f.readLine()) != null) {
            for (int i = 0; i < pos.length-1; i++) {
                res[i] += line.charAt(pos[i]) == '#' ? 1 : 0;
                pos[i] = (pos[i] + st[i]) % line.length();
            }
            if (skip-- == 0) {
                res[4] += line.charAt(pos[4]) == '#' ? 1 : 0;
                pos[4] = (pos[4] + st[4]) % line.length();
                skip = 1;
            }
        }
        for (var x : res) System.out.println(x);
        return res[0] * res[1] * res[2] * res[3] * res[4];
    }

    public static void main(String[] args) throws Exception {
        try (var f = new BufferedReader(new FileReader(new File("input3.txt")))) {
            System.out.println(part2(f));
        }
    }
}

