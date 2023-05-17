import java.io.*;
import java.util.*;

class Pair<T, V> {
    T first;
    V second;
    Pair(T t, V v) { first = t; second = v; }
}

class Tree {
    char tag;
    long value;
    Tree left = null, right = null;
    Tree() {}
    Tree(char t, long v, Tree l, Tree r) {
        tag = t;
        value = v;
        left = l;
        right = r;
    }
}

public class puzzle18 {
    public static Pair<Long, Integer> eval(String line, int i) {
        long res;
        if (line.charAt(i) == '(') {
            var p = apply(line, i+1);
            //var p = apply2(line, i+1);
            res   = p.first;
            i     = p.second + 1;
        } else {
            res = line.charAt(i++) - '0';
        }
        return new Pair<>(res, i);
    }

    public static Pair<Long, Integer> apply(String line, int i) {
        long res = 0, n = 0;
        char op = 0;
        
        while (i < line.length() || line.charAt(i) != ')') {
            var p = eval(line, i);
            n = p.first;
            i = p.second;
            switch (op) {
                case 0: res = n; break;
                case '+': res += n; break;
                case '*': res *= n; break;
            }
            if (i == line.length() || line.charAt(i) == ')') {
                break;
            }
            op = line.charAt(i++);
        }
        return new Pair<Long, Integer>(res, i);
    }

    public static Pair<Tree, Integer> evalNode(Tree ast, String line, int i) {
        char c = line.charAt(i);
        if (Character.isDigit(c)) {
            return new Pair<>(new Tree('n', c - '0', null, null), i+1);
        }
        switch (c) {
        case '(':
            return buildAst(line, i+1);
        case '+':
            var p1 = evalNode(ast, line, i+1);
            return new Pair<>(new Tree('+', 0, ast, p1.first), p1.second);
        case '*':
            var p2 = buildAst(line, i+1);
            if (p2.second < line.length() && line.charAt(p2.second-1) == ')')
                p2.second--;
            return new Pair<>(new Tree('*', 0, ast, p2.first), p2.second);
        }
        return null;
    }

    public static Pair<Tree, Integer> buildAst(String line, int i) {
        long res = 0;
        char op = 0, c = 0;
        Tree ast = null;
        
        var firstVal = evalNode(null, line, i);
        ast = firstVal.first;
        i = firstVal.second;
        while (i < line.length() && line.charAt(i) != ')') {
            var p = evalNode(ast, line, i);
            ast = p.first;
            i = p.second;
        }
        if (i != line.length()) {
            i++;
        }
        return new Pair<>(ast, i);
    }

    public static long countAst(Tree root) {
        if (root == null) {
            return 0;
        }
        switch (root.tag) {
        case 'n': return root.value;
        case '+': return countAst(root.left) + countAst(root.right);
        case '*': return countAst(root.left) * countAst(root.right);
        }
        return 0;
    }

    public static void main(String[] args) throws Exception {
        try (var f = new BufferedReader(new FileReader(new File("input18.txt")))) {
            String line;
            long sum = 0;

            while ((line = f.readLine()) != null) {
                String s = line.replaceAll(" ", "");
                // part 1
                // long res = apply(s, 0).first;
                // part 2
                sum += countAst(buildAst(s, 0).first);
            }
            System.out.println(sum);
        }
    }
}
