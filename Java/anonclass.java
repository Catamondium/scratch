import java.util.*;

class anonclass {
    public static Iterator<Integer> countRange(int x0, int x1) {
        return new Iterator<Integer>() {
            int i = x0;
            public Integer next() {
                return (i < x1) ? i++ : 0;
            }

            public boolean hasNext() {
                return i < x1;
            }
        };
    }

    public static void main(String[] args) {
        for(var i = countRange(2, 20); i.hasNext();) {
            System.out.println(i.next());
        }
    }
}