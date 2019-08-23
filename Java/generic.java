// Generic interface/class, invariant about T
interface MyPred<T> {
    boolean test(T obj);
}

class generic {
    // Generic method, invariant about T
    public static <T> String getClsName(T obj) {
        return obj.getClass().getName();
    }

    public static void main(String[] argv) {
        MyPred<String> sp = (String x) -> x.endsWith("C");
        System.out.println(sp);
    }
}