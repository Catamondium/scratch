/*
 * try-with-resource usage
 * closest equivalent to RAII
 * similar semantics to Python 'with' clauses
 *
 * try-withable objects must implement AutoCloseable
 */

class Test implements AutoCloseable {
    public static int made = 0;
    public int id;

    public Test() {
        this.id = made++;
        System.out.format("CREATE:\t%s\n", this.id);
    }

    @Override
    public void close() {
        System.out.format("CLOSE:\t%s\n", this.id);
    }
}

class tryresource {
    public static void main(String[] args) {
        Test baretest = new Test();
        baretest.close();

        // Try-with-resource
        // equivalent to try-finally
        try (Test res = new Test()) {
            System.out.println("body");
        }
    }
}