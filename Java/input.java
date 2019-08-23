import java.lang.annotation.*;
import java.util.function.Function;
import java.util.Iterator;
import java.util.Scanner;
import java.util.NoSuchElementException;
import java.io.BufferedReader;

/*
 *
 * annotation and wrapping iterator example
 */

@Documented
@interface Preamble {
    String author();

    String date();
}

/**
 * @param <I> input object
 * @param <R> output object
 */
class Mapper<I, R> implements Iterator<R> {
    private Function<I, R> fun;
    private Iterator<I> source;

    /**
     * @param source iterator to wrap
     * @param fun    function to apply against source elements
     */
    Mapper(Iterator<I> source, Function<I, R> fun) {
        this.source = source;
        this.fun = fun;
    }

    /**
     * @return applies fun to source.next()
     */
    @Override
    public R next() throws NoSuchElementException {
        return fun.apply(source.next());
    }

    @Override
    public boolean hasNext() {
        return source.hasNext();
    }
}

@Preamble(author = "ABC", date = "DEF")
class input {
    public static void main(String[] argv) {
        try (Scanner s = new Scanner(System.in)) {
            Mapper<String, Integer> m = new Mapper<String, Integer>(s, String::length);
            while (m.hasNext()) {
                System.out.println(m.next());
            }
        }
    }
}