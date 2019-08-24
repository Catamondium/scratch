import java.lang.annotation.*;
import java.util.function.Function;
import java.util.Iterator;
import java.util.Scanner;
import java.util.NoSuchElementException;

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
 * @param <I> input element type
 * @param <R> output element type
 */
class Mapper<I, R> implements Iterator<R> {
    private Function<? super I, ? extends R> fun;
    private Iterator<I> source;

    /**
     * @param source iterator to wrap
     * @param fun    function to apply against source elements
     */
    Mapper(Iterator<I> source, Function<? super I, ? extends R> fun) {
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
            // Variance rules allow String -> Object transform
            Mapper<String, Object> m = new Mapper<String, Object>(s, String::length);
            while (m.hasNext()) {
                System.out.println(m.next());
            }
        }
    }
}