package diydi;

public class Providers {

    public static <T> Provider<T> of(final T value) {
        return new Provider<T>() {
            @Override
            public T get() {
                return value;
            }
        };
    }

}
