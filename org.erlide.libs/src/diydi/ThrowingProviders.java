package diydi;

import java.util.LinkedList;
import java.util.Queue;

public class ThrowingProviders {
    /**
     * Creates a {@link ThrowingProvider} that will return the given values
     */
    public static <Result, E extends Throwable> ThrowingProviderBuilder<Result, E> of(
            final Result... results) {
        return new ThrowingProviderBuilder<Result, E>().of(results);
    }

    /**
     * Creates a {@link ThrowingProvider} that will throw the given exceptions
     */
    public static <Result, E extends Throwable> ThrowingProviderBuilder<Result, E> throwing(
            final E... exceptions) {
        return new ThrowingProviderBuilder<Result, E>().throwing(exceptions);
    }

    public static class ThrowingProviderBuilder<Result, E extends Throwable>
            implements ThrowingProvider<Result, E> {
        private final Queue<ThrowingProvider<Result, E>> delegates = new LinkedList<ThrowingProvider<Result, E>>();

        public Result get() throws E {
            return delegates.remove().get();
        }

        /**
         * Chains additional results for this provider to return
         */
        public ThrowingProviderBuilder<Result, E> of(final Result... results) {
            for (final Result result : results) {
                delegates.add(new ThrowingProvider<Result, E>() {
                    public Result get() {
                        return result;
                    }
                });
            }
            return this;
        }

        /**
         * Chains additional exceptions for this provider to throw
         */
        public ThrowingProviderBuilder<Result, E> throwing(
                final E... exceptions) {
            for (final E exception : exceptions) {
                delegates.add(new ThrowingProvider<Result, E>() {
                    public Result get() throws E {
                        throw exception;
                    }
                });
            }
            return this;
        }
    }
}
