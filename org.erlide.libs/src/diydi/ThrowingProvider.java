package diydi;

/**
 * Abstraction to hide a concrete factory that can throw
 */
public interface ThrowingProvider<Result, E extends Throwable> {
    Result get() throws E;
}
