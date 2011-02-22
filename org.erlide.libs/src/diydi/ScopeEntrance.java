package diydi;

/**
 * Abstraction to hide the first injection in a newly created scope
 */
public interface ScopeEntrance<Scope, Result> {
    Result get(Scope scope);
}
