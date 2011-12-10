package org.erlide.wrangler.refactoring.core.internal;

/**
 * Logic for running ad hoc user-defined elementary refactorings
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class ApplyAdhocElemRefactoring extends UserElementaryRefactoring {

    private final UserAdhocRefactoring delegate; // delegate for adhoc specific

    // functionality

    public ApplyAdhocElemRefactoring() {
        delegate = new UserAdhocRefactoring(this);
    }

    @Override
    public String getName() {
        return "Apply ad hoc refactoring";
    }

    @Override
    public boolean fetchParPrompts() {
        if (fetched) {
            return true;
        }

        return delegate.load() && super.fetchParPrompts();
    }

}
