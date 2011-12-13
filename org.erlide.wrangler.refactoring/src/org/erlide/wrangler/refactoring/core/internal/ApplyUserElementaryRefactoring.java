package org.erlide.wrangler.refactoring.core.internal;

/**
 * Logic for running user-defined, commited elementary refactrings
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class ApplyUserElementaryRefactoring extends UserElementaryRefactoring {

    private final String name; // name of the refactoring

    public ApplyUserElementaryRefactoring(final String name,
            final String callbackModule) {
        this.name = name;
        setCallbackModuleName(callbackModule);
    }

    @Override
    public String getName() {
        return name;
    }

}
