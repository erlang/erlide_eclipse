package org.erlide.wrangler.refactoring.core.internal;


/**
 * Logic for running user-defined, commited elementary refactrings
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class ApplyUserElementaryRefactoring extends UserElementaryRefactoring {

    private String name; // name of the refactoring

    public ApplyUserElementaryRefactoring(String name, String callbackModule) {
        this.name = name;
        setCallbackModuleName(callbackModule);
    }

    @Override
    public String getName() {
        return name;
    }

}
