package org.erlide.wrangler.refactoring.backend;

/**
 * Simple class that contains user refactoring's informations
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class UserRefactoringInfo {

    private String label; // menu item's label
    private String callback; // name of callback module

    public UserRefactoringInfo(String module) {
        callback = module;
        label = prepareLabel(module);
    }

    public String getLabel() {
        return label;
    }

    public String getCallback() {
        return callback;
    }

    // prepare label's text based on module name
    private String prepareLabel(String module) {
        StringBuffer buf = new StringBuffer();
        for (String part : module.split("_")) {
            buf.append(Character.toUpperCase(part.charAt(0)))
                    .append(part.substring(1)).append(" ");
        }
        return buf.toString().trim();
    }

    @Override
    public String toString() {
        return label;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof UserRefactoringInfo
                && ((UserRefactoringInfo) obj).getCallback().equals(callback))
            return true;
        return false;
    }

    @Override
    public int hashCode() {
        return callback.hashCode();
    }

}
