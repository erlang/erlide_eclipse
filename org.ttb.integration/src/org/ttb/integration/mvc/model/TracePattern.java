package org.ttb.integration.mvc.model;

/**
 * Trace pattern.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracePattern {

    private boolean enabled;
    private boolean local;
    private int arity = -1;
    private String moduleName = "";
    private String functionName = "";

    public TracePattern() {
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public boolean isLocal() {
        return local;
    }

    public void setLocal(boolean local) {
        this.local = local;
    }

    public String getModuleName() {
        return moduleName;
    }

    public void setModuleName(String pattern) {
        this.moduleName = pattern;
    }

    public String getFunctionName() {
        return functionName;
    }

    public void setFunctionName(String functionName) {
        this.functionName = functionName;
    }

    public int getArity() {
        return arity;
    }

    /**
     * Sets arity. If given value is less than 0 it means that arity should not
     * be specified while setting trace pattern (<code>ttb:tp</code> function).
     * 
     * @param arity
     *            arity value
     */
    public void setArity(int arity) {
        this.arity = arity;
    }
}
