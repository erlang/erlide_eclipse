package org.ttb.integration.mvc.model;

import java.io.Serializable;

/**
 * Trace pattern.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracePattern implements Serializable {

    private static final long serialVersionUID = 532722456924811842L;

    private boolean enabled;
    private boolean local;
    private int arity = -1;
    private String moduleName = "";
    private String functionName = "";
    private MatchSpec matchSpec;

    public TracePattern() {
        this.matchSpec = new MatchSpec();
        matchSpec.setFunctionString("");
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || !o.getClass().equals(TracePattern.class))
            return false;
        TracePattern tp = (TracePattern) o;

        return arity == tp.arity && moduleName.equals(tp.moduleName) && functionName.equals(tp.functionName) && matchSpec.equals(tp.matchSpec);
    }

    @Override
    public int hashCode() {
        // in set, when two objects have same hash code they are compared using
        // theirs equals methods
        return 0;
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

    public MatchSpec getMatchSpec() {
        return matchSpec;
    }

    public void setMatchSpec(MatchSpec matchSpec) {
        this.matchSpec = matchSpec;
    }
}
