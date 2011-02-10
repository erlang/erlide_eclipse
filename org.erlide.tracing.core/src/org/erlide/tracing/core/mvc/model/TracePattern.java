package org.erlide.tracing.core.mvc.model;

import java.io.Serializable;

import com.ericsson.otp.erlang.OtpErlangAtom;

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

    /**
     * Creates trace pattern object with empty match spec.
     */
    public TracePattern() {
        this(false);
    }

    /**
     * Creates trace pattern object. If specified match spec is set to "x"
     * (shortcut for match spec that matches exceptions and return values).
     * 
     * @param setXMatchSpec
     *            if match spec should be set to "x"
     */
    public TracePattern(final boolean setXMatchSpec) {
        matchSpec = new MatchSpec();
        if (setXMatchSpec) {
            matchSpec.setFunctionString("x");
            matchSpec.setMsObject(new OtpErlangAtom("x"));
        }
    }

    @Override
    public boolean equals(final Object o) {
        if (o == null || !o.getClass().equals(TracePattern.class)) {
            return false;
        }
        final TracePattern tp = (TracePattern) o;

        return arity == tp.arity && moduleName.equals(tp.moduleName)
                && functionName.equals(tp.functionName)
                && matchSpec.equals(tp.matchSpec);
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

    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    public boolean isLocal() {
        return local;
    }

    public void setLocal(final boolean local) {
        this.local = local;
    }

    public String getModuleName() {
        return moduleName;
    }

    public void setModuleName(final String pattern) {
        moduleName = pattern;
    }

    public String getFunctionName() {
        return functionName;
    }

    public void setFunctionName(final String functionName) {
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
    public void setArity(final int arity) {
        this.arity = arity;
    }

    public MatchSpec getMatchSpec() {
        return matchSpec;
    }

    public void setMatchSpec(final MatchSpec matchSpec) {
        this.matchSpec = matchSpec;
    }
}
