package org.ttb.integration.mvc.model;

/**
 * Trace pattern.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracePattern {

    private boolean enabled;
    private String moduleName;
    private String functionName;

    public TracePattern() {
        this(false, "", "");
    }

    public TracePattern(boolean enabled, String module, String functionName) {
        this.enabled = enabled;
        this.moduleName = module;
        this.functionName = functionName;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
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
}
