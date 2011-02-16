package org.erlide.tracing.core.mvc.model;

import java.io.Serializable;

/**
 * Traced nodes.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracedNode implements Serializable {

    private static final long serialVersionUID = 5089264076119831093L;

    private boolean enabled;
    private String nodeName = "";
    private String cookie = "";

    @Override
    public boolean equals(final Object o) {
        if (o == null || !o.getClass().equals(TracedNode.class)) {
            return false;
        }
        final TracedNode tracedNode = (TracedNode) o;
        return nodeName != null ? nodeName.equals(tracedNode.nodeName)
                : tracedNode.nodeName == null;
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

    public String getNodeName() {
        return nodeName;
    }

    public void setNodeName(final String nodeName) {
        this.nodeName = nodeName;
    }

    public String getCookie() {
        return cookie;
    }

    public void setCookie(final String cookie) {
        this.cookie = cookie;
    }
}
