package org.erlide.cover.runtime.launch;

/**
 * Supported frameworks
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public enum FrameworkType {

    //@formatter:off
    EUNIT("EUnit"), 
    CT("Common Test"), 
    QC("QuickCheck");
    //@formatter:on

    private final String fullRepr;

    private FrameworkType(final String str) {
        fullRepr = str;
    }

    public String getRepr() {
        return fullRepr;
    }

    public static FrameworkType find(final String repr) {
        for (final FrameworkType t : FrameworkType.values()) {
            if (t.getRepr().equals(repr)) {
                return t;
            }
        }
        return null;
    }

}
