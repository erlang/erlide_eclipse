package org.erlide.backend;

public final class NodeHostClassifier {
    public final HostnameType host;
    public final NodeType mode;

    public static enum HostnameType {
        NONE, SHORT, LONG
    }

    public static enum NodeType {
        LOCAL_STANDALONE, LOCAL_DISTRIBUTED, REMOTE
    }

    public NodeHostClassifier(final String name) {
        this(name, BackendUtils.getErlangHostName(false), BackendUtils
                .getErlangHostName(true));
    }

    NodeHostClassifier(final String name, final String shortThis,
            final String longThis) {
        if (name.length() == 0) {
            host = HostnameType.NONE;
            mode = NodeType.LOCAL_STANDALONE;
        } else {
            final String[] parts = name.split("@");
            if (parts.length == 1) {
                host = HostnameType.NONE;
                mode = NodeType.LOCAL_DISTRIBUTED;
            } else {
                final String ahost = parts[1];
                if (ahost.contains(".")) {
                    host = HostnameType.LONG;
                } else {
                    host = HostnameType.SHORT;
                }
                if (ahost.equals(shortThis) || ahost.equals(longThis)) {
                    mode = NodeType.LOCAL_DISTRIBUTED;
                } else {
                    mode = NodeType.REMOTE;
                }
            }
        }
    }

}
