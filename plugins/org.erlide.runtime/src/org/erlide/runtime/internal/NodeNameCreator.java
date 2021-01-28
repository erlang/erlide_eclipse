package org.erlide.runtime.internal;

import org.eclipse.xtend2.lib.StringConcatenation;

@SuppressWarnings("all")
public class NodeNameCreator {
    public static String create(final String hostName) {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("jerlide_");
        final String _timeSuffix = NodeNameCreator.getTimeSuffix();
        _builder.append(_timeSuffix);
        _builder.append("@");
        _builder.append(hostName);
        return _builder.toString();
    }

    private static String getTimeSuffix() {
        return Long.toHexString(System.currentTimeMillis() & 0xFFFFFFFL);
    }
}
