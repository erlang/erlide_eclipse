package org.erlide.engine.model.erlang;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

public enum SourceKind {
    BAD, HRL, ERL, YRL;

    public static SourceKind extensionToModuleKind(final String ext) {
        if (ext == null) {
            return BAD;
        }
        if (ext.equalsIgnoreCase("hrl")) {
            return HRL;
        }
        if (ext.equalsIgnoreCase("erl")) {
            return ERL;
        }
        if (ext.equalsIgnoreCase("yrl")) {
            return YRL;
        }
        return BAD;
    }

    public static SourceKind nameToModuleKind(final String name) {
        final IPath p = new Path(name);
        return SourceKind.extensionToModuleKind(p.getFileExtension());
    }

    public static boolean hasModuleExtension(final String name) {
        return nameToModuleKind(name) != BAD;
    }

    public static boolean hasErlExtension(final String name) {
        return nameToModuleKind(name) == ERL;
    }

    public static boolean hasHrlExtension(final String name) {
        return nameToModuleKind(name) == HRL;
    }
}
