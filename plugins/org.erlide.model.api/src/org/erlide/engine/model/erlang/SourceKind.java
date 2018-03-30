package org.erlide.engine.model.erlang;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

public enum SourceKind {
    BAD, HRL, ERL, YRL;

    public static SourceKind extensionToModuleKind(final String ext) {
        if (ext == null) {
            return SourceKind.BAD;
        }
        if ("hrl".equalsIgnoreCase(ext)) {
            return SourceKind.HRL;
        }
        if ("erl".equalsIgnoreCase(ext)) {
            return SourceKind.ERL;
        }
        if ("yrl".equalsIgnoreCase(ext)) {
            return SourceKind.YRL;
        }
        return SourceKind.BAD;
    }

    public static SourceKind nameToModuleKind(final String name) {
        final IPath p = new Path(name);
        return SourceKind.extensionToModuleKind(p.getFileExtension());
    }

    public static boolean hasModuleExtension(final String name) {
        return SourceKind.nameToModuleKind(name) != SourceKind.BAD;
    }

    public static boolean hasErlExtension(final String name) {
        return SourceKind.nameToModuleKind(name) == SourceKind.ERL;
    }

    public static boolean hasHrlExtension(final String name) {
        return SourceKind.nameToModuleKind(name) == SourceKind.HRL;
    }
}
