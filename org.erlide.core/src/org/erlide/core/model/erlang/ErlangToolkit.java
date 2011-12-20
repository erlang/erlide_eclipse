package org.erlide.core.model.erlang;

import org.eclipse.core.resources.IResource;
import org.erlide.core.internal.model.erlang.ErlParser;
import org.erlide.core.internal.model.erlang.ErlScanner;
import org.erlide.jinterface.Assert;

public class ErlangToolkit {

    public static String createScannerModuleName(final IErlModule module) {
        Assert.isNotNull(module);
        final IResource res = module.getResource();
        if (res != null) {
            return createScannerModuleNameFromResource(res);
        } else if (module.getFilePath() != null && module.isRealFile()) {
            return "mod" + module.getFilePath().hashCode() + "__"
                    + module.getName();
        }
        // This is not used more than temporarily, so it's OK to have
        // a name that's temporary, as long as it's unique
        return "mod" + module.hashCode() + "_";
    }

    public static String createScannerModuleNameFromResource(final IResource res) {
        String resName;
        resName = "mod" + res.getFullPath().toPortableString().hashCode() + "_"
                + res.getName();
        return resName;
    }

    public static IErlParser createParser() {
        return new ErlParser();
    }

    public static IErlScanner createScanner(final String scannerName,
            final String initialText, final String path, final boolean useCaches) {
        return new ErlScanner(scannerName, initialText, path, useCaches);
    }

}
