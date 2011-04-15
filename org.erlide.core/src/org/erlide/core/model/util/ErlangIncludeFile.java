package org.erlide.core.model.util;

public class ErlangIncludeFile {

    boolean systemInclude;

    String filename;

    /**
     * @param systemInclude
     * @param filename
     */
    public ErlangIncludeFile(final boolean systemInclude, final String filename) {
        super();
        this.systemInclude = systemInclude;
        this.filename = filename;
    }

    public String getFilename() {
        return filename;
    }

    public boolean isSystemInclude() {
        return systemInclude;
    }

    public String getFilenameLastPart() {
        final int i = filename.lastIndexOf('/');
        if (i != -1) {
            return filename.substring(i + 1);
        }
        return filename;
    }
}
