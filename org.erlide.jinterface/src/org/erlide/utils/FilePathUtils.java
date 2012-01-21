package org.erlide.utils;

import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.Path;

public class FilePathUtils {
    public static boolean equalFilePaths(String path_1, String path_2) {
        Assert.isNotNull(path_1);
        Assert.isNotNull(path_2);
        if (!EFS.getLocalFileSystem().isCaseSensitive()) {
            path_1 = path_1.toLowerCase();
            path_2 = path_2.toLowerCase();
        }
        final Path path1 = new Path(path_1);
        final Path path2 = new Path(path_2);
        return path1.equals(path2);
    }

}
