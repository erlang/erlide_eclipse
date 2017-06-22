package org.erlide.util;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.nio.charset.Charset;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;

public class FileUtils {

    /**
     * Whether the underlying file system is case sensitive.
     */
    public static final boolean IS_CASE_SENSITIVE = !new File("Temp")
            .equals(new File("temp"));

    public static void createFileInProjectAt(final IProject project,
            final String filename, final String content, final Charset encoding)
            throws CoreException {
        final IFile res = project.getFile(filename);
        res.create(new ByteArrayInputStream(content.getBytes(encoding)), false, null);
    }

}
