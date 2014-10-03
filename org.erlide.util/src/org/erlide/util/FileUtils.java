package org.erlide.util;

import java.io.ByteArrayInputStream;
import java.nio.charset.Charset;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;

public class FileUtils {

    public static void createFileInProjectAt(final IProject project,
            final String filename, final String content, final Charset encoding)
            throws CoreException {
        final IFile res = project.getFile(filename);
        res.create(new ByteArrayInputStream(content.getBytes(encoding)), false, null);
    }

}
