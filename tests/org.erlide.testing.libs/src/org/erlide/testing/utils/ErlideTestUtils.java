package org.erlide.testing.utils;

import java.io.File;
import java.io.InputStream;

import org.eclipse.xtext.xbase.lib.Exceptions;

@SuppressWarnings("all")
public class ErlideTestUtils {
    public static void deleteRecursive(final File d) {
        final boolean _exists = d.exists();
        if (_exists) {
            final File[] filesOpt = d.listFiles();
            for (final File file : filesOpt) {
                final boolean _isDirectory = file.isDirectory();
                if (_isDirectory) {
                    ErlideTestUtils.deleteRecursive(file);
                } else {
                    file.delete();
                }
            }
            d.delete();
        }
    }

    public File createTempDir(final String name) {
        File _xblockexpression = null;
        {
            final String _property = System.getProperty("user.home");
            final String userHome = new File(_property).getAbsolutePath();
            final File rootDir = new File(userHome, "ErlangCoreTestTempDir");
            final File result = new File(rootDir, name);
            final boolean _exists = result.exists();
            if (_exists) {
                ErlideTestUtils.deleteRecursive(result);
            }
            _xblockexpression = result;
        }
        return _xblockexpression;
    }

    public void deleteTempDirs() {
        final String _property = System.getProperty("user.home");
        final String userHome = new File(_property).getAbsolutePath();
        final File rootDir = new File(userHome, "ErlangCoreTestTempDir");
        final boolean _exists = rootDir.exists();
        if (_exists) {
            ErlideTestUtils.deleteRecursive(rootDir);
        }
    }

    public static String readAndClose(final InputStream inputStream) {
        try {
            String _xblockexpression = null;
            {
                final StringBuilder stringBuilder = new StringBuilder();
                try {
                    int ch = inputStream.read();
                    while (ch != -1) {
                        {
                            stringBuilder.append((char) ch);
                            ch = inputStream.read();
                        }
                    }
                } finally {
                    inputStream.close();
                }
                _xblockexpression = stringBuilder.toString();
            }
            return _xblockexpression;
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }
}
