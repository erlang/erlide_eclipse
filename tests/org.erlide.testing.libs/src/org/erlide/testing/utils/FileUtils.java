package org.erlide.testing.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import org.eclipse.xtext.xbase.lib.Exceptions;

@SuppressWarnings("all")
public class FileUtils {
    public static byte[] read(final File file) {
        try {
            byte[] _xblockexpression = null;
            {
                final long _length = file.length();
                final int len = (int) _length;
                final byte[] bytes = new byte[len];
                final FileInputStream stream = new FileInputStream(file);
                int bytesRead = 0;
                int lastReadSize = 0;
                try {
                    while (lastReadSize != -1 && bytesRead != len) {
                        {
                            lastReadSize = stream.read(bytes, bytesRead, len - bytesRead);
                            final int _bytesRead = bytesRead;
                            bytesRead = _bytesRead + lastReadSize;
                        }
                    }
                } finally {
                    stream.close();
                }
                _xblockexpression = bytes;
            }
            return _xblockexpression;
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    /**
     * Copy file from src (path to the original file) to dest (path to the destination
     * file).
     */
    public static void copy(final File src, final File dest) {
        try {
            final byte[] srcBytes = FileUtils.read(src);
            final FileOutputStream out = new FileOutputStream(dest);
            try {
                out.write(srcBytes);
            } finally {
                out.close();
            }
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    /**
     * Copy the given source directory (and all its contents) to the given target
     * directory.
     */
    public static void copyDirectory(final File source, final File target) {
        final boolean _exists = target.exists();
        final boolean _not = !_exists;
        if (_not) {
            target.mkdirs();
        }
        final File[] files = source.listFiles();
        if (files == null) {
            return;
        }
        for (final File src : files) {
            {
                final String name = src.getName();
                final boolean _shouldSkip = FileUtils.shouldSkip(src.getName());
                final boolean _not_1 = !_shouldSkip;
                if (_not_1) {
                    final File targetChild = new File(target, name);
                    final boolean _isDirectory = src.isDirectory();
                    if (_isDirectory) {
                        FileUtils.copyDirectory(src, targetChild);
                    } else {
                        FileUtils.copy(src, targetChild);
                    }
                }
            }
        }
    }

    private static boolean shouldSkip(final String name) {
        boolean _switchResult = false;
        if (name != null) {
            switch (name) {
            case "CVS":
            case ".svn":
            case ".git":
                _switchResult = true;
                break;
            default:
                _switchResult = false;
                break;
            }
        } else {
            _switchResult = false;
        }
        return _switchResult;
    }
}
