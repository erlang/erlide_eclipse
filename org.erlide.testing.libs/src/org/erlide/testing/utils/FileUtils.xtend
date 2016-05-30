package org.erlide.testing.utils

import java.io.File
import java.io.FileOutputStream

class FileUtils {

    def static byte[] read(File file) {
        val len = file.length as int
        val byte[] bytes = newByteArrayOfSize(len)
        val stream = new java.io.FileInputStream(file)
        var bytesRead = 0
        var lastReadSize = 0

        try {
            while ((lastReadSize != -1) && (bytesRead != len)) {
                lastReadSize = stream.read(bytes, bytesRead, len - bytesRead)
                bytesRead += lastReadSize
            }
        } finally {
            stream.close()
        }
        bytes
    }

    /**
     * Copy file from src (path to the original file) to dest (path to the destination file).
     */
    def static void copy(File src, File dest) {
        val srcBytes = read(src)

        val out = new FileOutputStream(dest)
        try {
            out.write(srcBytes)
        } finally {
            out.close()
        }
    }

    /**
     * Copy the given source directory (and all its contents) to the given target directory.
     */
    def static void copyDirectory(File source, File target) {
        if (!target.exists)
            target.mkdirs()

        val files = source.listFiles();
        if (files === null)
            return;

        for (src : files) {
            val name = src.getName;
            if (!shouldSkip(src.getName)) {
                val targetChild = new File(target, name)
                if (src.isDirectory)
                    copyDirectory(src, targetChild)
                else
                    copy(src, targetChild)
            }
        }
    }

    def static private boolean shouldSkip(String name) {
        switch (name) {
            case "CVS",
            case ".svn",
            case ".git":
                true
            default:
                false
        }
    }

}
