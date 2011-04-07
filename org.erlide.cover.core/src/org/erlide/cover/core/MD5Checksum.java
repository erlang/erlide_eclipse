package org.erlide.cover.core;

import java.io.File;
import java.io.FileInputStream;
import java.security.MessageDigest;

/**
 * Provides a way to calculate md5 checksum to a file
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class MD5Checksum {

    public static String getMD5(final File file) throws Exception {

        final FileInputStream f = new FileInputStream(file);

        final byte[] buffer = new byte[1024];
        final MessageDigest digest = MessageDigest.getInstance("MD5");

        int numRead;
        while ((numRead = f.read(buffer)) != -1) {
            digest.update(buffer, 0, numRead);
        }
        f.close();
        return new String(digest.digest());
    }

}
