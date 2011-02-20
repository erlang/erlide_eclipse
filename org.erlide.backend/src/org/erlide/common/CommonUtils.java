/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.common;

import java.io.File;

import org.erlide.backend.BackendException;
import org.erlide.backend.rpc.RpcCallSite;
import org.erlide.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class CommonUtils {

    private static Boolean fgCacheIsEricssonUser;

    public static boolean isDeveloper() {
        final String dev = System.getProperty("erlide.devel");
        return dev != null && "true".equals(dev);
    }

    public static boolean isClearCacheAvailable() {
        final String test = System.getProperty("erlide.clearCacheAvailable");
        return test != null && "true".equals(test);
    }

    public static boolean isEricssonUser() {
        if (fgCacheIsEricssonUser == null) {
            final String dev = System.getProperty("erlide.ericsson.user");
            fgCacheIsEricssonUser = new Boolean("true".equals(dev));
        }
        return fgCacheIsEricssonUser.booleanValue();
    }

    public static boolean isTest() {
        final String test = System.getProperty("erlide.test");
        return test != null && "true".equals(test);
    }

    public static boolean isOnWindows() {
        return System.getProperty("os.name").toLowerCase().contains("windows");
    }

    private CommonUtils() {
    }

    public static boolean isAccessible(final RpcCallSite backend,
            final String localDir) {
        File f = null;
        try {
            f = new File(localDir);
            final OtpErlangObject r = backend.call("file", "read_file_info",
                    "s", localDir);
            if (Util.isOk(r)) {
                final OtpErlangTuple result = (OtpErlangTuple) r;
                final OtpErlangTuple info = (OtpErlangTuple) result
                        .elementAt(1);
                final String access = info.elementAt(3).toString();
                final int mode = ((OtpErlangLong) info.elementAt(7)).intValue();
                return ("read".equals(access) || "read_write".equals(access))
                        && (mode & 4) == 4;
            }

        } catch (final OtpErlangRangeException e) {
            ErlLogger.error(e);
        } catch (final BackendException e) {
            ErlLogger.error(e);
        } finally {
            if (f != null) {
                f.delete();
            }
        }
        return false;
    }
}
