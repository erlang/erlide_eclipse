/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *     Jakob C
 *******************************************************************************/
package org.erlide.core.model.util;

import java.io.File;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.erlide.backend.IBackend;
import org.erlide.core.ErlangCore;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.utils.Util;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public final class ErlideUtil {

    public static boolean isAccessible(final IBackend backend,
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
        } catch (final RpcException e) {
        } finally {
            if (f != null) {
                f.delete();
            }
        }
        return false;
    }

    private static Boolean fgCacheNoModelCache = null;

    public static boolean isCacheDisabled() {
        if (fgCacheNoModelCache == null) {
            final String test = System.getProperty("erlide.noModelCache");
            fgCacheNoModelCache = Boolean.valueOf(test);
        }
        return fgCacheNoModelCache.booleanValue();
    }

    /**
     * Returns true if the given project is accessible and it has a Erlang
     * nature, otherwise false.
     * 
     * @param project
     *            IProject
     * @return boolean
     */
    public static boolean hasErlangNature(final IProject project) {
        if (project != null) {
            try {
                return project.hasNature(ErlangCore.NATURE_ID);
            } catch (final CoreException e) {
                // project does not exist or is not open
            }
        }
        return false;
    }

    private ErlideUtil() {
    }
}
