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
package org.erlide.core.model.erlang.util;

import java.io.File;
import java.util.Collection;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.erlide.core.ErlangCore;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.backend.BackendException;
import org.erlide.core.backend.RpcCallSite;
import org.erlide.core.common.Util;
import org.erlide.core.model.erlang.IErlProject;
import org.erlide.jinterface.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public final class ErlideUtil {

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

    private static Boolean fgCacheNoModelCache = null;

    public static boolean isNoModelCache() {
        if (fgCacheNoModelCache == null) {
            final String test = System.getProperty("erlide.noModelCache");
            fgCacheNoModelCache = Boolean.valueOf("true".equals(test));
        }
        return fgCacheNoModelCache.booleanValue();
    }

    public static boolean isOnSourcePathOrParentToFolderOnSourcePath(
            final IFolder folder) {
        final IProject project = folder.getProject();
        final IPath folderPath = folder.getFullPath();
        final IErlProject erlProject = ErlangCore.getModel().getErlangProject(
                project);
        final Collection<IPath> sourcePaths = erlProject.getSourceDirs();
        for (final IPath p : sourcePaths) {
            final IPath path = project.getFolder(p).getFullPath();
            if (folderPath.isPrefixOf(path)) {
                return true;
            }
        }
        return false;
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
                return project.hasNature(ErlangPlugin.NATURE_ID);
            } catch (final CoreException e) {
                // project does not exist or is not open
            }
        }
        return false;
    }

    private ErlideUtil() {
    }
}
