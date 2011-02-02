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
package org.erlide.core.erlang.util;

import java.io.File;
import java.util.Collection;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.content.IContentType;
import org.eclipse.core.runtime.content.IContentTypeManager;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule.ModuleKind;
import org.erlide.core.erlang.IOldErlangProjectProperties;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public final class ErlideUtil {

    public static boolean isAccessible(final Backend backend,
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

    public static boolean isDeveloper() {
        final String dev = System.getProperty("erlide.devel");
        return dev != null && "true".equals(dev);
    }

    public static boolean isTest() {
        final String test = System.getProperty("erlide.test");
        return test != null && "true".equals(test);
    }

    public static boolean isClearCacheAvailable() {
        final String test = System.getProperty("erlide.clearCacheAvailable");
        return test != null && "true".equals(test);
    }

    private static boolean isEriUserCached = false;
    private static boolean isEricssonUser;

    public static boolean isEricssonUser() {
        if (!isEriUserCached) {
            final String dev = System.getProperty("erlide.ericsson.user");
            isEricssonUser = "true".equals(dev);
            isEriUserCached = true;
        }
        return isEricssonUser;
    }

    public static boolean isModuleExtensionx(final String ext) {
        return extensionToModuleKind(ext) != ModuleKind.BAD;
    }

    public static ModuleKind extensionToModuleKind(final String ext) {
        if (ext == null) {
            return ModuleKind.BAD;
        }
        if (ext.equalsIgnoreCase("hrl")) {
            return ModuleKind.HRL;
        }
        if (ext.equalsIgnoreCase("erl")) {
            return ModuleKind.ERL;
        }
        if (ext.equalsIgnoreCase("yrl")) {
            return ModuleKind.YRL;
        }
        return ModuleKind.BAD;
    }

    public static ModuleKind nameToModuleKind(final String name) {
        final IPath p = new Path(name);
        return extensionToModuleKind(p.getFileExtension());
    }

    public static boolean hasModuleExtension(final String name) {
        return nameToModuleKind(name) != ModuleKind.BAD || true;
    }

    public static boolean hasExtension(final String name) {
        final int i = name.lastIndexOf('.');
        return i != -1;
    }

    public static String withoutExtension(final String name) {
        final int i = name.lastIndexOf('.');
        if (i == -1) {
            return name;
        }
        return name.substring(0, i);
    }

    public static boolean hasErlExtension(final String name) {
        return nameToModuleKind(name) == ModuleKind.ERL;
    }

    public static boolean hasErlideExternalExtension(final String name) {
        final IPath path = new Path(name);
        final String fileExtension = path.getFileExtension();
        return fileExtension != null && fileExtension.equals(".erlidex");
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

    public static boolean isOnSourcePathOrParentToFolderOnSourcePath(
            final IFolder folder) {
        final IProject project = folder.getProject();
        final IPath folderPath = folder.getFullPath();
        final IOldErlangProjectProperties prefs = ErlangCore
                .getProjectProperties(project);
        final Collection<IPath> sourcePaths = prefs.getSourceDirs();
        for (final IPath p : sourcePaths) {
            final IPath path = project.getFolder(p).getFullPath();
            if (folderPath.isPrefixOf(path)) {
                return true;
            }
        }
        return false;
    }

    public static String basenameWithoutExtension(final String m) {
        final IPath p = new Path(m);
        return withoutExtension(p.lastSegment());
    }

    public static boolean isOnWindows() {
        return System.getProperty("os.name").toLowerCase().contains("windows");
    }

    private ErlideUtil() {
    }

    public static boolean isErlangFileContentFileName(final String fileName) {
        final IContentTypeManager contentTypeManager = Platform
                .getContentTypeManager();
        final IContentType[] contentTypes = contentTypeManager
                .findContentTypesFor(fileName);
        for (final IContentType contentType : contentTypes) {
            if (contentType.getId().equals("org.erlide.core.content.erlang")) {
                return true;
            }
        }
        return false;
    }
}
