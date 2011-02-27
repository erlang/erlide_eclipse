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
package org.erlide.core.common;

import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.content.IContentType;
import org.eclipse.core.runtime.content.IContentTypeManager;
import org.erlide.core.backend.BackendException;
import org.erlide.core.backend.RpcCallSite;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class CommonUtils {

    private static final List<String> EMPTY_LIST = Lists.newArrayList();

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
            fgCacheIsEricssonUser = Boolean.valueOf("true".equals(dev));
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

    public static boolean isTracing(final String traceOption) {
        if (!Platform.inDebugMode()) {
            return false;
        }
        final String globalTraceValue = Platform
                .getDebugOption(ErlLogger.ERLIDE_GLOBAL_TRACE_OPTION);
        final String value = Platform
                .getDebugOption(ErlLogger.ERLIDE_GLOBAL_TRACE_OPTION + "/"
                        + traceOption);
        if (null != globalTraceValue && globalTraceValue.equals("true")
                && null != value && value.equals("true")) {
            return true;
        }
        return false;
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

    public static boolean hasErlideExternalExtension(final String name) {
        final IPath path = new Path(name);
        final String fileExtension = path.getFileExtension();
        return fileExtension != null && fileExtension.equals(".erlidex");
    }

    public static String basenameWithoutExtension(final String m) {
        final IPath p = new Path(m);
        return withoutExtension(p.lastSegment());
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

    public static String packList(final Iterable<String> strs, final String sep) {
        final StringBuilder result = new StringBuilder();
        for (final String s : strs) {
            result.append(s).append(sep);
        }
        return result.length() > 0 ? result.substring(0,
                result.length() - sep.length()) : "";
    }

    public static String packArray(final String[] strs, final String sep) {
        final StringBuilder result = new StringBuilder();
        for (final String s : strs) {
            result.append(s).append(sep);
        }
        return result.length() > 0 ? result.substring(0,
                result.length() - sep.length()) : "";
    }

    public static IPath[] unpackArray(final String str, final String sep) {
        return CommonUtils.unpackList(str, sep).toArray(new IPath[0]);
    }

    public static List<String> unpackList(final String string, final String sep) {
        if (string.length() == 0) {
            return EMPTY_LIST;
        }
        final String[] v = string.split(sep);
        final List<String> result = Lists.newArrayListWithCapacity(v.length);
        for (final String s : v) {
            result.add(s);
        }
        return result;
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

    public static boolean isModuleExtensionx(final String ext) {
        return CommonUtils.extensionToModuleKind(ext) != ModuleKind.BAD;
    }

    public static boolean hasModuleExtension(final String name) {
        return CommonUtils.nameToModuleKind(name) != ModuleKind.BAD;
    }

    public static boolean hasErlExtension(final String name) {
        return CommonUtils.nameToModuleKind(name) == ModuleKind.ERL;
    }
}
