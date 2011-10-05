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

import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.content.IContentType;
import org.eclipse.core.runtime.content.IContentTypeManager;
import org.erlide.jinterface.ErlLogger;

import com.google.common.collect.Lists;

public class CommonUtils {

    private static final List<String> EMPTY_LIST = Lists.newArrayList();

    private static Boolean fgCacheIsEricssonUser;

    public static boolean isDeveloper() {
        final String dev = System.getProperty("erlide.devel");
        return dev != null && Boolean.parseBoolean(dev);
    }

    public static boolean isClearCacheAvailable() {
        final String test = System.getProperty("erlide.clearCacheAvailable");
        return test != null && Boolean.parseBoolean(test);
    }

    public static boolean isEricssonUser() {
        if (fgCacheIsEricssonUser == null) {
            final String dev = System.getProperty("erlide.ericsson.user");
            fgCacheIsEricssonUser = Boolean.valueOf(dev);
        }
        return fgCacheIsEricssonUser.booleanValue();
    }

    public static boolean isTest() {
        final String test = System.getProperty("erlide.test");
        return test != null && Boolean.parseBoolean(test);
    }

    public static boolean isOnWindows() {
        return System.getProperty("os.name").toLowerCase().contains("windows");
    }

    private CommonUtils() {
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
        if ("true".equalsIgnoreCase(globalTraceValue)
                && "true".equalsIgnoreCase(value)) {
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

    public static String[] unpackArray(final String str, final String sep) {
        return CommonUtils.unpackList(str, sep).toArray(new String[0]);
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

}
