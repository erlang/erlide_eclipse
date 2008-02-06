package org.erlide.core.builder;

import org.erlide.core.ErlangPlugin;
import org.eclipse.core.runtime.Platform;

/**
 * Created by IntelliJ IDEA.
 * User: Vlad
 * Date: 2008-jan-30
 * Time: 21:25:50
 * To change this template use File | Settings | File Templates.
 */
public class BuilderUtils {
    public static boolean isDebugging() {
        return ErlangPlugin.getDefault().isDebugging()
                && Platform.getDebugOption("org.erlide.core/debug/builder")
                        .equals("true");
    }
}
