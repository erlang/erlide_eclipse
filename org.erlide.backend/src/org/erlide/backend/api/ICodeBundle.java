/*******************************************************************************
 * Copyright (c) 2009-2013 Vlad Dumitrescu and others. All rights reserved. This program
 * and the accompanying materials are made available under the terms of the Eclipse Public
 * License v1.0 which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend.api;

import java.net.URL;
import java.util.Collection;

import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.osgi.framework.Bundle;

public interface ICodeBundle {

    // common context is always included in the others
    public static enum CodeContext {
        COMMON, IDE, DEBUGGER
    }

    Bundle getBundle();

    /**
     * List of bundle-relative paths of ebin entries.
     * 
     * @param context
     * @return
     */
    Collection<String> getEbinPaths(CodeContext context);

    /**
     * List of absolute paths of local directories corresponding to ebin entries.
     *
     * @param context
     * @return
     */
    Collection<String> getEbinDirs(CodeContext context);

    /**
     * List of URLs for all beam files within ebin entries.
     * 
     * @param context
     * @return
     */
    Collection<URL> getEbinBeamURLs(CodeContext context);

    Collection<Pair<String, String>> getInits();

    RuntimeVersion getVersion();

}
