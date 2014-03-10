/*******************************************************************************
 * Copyright (c) 2009-2013 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend.api;

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

    Collection<String> getEbinDirs(CodeContext context);

    Collection<Pair<String, String>> getInits();

    RuntimeVersion getVersion();

}
