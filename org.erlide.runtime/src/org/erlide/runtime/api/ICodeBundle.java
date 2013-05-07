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
package org.erlide.runtime.api;

import java.util.Collection;
import java.util.Map;

import org.eclipse.xtext.xbase.lib.Pair;

public interface ICodeBundle {

    public static enum CodeContext {
        ANY, COMMON, BUILDER, IDE, DEBUGGER
    }

    String getBundleName();

    Collection<String> getEbinDirs();

    Map<String, CodeContext> getPaths();

    Collection<Pair<String, String>> getInits();

}
