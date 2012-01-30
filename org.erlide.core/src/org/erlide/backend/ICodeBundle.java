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
package org.erlide.backend;

import java.util.Collection;
import java.util.Map;

import org.erlide.utils.Tuple;
import org.osgi.framework.Bundle;

public interface ICodeBundle {

    public static enum CodeContext {
        ANY, COMMON, BUILDER, IDE, DEBUGGER
    }

    Bundle getBundle();

    Collection<String> getEbinDirs();

    Map<String, CodeContext> getPaths();

    Collection<Tuple<String, String>> getInits();

}
