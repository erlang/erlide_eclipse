/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.model.root;

import java.util.Collection;

import org.erlide.core.ErlangCore;
import org.erlide.core.model.erlang.IErlModule;

/**
 * An Erlang folder is a collection of colder with erlang files. It knows
 * whether it is on the compile path
 * 
 * @see ErlangCore#createRoot(org.eclipse.core.resources.IFolder)
 */
public interface IErlFolder extends IParent, IErlElement, IOpenable {

    /**
     * @return all modules in this folder and its sub-folders
     * @throws ErlModelException
     */
    Collection<IErlModule> getModules() throws ErlModelException;

    boolean isOnSourcePath();

    boolean isOnIncludePath();

    boolean isSourcePathParent();

    IErlModule findModule(String moduleName, String modulePath)
            throws ErlModelException;

    IErlModule findInclude(String includeName, String includePath)
            throws ErlModelException;
}
