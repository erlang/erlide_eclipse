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
package org.erlide.core.erlang;

import java.util.Collection;

import org.eclipse.core.resources.IResource;

/**
 * An Erlang folder is a collection of colder with erlang files. It knows
 * whether it is on the compile path
 * 
 * @see ErlangCore#createRoot(org.eclipse.core.resources.IFolder)
 */
public interface IErlFolder extends IParent, IErlElement, IOpenable {

    /**
     * Returns an array of non-Erlang resources directly contained in this
     * project. It does not transitively answer non-Erlang resources contained
     * in folders; these would have to be explicitly iterated over.
     * <p>
     * Non-Erlang resources includes other files and folders located in the
     * project not accounted for by any of it source or binary package fragment
     * roots. If the project is a source folder itself, resources excluded from
     * the corresponding source class-path entry by one or more exclusion
     * patterns are considered non-Erlang resources and will appear in the
     * result (possibly in a folder)
     * </p>
     * 
     * @return an array of non-Erlang resources (<code>IFile</code> s and/or
     *         <code>IFolder</code>s) directly contained in this project
     * @throws ErlModelException
     *             if this element does not exist or if an exception occurs
     *             while accessing its corresponding resource
     */
    Collection<IResource> getNonErlangResources() throws ErlModelException;

    /**
     * @return all modules in this folder and its sub-folders
     * @throws ErlModelException
     */
    Collection<IErlModule> getModules() throws ErlModelException;

    IErlModule getModule(String name) throws ErlModelException;

    IErlModule getModuleExt(String name);

    boolean isOnSourcePath();

    boolean isSourcePathParent();
}
