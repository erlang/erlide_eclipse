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

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * An Erlang project represents a view of a project resource in terms of Erlang
 * elements such as applications, modules, attributes and functions. A project
 * may contain several applications, which contain modules. An application
 * corresponds to an underlying folder.
 * <p>
 * Each Erlang project has a code path, defining which folders contain source
 * code and where required libraries are located. Each Erlang project also has
 * an output location, defining where the builder writes <code>.beam</code>
 * files. A project that references modules in another project can access the
 * modules by including the required project in a code path entry. The Erlang
 * model will present the source elements in the required project; when
 * building, the compiler will use the corresponding generated beam files from
 * the required project's output location(s). The code path format is a sequence
 * of code path entries describing the location and contents of applications.
 * </p>
 * Erlang project elements need to be opened before they can be navigated or
 * manipulated. The children of a Erlang project are the package fragment roots
 * that are defined by the classpath and contained in this project (in other
 * words, it does not include package fragment roots for other projects). </p>
 * <p>
 * This interface is not intended to be implemented by clients. An instance of
 * one of these handles can be created via
 * <code>ErlangCore.create(project)</code>.
 * </p>
 * 
 * @see ErlangCore#createRoot(org.eclipse.core.resources.IProject)
 */
public interface IErlProject extends IErlFolder {

    /**
     * Returns the <code>IProject</code> on which this <code>IErlProject</code>
     * was created. This is handle-only method.
     * 
     * @return the <code>IProject</code> on which this <code>IErlProject</code>
     *         was created
     */
    IProject getProject();

    /**
     * Returns the default output location for this project as a workspace-
     * relative absolute path.
     * <p>
     * The default output location is where class files are ordinarily generated
     * (and resource files, copied). Each source classpath entry can also
     * specify an output location for the generated class files (and copied
     * resource files) corresponding to compilation units under that source
     * folder. This makes it possible to arrange generated class files for
     * different source folders in different output folders, and not necessarily
     * the default output folder. This means that the generated class files for
     * the project may end up scattered across several folders, rather than all
     * in the default output folder (which is more standard).
     * </p>
     * 
     * @return the workspace-relative absolute path of the default output folder
     * @throws ErlModelException
     *             if this element does not exist
     * @see #setOutputLocation(org.eclipse.core.runtime.IPath, IProgressMonitor)
     */
    IPath getOutputLocation() throws ErlModelException;

    /**
     * Sets the default output location of this project to the location
     * described by the given workspace-relative absolute path.
     * 
     * @param path
     *            the workspace-relative absolute path of the default output
     *            folder
     * @param monitor
     *            the progress monitor
     * 
     * @throws ErlModelException
     *             if the classpath could not be set. Reasons include:
     *             <ul>
     *             <li>This Erlang element does not exist
     *             (ELEMENT_DOES_NOT_EXIST)</li>
     *             <li>The path refers to a location not contained in this
     *             project ( <code>PATH_OUTSIDE_PROJECT</code>)
     *             <li>The path is not an absolute path (
     *             <code>RELATIVE_PATH</code>)
     *             <li>The path is nested inside a package fragment root of this
     *             project ( <code>INVALID_PATH</code>)
     *             <li>The output location is being modified during resource
     *             change event notification (CORE_EXCEPTION)
     *             </ul>
     * @see #getOutputLocation()
     * @see IClasspathEntry#getOutputLocation()
     */
    void setOutputLocation(IPath path, IProgressMonitor monitor)
            throws ErlModelException;

    /**
     * Returns the names of the projects that are directly required by this
     * project. A project is required if it is in its classpath.
     * <p>
     * The project names are returned in the order they appear on the classpath.
     * 
     * @return the names of the projects that are directly required by this
     *         project in classpath order
     * @throws CoreException
     */
    Collection<String> getRequiredProjectNames() throws CoreException;

    Collection<IErlModule> getModulesAndHeaders() throws ErlModelException;

    IOldErlangProjectProperties getProperties();

    Collection<IErlModule> getExternalModules() throws CoreException;

}
