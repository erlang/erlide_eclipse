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

import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
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
 * files. A project that references packages in another project can access the
 * packages by including the required project in a code path entry. The Erlang
 * model will present the source elements in the required project; when
 * building, the compiler will use the corresponding generated class files from
 * the required project's output location(s). The code path format is a sequence
 * of code path entries describing the location and contents of applications.
 * </p>
 * Erlang project elements need to be opened before they can be navigated or
 * manipulated. The children of a Erlang project are the package fragment roots
 * that are defined by the classpath and contained in this project (in other
 * words, it does not include package fragment roots for other projects).
 * </p>
 * <p>
 * This interface is not intended to be implemented by clients. An instance of
 * one of these handles can be created via
 * <code>ErlangCore.create(project)</code>.
 * </p>
 * 
 * @see ErlangCore#create(org.eclipse.core.resources.IProject)
 */
public interface IErlProject extends IParent, IErlElement, IOpenable {

	/**
	 * Returns the <code>IErlElement</code> corresponding to the given
	 * classpath-relative path, or <code>null</code> if no such
	 * <code>IErlElement</code> is found. The result is one of an
	 * <code>IErlModule</code>, <code>IClassFile</code>, or
	 * <code>IPackageFragment</code>.
	 * <p>
	 * When looking for a package fragment, there might be several potential
	 * matches; only one of them is returned.
	 * 
	 * <p>
	 * For example, the path "erl/lang/mod.erl", would result in the
	 * <code>IErlModule</code> or <code>IBeamFile</code> corresponding to
	 * "erl.lang.mod".
	 * 
	 * @param path
	 *            the given classpath-relative path
	 * @throws ErlModelException
	 *             if the given path is <code>null</code> or absolute
	 * @return the <code>IErlElement</code> corresponding to the given
	 *         classpath-relative path, or <code>null</code> if no such
	 *         <code>IErlElement</code> is found
	 */
	public IErlElement findElement(IPath path) throws ErlModelException;

	/**
	 * Returns an array of non-Erlang resources directly contained in this
	 * project. It does not transitively answer non-Erlang resources contained
	 * in folders; these would have to be explicitly iterated over.
	 * <p>
	 * Non-Erlang resources includes other files and folders located in the
	 * project not accounted for by any of it source or binary package fragment
	 * roots. If the project is a source folder itself, resources excluded from
	 * the corresponding source classpath entry by one or more exclusion
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
	IResource[] getNonErlangResources() throws ErlModelException;

	/**
	 * Helper method for returning one option value only. Equivalent to
	 * <code>(String)this.getOptions(inheritErlangCoreOptions).get(optionName)</code>
	 * Note that it may answer <code>null</code> if this option does not
	 * exist, or if there is no custom value for it.
	 * <p>
	 * For a complete description of the configurable options, see
	 * <code>ErlangCore#getDefaultOptions</code>.
	 * </p>
	 * 
	 * @param optionName
	 *            the name of an option
	 * @param inheritErlangCoreOptions -
	 *            boolean indicating whether ErlangCore options should be
	 *            inherited as well
	 * @return the String value of a given option
	 * @see ErlangCore#getDefaultOptions()
	 * 
	 */
	String getOption(String optionName, boolean inheritErlangCoreOptions);

	/**
	 * Returns the table of the current custom options for this project.
	 * Projects remember their custom options, in other words, only the options
	 * different from the the ErlangCore global options for the workspace. A
	 * boolean argument allows to directly merge the project options with global
	 * ones from <code>ErlangCore</code>.
	 * <p>
	 * For a complete description of the configurable options, see
	 * <code>ErlangCore#getDefaultOptions</code>.
	 * </p>
	 * 
	 * @param inheritErlangCoreOptions -
	 *            boolean indicating whether ErlangCore options should be
	 *            inherited as well
	 * @return table of current settings of all options (key type:
	 *         <code>String</code>; value type: <code>String</code>)
	 * @see ErlangCore#getDefaultOptions()
	 * 
	 */
	Map<String,String> getOptions(boolean inheritErlangCoreOptions);

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
	 * Returns the <code>IProject</code> on which this
	 * <code>IErlProject</code> was created. This is handle-only method.
	 * 
	 * @return the <code>IProject</code> on which this
	 *         <code>IErlProject</code> was created
	 */
	IProject getProject();

	/**
	 * Returns the names of the projects that are directly required by this
	 * project. A project is required if it is in its classpath.
	 * <p>
	 * The project names are returned in the order they appear on the classpath.
	 * 
	 * @return the names of the projects that are directly required by this
	 *         project in classpath order
	 * @throws ErlModelException
	 *             if this element does not exist or if an exception occurs
	 *             while accessing its corresponding resource
	 */
	String[] getRequiredProjectNames() throws ErlModelException;

	/**
	 * Returns whether this project has been built at least once and thus
	 * whether it has a build state.
	 * 
	 * @return true if this project has been built at least once, false
	 *         otherwise
	 */
	boolean hasBuildState();

	/**
	 * Helper method for setting one option value only. Equivalent to
	 * <code>Map options = this.getOptions(false); map.put(optionName, optionValue); this.setOptions(map)</code>
	 * <p>
	 * For a complete description of the configurable options, see
	 * <code>ErlangCore#getDefaultOptions</code>.
	 * </p>
	 * 
	 * @param optionName
	 *            the name of an option
	 * @param optionValue
	 *            the value of the option to set
	 * @see ErlangCore#getDefaultOptions()
	 * 
	 */
	void setOption(String optionName, String optionValue);

	/**
	 * Sets the project custom options. All and only the options explicitly
	 * included in the given table are remembered; all previous option settings
	 * are forgotten, including ones not explicitly mentioned.
	 * <p>
	 * For a complete description of the configurable options, see
	 * <code>ErlangCore#getDefaultOptions</code>.
	 * </p>
	 * 
	 * @param newOptions
	 *            the new options (key type: <code>String</code>; value type:
	 *            <code>String</code>), or <code>null</code> to flush all
	 *            custom options (clients will automatically get the global
	 *            ErlangCore options).
	 * @see ErlangCore#getDefaultOptions()
	 * 
	 */
	void setOptions(Map newOptions);

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
	 *             <li>The path is not an absolute path (<code>RELATIVE_PATH</code>)
	 *             <li>The path is nested inside a package fragment root of
	 *             this project ( <code>INVALID_PATH</code>)
	 *             <li>The output location is being modified during resource
	 *             change event notification (CORE_EXCEPTION)
	 *             </ul>
	 * @see #getOutputLocation()
	 * @see IClasspathEntry#getOutputLocation()
	 */
	void setOutputLocation(IPath path, IProgressMonitor monitor)
			throws ErlModelException;

	IErlModule[] getModules() throws ErlModelException;

	IErlModule getModule(String name) throws ErlModelException;

}
