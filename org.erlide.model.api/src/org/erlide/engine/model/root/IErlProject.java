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
package org.erlide.engine.model.root;

import java.util.Collection;

import org.eclipse.core.resources.IProject;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IOpenable;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.builder.BuilderProperties;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;

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
 *
 * @noimplement This interface is not intended to be implemented by clients. An
 *              instance of one of these handles can be created via
 *              <code>ErlangCore.create(project)</code>.
 *              </p>
 *
 * @see ErlangCore#createRoot(org.eclipse.core.resources.IProject)
 */
public interface IErlProject extends IParent, IErlElement, IOpenable {

    Collection<IErlModule> getModules() throws ErlModelException;

    Collection<IErlModule> getIncludes() throws ErlModelException;

    Collection<IErlModule> getModulesAndIncludes() throws ErlModelException;

    Collection<IErlModule> getExternalModules() throws ErlModelException;

    Collection<IErlModule> getExternalIncludes() throws ErlModelException;

    RuntimeInfo getRuntimeInfo();

    RuntimeVersion getRuntimeVersion();

    /**
     * Returns the project's current configuration.
     * <p>
     * Value must not be cached! It can be changed in the background when config
     * file or preferences are edited.
     * </p>
     */
    ErlangProjectProperties getProperties();

    void setProperties(ErlangProjectProperties properties);

    Collection<IErlProject> getReferencedProjects() throws ErlModelException;

    IErlModule getModule(String name) throws ErlModelException;

    IProject getWorkspaceProject();

    ProjectConfigType getConfigType();

    void setConfigType(ProjectConfigType config);

    void storeAllProperties();

    void setBuilderProperties(BuilderProperties props);

    BuilderProperties getBuilderProperties();

}
