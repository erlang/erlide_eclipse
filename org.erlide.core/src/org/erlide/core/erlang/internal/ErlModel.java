/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.core.erlang.internal;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IPathVariableChangeEvent;
import org.eclipse.core.resources.IPathVariableChangeListener;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlElementVisitor;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModelChangeListener;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlangFirstThat;
import org.erlide.core.erlang.IOldErlangProjectProperties;
import org.erlide.core.erlang.IOpenable;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.erlang.util.PluginUtils;
import org.erlide.jinterface.backend.util.PreferencesUtils;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

import erlang.FunctionRef;

/**
 * Implementation of
 * <code>IErlModel<code>. The Erlang Model maintains a cache of
 * active <code>IErlProject</code>s in a workspace. A Erlang Model is specific
 * to a workspace. To retrieve a workspace's model, use the
 * <code>#getErlangModel(IWorkspace)</code> method.
 * 
 * @see IErlModel
 */
public class ErlModel extends Openable implements IErlModel {

    private final ArrayList<IErlModelChangeListener> fListeners = new ArrayList<IErlModelChangeListener>(
            5);

    private final IPathVariableChangeListener fPathVariableChangeListener;

    /**
     * Constructs a new Erlang Model on the given workspace. Note that only one
     * instance of ErlModel handle should ever be created. One should only
     * indirect through ErlModelManager#getErlangModel() to get access to it.
     * 
     * @exception Error
     *                if called more than once
     */
    ErlModel() {
        super(null, ""); //$NON-NLS-1$
        final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
                .getPathVariableManager();
        fPathVariableChangeListener = new IPathVariableChangeListener() {
            public void pathVariableChanged(final IPathVariableChangeEvent event) {
                fCachedPathVars = null;
            }
        };
        pvm.addChangeListener(fPathVariableChangeListener);
    }

    @Override
    protected boolean buildStructure(final IProgressMonitor pm) {
        // determine my children
        final IProject[] projects = ResourcesPlugin.getWorkspace().getRoot()
                .getProjects();
        for (final IProject project : projects) {
            if (ErlideUtil.hasErlangNature(project)) {
                if (getErlangProject(project.getName()) == null) {
                    addChild(makeErlangProject(project));
                }
            }
        }

        return true;
    }

    /**
     * @see IErlModel
     */
    public void copy(final IErlElement[] elements,
            final IErlElement[] containers, final IErlElement[] siblings,
            final String[] renamings, final boolean force,
            final IProgressMonitor monitor) throws ErlModelException {
        // if (elements != null && elements.length > 0 && elements[0] != null
        // && elements[0].getElementType() < IErlElement.TYPE)
        // {
        // runOperation(new CopyResourceElementsOperation(elements, containers,
        // force),
        // elements,
        // siblings, renamings, monitor);
        // } else
        // {
        // runOperation(new CopyElementsOperation(elements, containers, force),
        // elements,
        // siblings, renamings, monitor);
        // }
    }

    /**
     * @see IErlModel
     */
    public void delete(final IErlElement[] elements, final boolean force,
            final IProgressMonitor monitor) throws ErlModelException {
        // if (elements != null && elements.length > 0 && elements[0] != null
        // && elements[0].getElementType() < IErlElement.TYPE)
        // {
        // new DeleteResourceElementsOperation(elements, force)
        // .runOperation(monitor);
        // }
        // else
        // {
        // new DeleteElementsOperation(elements, force).runOperation(monitor);
        // }
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof ErlModel)) {
            return false;
        }
        return super.equals(o);
    }

    /**
     * @see IErlElement
     */
    public Kind getKind() {
        return Kind.MODEL;
    }

    /**
     * @see ErlElement#getHandleMemento()
     */
    public String getHandleMemento() {
        return getName();
    }

    /**
     * Returns the <code>char</code> that marks the start of this handles
     * contribution to a memento.
     */
    protected char getHandleMementoDelimiter() {
        Assert.isTrue(false, "Should not be called"); //$NON-NLS-1$
        return 0;
    }

    /**
     * @see IErlModel
     */
    public IErlProject getErlangProject(final String projectName) {
        final IErlElement e = getChildNamed(projectName);
        if (e != null) {
            return (IErlProject) e;
        }
        return makeErlangProject(getWorkspace().getRoot().getProject(
                projectName));
    }

    public IErlProject makeErlangProject(final IProject project) {
        final IErlProject ep = new ErlProject(project, this);
        ErlLogger.debug("ep " + ep);
        addChild(ep);
        return ep;
    }

    /**
     * @see IErlModel
     */
    public Collection<IErlProject> getErlangProjects() throws ErlModelException {
        final Collection<IErlElement> list = getChildrenOfKind(Kind.PROJECT);
        final Collection<IErlProject> result = Lists.newArrayList();
        for (final IErlElement e : list) {
            result.add((IErlProject) e);
        }
        return result;
    }

    /*
     * @see IErlElement
     */
    @Override
    public IResource getResource() {
        return ResourcesPlugin.getWorkspace().getRoot();
    }

    // /**
    // * @see IOpenable
    // */
    // @Override
    // public IResource getUnderlyingResource() {
    // return null;
    // }

    /**
     * Returns the workbench associated with this object.
     */
    public IWorkspace getWorkspace() {
        return ResourcesPlugin.getWorkspace();
    }

    /**
     * @see IErlModel
     */
    public void move(final IErlElement[] elements,
            final IErlElement[] containers, final IErlElement[] siblings,
            final String[] renamings, final boolean force,
            final IProgressMonitor monitor) throws ErlModelException {
        // if (elements != null && elements.length > 0 && elements[0] != null
        // && elements[0].getElementType() < IErlElement.TYPE)
        // {
        // runOperation(new MoveResourceElementsOperation(elements,
        // containers, force), elements, siblings, renamings, monitor);
        // }
        // else
        // {
        // runOperation(
        // new MoveElementsOperation(elements, containers, force),
        // elements, siblings, renamings, monitor);
        // }
    }

    /**
     * @see IErlModel
     */
    public void rename(final IErlElement[] elements,
            final IErlElement[] destinations, final String[] renamings,
            final boolean force, final IProgressMonitor monitor)
            throws ErlModelException {
        // MultiOperation op;
        // if (elements != null && elements.length > 0 && elements[0] != null
        // && elements[0].getElementType() < IErlElement.TYPE)
        // {
        // op = new RenameResourceElementsOperation(elements, destinations,
        // renamings, force);
        // }
        // else
        // {
        // op = new RenameElementsOperation(elements, destinations, renamings,
        // force);
        // }
        //
        // op.runOperation(monitor);
    }

    // /**
    // * Configures and runs the <code>MultiOperation</code>.
    // */
    // protected void runOperation(MultiOperation op, IErlElement[] elements,
    // IErlElement[] siblings, String[] renamings,
    // IProgressMonitor monitor) throws ErlModelException
    // {
    // op.setRenamings(renamings);
    // if (siblings != null)
    // {
    // for (int i = 0; i < elements.length; i++)
    // {
    // op.setInsertBefore(elements[i], siblings[i]);
    // }
    // }
    // op.runOperation(monitor);
    // }

    /**
     * @private Debugging purposes
     */
    @Override
    protected void toStringInfo(final int tab, final StringBuilder buffer,
            final Object info) {
        buffer.append(tabString(tab));
        buffer.append("Erlang Model"); //$NON-NLS-1$
        if (info == null) {
            buffer.append(" (not open)"); //$NON-NLS-1$
        }
    }

    /**
     * Helper method - returns the targeted item (IResource if internal or
     * java.io.File if external), or null if unbound Internal items must be
     * referred to using container relative paths.
     */
    public static Object getTarget(final IContainer container,
            final IPath path, final boolean checkResourceExistence) {

        if (path == null) {
            return null;
        }

        // lookup - inside the container
        if (path.getDevice() == null) { // container relative paths should not
            // contain a device
            // (see http://dev.eclipse.org/bugs/show_bug.cgi?id=18684)
            // (case of a workspace rooted at d:\ )
            final IResource resource = container.findMember(path);
            if (resource != null) {
                if (!checkResourceExistence || resource.exists()) {
                    return resource;
                }
                return null;
            }
        }

        // if path is relative, it cannot be an external path
        // (see http://dev.eclipse.org/bugs/show_bug.cgi?id=22517)
        if (!path.isAbsolute()) {
            return null;
        }

        // lookup - outside the container
        final File externalFile = new File(path.toOSString());
        if (!checkResourceExistence) {
            return externalFile;
        }
        if (externalFile.exists()) {
            return externalFile;
        }
        return null;
    }

    public void notifyChange(final IErlElement element) {
        if (System.getProperty("erlide.model.notify") != null) {
            ErlLogger.debug("^> notifying change of " + element.getName());
            ErlLogger.debug("   caller = " + getStack());
        }
        for (int i = 0; i < fListeners.size(); i++) {
            fListeners.get(i).elementChanged(element);
        }
    }

    private static synchronized String getStack() {
        final StringBuilder result = new StringBuilder();
        final StackTraceElement[] st = Thread.currentThread().getStackTrace();
        for (final StackTraceElement el : st) {
            result.append("      ").append(el.toString()).append("\n");
        }
        return result.toString();
    }

    public void addModelChangeListener(final IErlModelChangeListener listener) {
        if (!fListeners.contains(listener)) {
            fListeners.add(listener);
        }
    }

    public void removeModelChangeListener(final IErlModelChangeListener listener) {
        fListeners.remove(listener);
    }

    public boolean isVisibleInOutline() {
        return false;
    }

    @Override
    protected void closing(final Object info) throws ErlModelException {
        final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
                .getPathVariableManager();
        pvm.removeChangeListener(fPathVariableChangeListener);
    }

    public IErlElement findElement(final IResource rsrc) {
        return findElement(rsrc, false);
    }

    public IErlElement findElement(final IResource rsrc,
            final boolean openElements) {
        if (rsrc == null) {
            return null;
        }
        final IPath path = rsrc.getFullPath();
        IParent p = this;
        for (final String segment : path.segments()) {
            IErlElement c = p.getChildWithResource(rsrc);
            if (c != null) {
                return c;
            }
            c = p.getChildNamed(segment);
            if (c == null) {
                return null;
            }
            if (openElements) {
                if (c instanceof IOpenable) {
                    final IOpenable o = (IOpenable) c;
                    try {
                        o.open(null);
                    } catch (final ErlModelException e) {
                        e.printStackTrace();
                        return null;
                    }
                }
            }
            final IResource resource = c.getResource();
            if (resource != null && resource.equals(rsrc)) {
                return c;
            }
            p = (IParent) c;
        }
        return null;
    }

    public IErlElement innermostThat(final IErlElement el,
            final IErlangFirstThat firstThat) {
        if (el instanceof IParent) {
            final IParent p = (IParent) el;
            try {
                for (final IErlElement child : p.getChildren()) {
                    final IErlElement e2 = innermostThat(child, firstThat);
                    if (e2 != null) {
                        return e2;
                    }
                }
            } catch (final ErlModelException e) {
            }
        }
        if (firstThat.firstThat(el)) {
            return el;
        }
        return null;
    }

    public IErlModule findModule(final IFile file) {
        IErlElement element = findElement(file, false);
        if (element == null) {
            // Ok, if it's not found, we'll try to build it...
            element = findElement(file, true);
        }
        // if (element == null) {
        // final String path = file.getLocation().toPortableString();
        // element = ErlangCore.getModelManager().getModuleFromFile(this,
        // file.getName(), null, path, path);
        // }
        return (IErlModule) element;
    }

    public IErlProject findProject(final IProject project) {
        final IErlElement e = findElement(project);
        if (e == null) {
            return null;
        }
        return (IErlProject) e;
    }

    public IErlModule findModule(final String name) {
        try {
            for (final IErlElement e : getChildren()) {
                if (e instanceof IErlProject) {
                    final IErlProject p = (IErlProject) e;
                    try {
                        final IErlModule m = p.getModule(name);
                        if (m != null) {
                            return m;
                        }
                    } catch (final ErlModelException e1) {
                        e1.printStackTrace();
                    }
                }
            }
        } catch (final ErlModelException e) {
        }
        return null;
    }

    public IErlProject createOtpProject(final IProject project)
            throws CoreException {
        final IPath location = project.getLocation();

        final IErlProject p = getErlangProject(project.getName());

        final IOldErlangProjectProperties props = p.getProperties();

        final IFile file = project.getFile(".");
        if (!file.isLinked()) {
            file.createLink(location, IResource.NONE, null);
        }

        Collection<IPath> dirs;
        dirs = findOtpSourceDirs(new File(location.toString()));
        props.setSourceDirs(dirs);
        dirs = findOtpIncludeDirs(new File(location.toString()));
        props.setIncludeDirs(dirs);
        props.store();
        p.open(null);
        return p;
    }

    private static Collection<IPath> findOtpSourceDirs(final File file) {
        final List<IPath> result = Lists.newArrayList();
        return result;
    }

    private static Collection<IPath> findOtpIncludeDirs(final File file) {
        final List<IPath> result = Lists.newArrayList();
        return result;
    }

    public final IErlProject newProject(final String name, final String path)
            throws ErlModelException {
        final IWorkspace ws = ResourcesPlugin.getWorkspace();
        final IProject project = ws.getRoot().getProject(name);
        try {
            if (!project.exists()) {
                project.create(null);
                project.open(null);
                final IProjectDescription description = project
                        .getDescription();
                description
                        .setNatureIds(new String[] { ErlangPlugin.NATURE_ID });
                description.setName(name);
                project.setDescription(description, null);
            }
            if (!project.isOpen()) {
                project.open(null);
            }
            return makeErlangProject(project);
        } catch (final CoreException e) {
            throw new ErlModelException(e);
        }
    }

    public final void accept(final IErlElement element,
            final IErlElementVisitor visitor, final int flags,
            final IErlElement.Kind leafKind) throws ErlModelException {
        if (element.getKind() == leafKind) {
            visitor.visit(element);
        } else {
            boolean visitChildren = true;
            if ((flags & IErlElement.VISIT_LEAFS_ONLY) == 0) {
                visitChildren = visitor.visit(element);
            }
            if (visitChildren && element instanceof IParent) {
                final IParent parent = (IParent) element;
                for (final IErlElement child : parent.getChildren()) {
                    accept(child, visitor, flags, leafKind);
                }
                // if (parent instanceof IErlProject) {
                // final IErlProject project = (IErlProject) parent;
                // if ((flags & IErlElement.VISIT_REFERENCED) != 0) {
                // final IProject p = project.getProject();
                // try {
                // for (final IProject referenced : p
                // .getReferencedProjects()) {
                // final IErlElement e = findElement(referenced);
                // if (e instanceof IErlProject) {
                // final IErlProject ep = (IErlProject) e;
                // accept(ep, visitor, flags
                // & ~IErlElement.VISIT_REFERENCED,
                // leafKind);
                // }
                // }
                // } catch (final CoreException e) {
                // ErlLogger.warn(e);
                // }
                // }
                // if ((flags & IErlElement.VISIT_EXTERNALS) != 0) {
                // // FIXME how do we do that?
                // }
                // }
            }
        }
    }

    public enum External {
        EXTERNAL_MODULES, EXTERNAL_INCLUDES
    }

    private String getExternal(final IErlProject project,
            final External external) {
        final IPreferencesService service = Platform.getPreferencesService();
        final String key = external == External.EXTERNAL_INCLUDES ? "default_external_includes"
                : "default_external_modules";
        String result = getExternal(project, external, service, key,
                "org.erlide.ui");
        if ("".equals(result) && project != null) {
            result = getExternal(null, external, service, key,
                    ErlangPlugin.PLUGIN_ID);
        }
        return result;
    }

    private String getExternal(final IErlProject project,
            final External external, final IPreferencesService service,
            final String key, final String pluginId) {
        final String s = service.getString(pluginId, key, "", null);
        if (s.length() > 0) {
            ErlLogger.debug("%s: '%s'", key, s);
        }
        final String global = s;
        if (project != null) {
            final IOldErlangProjectProperties prefs = project.getProperties();
            final String projprefs = external == External.EXTERNAL_INCLUDES ? prefs
                    .getExternalIncludesFile() : prefs.getExternalModulesFile();
            return PreferencesUtils
                    .packArray(new String[] { projprefs, global });
        }
        return global;
    }

    OtpErlangList fCachedPathVars = null;

    public OtpErlangList getPathVars() {
        if (fCachedPathVars == null) {
            final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
                    .getPathVariableManager();
            final String[] names = pvm.getPathVariableNames();
            final OtpErlangObject[] objects = new OtpErlangObject[names.length];
            for (int i = 0; i < names.length; i++) {
                final String name = names[i];
                final String value = PluginUtils.getPVMValue(pvm, name)
                        .toOSString();
                objects[i] = new OtpErlangTuple(new OtpErlangObject[] {
                        new OtpErlangString(name), new OtpErlangString(value) });
            }
            fCachedPathVars = new OtpErlangList(objects);
        }
        return fCachedPathVars;
    }

    public IErlFunction findFunction(final FunctionRef r) {
        final IErlModule m = findModule(r.module);
        try {
            m.open(null);
            return m.findFunction(new ErlangFunction(r.function, r.arity));
        } catch (final ErlModelException e) {
        }
        return null;
    }

    public IErlModule findModuleExt(final String name) {
        try {
            for (final IErlElement e : getChildren()) {
                if (e instanceof IErlProject) {
                    final IErlProject p = (IErlProject) e;
                    final IErlModule m = p.getModuleExt(name);
                    if (m != null) {
                        return m;
                    }
                }
            }
        } catch (final ErlModelException e) {
        }
        return null;
    }

    public String getExternalModules(final IErlProject erlProject) {
        return getExternal(erlProject, External.EXTERNAL_MODULES);
    }

    public String getExternalIncludes(final IErlProject erlProject) {
        return getExternal(erlProject, External.EXTERNAL_INCLUDES);
    }
}
