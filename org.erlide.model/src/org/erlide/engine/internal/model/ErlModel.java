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
package org.erlide.engine.internal.model;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IPathVariableChangeEvent;
import org.eclipse.core.resources.IPathVariableChangeListener;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.ModelPlugin;
import org.erlide.engine.internal.model.cache.ErlModelCache;
import org.erlide.engine.internal.model.erlang.ErlModule;
import org.erlide.engine.internal.model.root.ErlElementDelta;
import org.erlide.engine.internal.model.root.ErlFolder;
import org.erlide.engine.internal.model.root.ErlOtpLibrary;
import org.erlide.engine.internal.model.root.ErlProject;
import org.erlide.engine.internal.model.root.Openable;
import org.erlide.engine.internal.util.ModelConfig;
import org.erlide.engine.model.ElementChangedEvent;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.ErlModelStatus;
import org.erlide.engine.model.IElementChangedListener;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.IErlModelChangeListener;
import org.erlide.engine.model.IOpenable;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.ErlangFunction;
import org.erlide.engine.model.erlang.FunctionRef;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlElementDelta;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlFolder;
import org.erlide.engine.model.root.IErlLibrary;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.ProjectConfigurationChangeListener;
import org.erlide.engine.util.CommonUtils;
import org.erlide.engine.util.NatureUtil;
import org.erlide.engine.util.ResourceUtil;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Predicate;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

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

    private final List<IErlModelChangeListener> fListeners;
    private final IPathVariableChangeListener fPathVariableChangeListener;
    final List<IElementChangedListener> elementChangedListeners;
    private final ErlModelDeltaManager deltaManager;
    OtpErlangList fCachedPathVars;

    /**
     * Constructs a new Erlang Model on the given workspace. Note that only one
     * instance of ErlModel handle should ever be created.
     */
    public ErlModel() {
        super(null, ""); //$NON-NLS-1$
        fPathVariableChangeListener = new PathVariableChangeListener();
        setupWorkspaceListeners();
        fListeners = Lists.newArrayList();
        elementChangedListeners = Lists.newArrayList();
        deltaManager = new ErlModelDeltaManager(this);
    }

    public final void setupWorkspaceListeners() {
        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        final IPathVariableManager pvm = workspace.getPathVariableManager();
        pvm.addChangeListener(fPathVariableChangeListener);
        final IResourceChangeListener listener = new ResourceChangeListener();
        workspace.addResourceChangeListener(listener);
    }

    @Override
    public boolean buildStructure(final IProgressMonitor pm) {
        setChildren(null);
        // determine my children
        final IProject[] projects = ResourcesPlugin.getWorkspace().getRoot()
                .getProjects();
        for (final IProject project : projects) {
            if (NatureUtil.hasErlangNature(project) && getErlangProject(project) == null) {
                addChild(createErlangProject(project));
            }
        }

        return true;
    }

    /**
     * @see IErlElement
     */
    @Override
    public ErlElementKind getKind() {
        return ErlElementKind.MODEL;
    }

    /**
     * @see IErlModel
     */
    @Override
    public IErlProject getErlangProject(final IProject project) {
        if (!project.isAccessible()) {
            return null;
        }
        final IErlElement e = getChildWithResource(project);
        if (e instanceof IErlProject) {
            return (IErlProject) e;
        }
        if (NatureUtil.hasErlangNature(project)) {
            return createErlangProject(project);
        }
        return null;
    }

    private IErlProject createErlangProject(final IProject project) {
        final IErlProject ep = new ErlProject(project, this);
        addChild(ep);
        final ErlModelCache cache = getModelCache();
        cache.newProjectCreated();
        return ep;
    }

    /**
     * @see IErlModel
     */
    @Override
    public Collection<IErlProject> getErlangProjects() throws ErlModelException {
        final Collection<IErlElement> list = getChildrenOfKind(ErlElementKind.PROJECT);
        final Collection<IErlProject> result = Lists.newArrayList();
        for (final IErlElement e : list) {
            result.add((IErlProject) e);
        }
        return result;
    }

    @Override
    public IResource getResource() {
        return ResourcesPlugin.getWorkspace().getRoot();
    }

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

    @Override
    public void notifyChange(final IErlElement element) {
        if (System.getProperty("erlide.model.notify") != null) {
            ErlLogger.debug("   caller = " + getStack());
        }
        for (final IErlModelChangeListener listener : fListeners) {
            listener.elementChanged(element);
        }
    }

    private static synchronized String getStack() {
        final StringBuilder result = new StringBuilder();
        final StackTraceElement[] st = new Throwable().getStackTrace();
        for (final StackTraceElement el : st) {
            result.append("      ").append(el.toString()).append("\n");
        }
        return result.toString();
    }

    @Override
    public void addModelChangeListener(final IErlModelChangeListener listener) {
        if (!fListeners.contains(listener)) {
            fListeners.add(listener);
        }
    }

    @Override
    public void removeModelChangeListener(final IErlModelChangeListener listener) {
        fListeners.remove(listener);
    }

    @Override
    protected void closing(final Object info) throws ErlModelException {
        final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
                .getPathVariableManager();
        pvm.removeChangeListener(fPathVariableChangeListener);
    }

    @Override
    public IErlElement findElement(final IResource rsrc) {
        return findElement(rsrc, false);
    }

    @Override
    public IErlElement findElement(final IResource rsrc, final boolean openElements) {
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
            if (openElements && c instanceof IOpenable) {
                final IOpenable o = (IOpenable) c;
                try {
                    o.open(null);
                } catch (final ErlModelException e) {
                    return null;
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

    @Override
    public IErlElement innermostThat(final IErlElement el,
            final Predicate<IErlElement> firstThat) {
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
        if (firstThat.apply(el)) {
            return el;
        }
        return null;
    }

    @Override
    public IErlModule findModule(final IFile file) {
        try {
            open(null);
        } catch (final ErlModelException e) {
        }
        IErlElement element = findElement(file, false);
        if (element == null) {
            element = findElement(file, true);
        }
        if (element == null) {
            return (IErlModule) create(file);
        }
        return (IErlModule) element;
    }

    @Override
    public IErlProject findProject(final IProject project) {
        try {
            open(null);
        } catch (final ErlModelException e) {
        }
        final IErlElement e = findElement(project);
        if (e == null) {
            return null;
        }
        return (IErlProject) e;
    }

    @Override
    public IErlModule findModule(final String name) throws ErlModelException {
        return findModuleFromProject(null, name, null, false,
                IErlElementLocator.Scope.ALL_PROJECTS);
    }

    @Override
    public final IErlProject newProject(final String name, final String path)
            throws ErlModelException {
        final IWorkspace ws = ResourcesPlugin.getWorkspace();
        final IProject project = ws.getRoot().getProject(name);
        try {
            if (!project.exists()) {
                project.create(null);
                project.open(null);
                final IProjectDescription description = project.getDescription();
                description.setNatureIds(new String[] { ModelPlugin.NATURE_ID });
                description.setName(name);
                project.setDescription(description, null);
            }
            if (!project.isOpen()) {
                project.open(null);
            }
            return findProject(project);
        } catch (final CoreException e) {
            throw new ErlModelException(e, new ErlModelStatus(e));
        }
    }

    private final class PathVariableChangeListener implements IPathVariableChangeListener {

        @Override
        public void pathVariableChanged(final IPathVariableChangeEvent event) {
            fCachedPathVars = null;
            ErlModelCache.getDefault().pathVarsChanged();
            try {
                // broadcast this change to projects, they need to clear their
                // caches
                for (final IErlProject project : getErlangProjects()) {
                    ((ErlProject) project).pathVarsChanged();
                }
            } catch (final ErlModelException e) {
            }
        }

    }

    @Override
    public OtpErlangList getPathVars() {
        // if (fCachedPathVars == null) {
        final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
                .getPathVariableManager();
        final String[] names = pvm.getPathVariableNames();
        final OtpErlangObject[] objects = new OtpErlangObject[names.length];
        for (int i = 0; i < names.length; i++) {
            final String name = names[i];
            final String value = URIUtil.toPath(pvm.getURIValue(name)).toPortableString();
            objects[i] = new OtpErlangTuple(new OtpErlangObject[] {
                    new OtpErlangString(name), new OtpErlangString(value) });
        }
        fCachedPathVars = new OtpErlangList(objects);
        // }
        return fCachedPathVars;
    }

    @Override
    public IErlFunction findFunction(final FunctionRef r) throws ErlModelException {
        final IErlModule module = findModule(r.module);
        if (module == null) {
            return null;
        }
        module.open(null);
        return module.findFunction(new ErlangFunction(r.function, r.arity));
    }

    @Override
    public IErlModule findModule(final String moduleName, final String modulePath)
            throws ErlModelException {
        return findModuleFromProject(null, moduleName, modulePath, true,
                IErlElementLocator.Scope.ALL_PROJECTS);
    }

    @Override
    public IErlModule findInclude(final String includeName, final String includePath)
            throws ErlModelException {
        return findIncludeFromProject(null, includeName, includePath, false,
                IErlElementLocator.Scope.ALL_PROJECTS);
    }

    /**
     * Adds the given listener for changes to Erlang elements. Has no effect if
     * an identical listener is already registered. After completion of this
     * method, the given listener will be registered for exactly the specified
     * events. If they were previously registered for other events, they will be
     * deregistered.
     * <p>
     * Once registered, a listener starts receiving notification of changes to
     * Erlang elements in the model. The listener continues to receive
     * notifications until it is replaced or removed.
     * </p>
     * <p>
     * Listeners can listen for several types of event as defined in
     * <code>ElementChangeEvent</code>. Clients are free to register for any
     * number of event types however if they register for more than one, it is
     * their responsibility to ensure they correctly handle the case where the
     * same Erlang element change shows up in multiple notifications. Clients
     * are guaranteed to receive only the events for which they are registered.
     * </p>
     *
     * @param listener
     *            the listener
     * @param eventMask
     *            the bit-wise OR of all event types of interest to the listener
     * @see IElementChangedListener
     * @see ElementChangedEvent
     * @see #removeElementChangedListener(IElementChangedListener)
     */
    @Override
    public void addElementChangedListener(final IElementChangedListener listener,
            final int eventMask) {
        // getDefault().addElementChangedListener(listener, eventMask);
    }

    /**
     * Removes the given element changed listener. Has no affect if an identical
     * listener is not registered.
     *
     * @param listener
     *            the listener
     */
    @Override
    public void removeElementChangedListener(final IElementChangedListener listener) {
        // getDefault().removeElementChangedListener(listener);
    }

    /**
     * Adds the given listener for changes to Erlang elements. Has no effect if
     * an identical listener is already registered.
     *
     * This listener will only be notified during the POST_CHANGE resource
     * change notification and any reconcile operation (POST_RECONCILE). For
     * finer control of the notification, use
     * <code>addElementChangedListener(IElementChangedListener,int)</code>,
     * which allows to specify a different eventMask.
     *
     * @param listener
     *            the listener
     * @see ElementChangedEvent
     */
    @Override
    public void addElementChangedListener(final IElementChangedListener listener) {
        addElementChangedListener(listener, ElementChangedEvent.POST_CHANGE);
        // | ElementChangedEvent.POST_RECONCILE);
    }

    private static Map<Object, IErlModule> moduleMap = new HashMap<Object, IErlModule>();
    private static Map<IErlModule, Object> mapModule = new HashMap<IErlModule, Object>();

    @Override
    public IErlModule getModuleFromFile(final IParent parent, final String name,
            final String path, final String encoding, final String key) {
        return getModuleWithoutResource(parent, name, path, encoding, null, key);
    }

    @Override
    public IErlModule getModuleFromText(final IParent parent, final String name,
            final String initialText, final String key) {
        return getModuleWithoutResource(parent, name, null, null, initialText, key);
    }

    private IErlModule getModuleWithoutResource(final IParent parent, final String name,
            final String path, final String encoding, final String initialText,
            final String key) {
        IErlModule m = moduleMap.get(key);
        if (m == null) {
            final IParent parent2 = parent == null ? this : parent;
            m = new ErlModule(parent2, name, path, encoding, initialText);
            if (key != null) {
                moduleMap.put(key, m);
                mapModule.put(m, key);
            }
        }
        return m;
    }

    @Override
    public void removeModule(final IErlModule module) {
        final Object key = mapModule.get(module);
        if (key != null) {
            mapModule.remove(module);
            moduleMap.remove(key);
        }
        ErlModelCache.getDefault().removeModule(module);
    }

    @Override
    public void putEdited(final String path, final IErlModule module) {
        ErlModelCache.getDefault().putEdited(path, module);
    }

    /**
     * Registers the given delta with this manager. This API is to be used to
     * registered deltas that are created explicitly by the Erlang Model. Deltas
     * created as translations of <code>IResourceDeltas</code> are to be
     * registered with <code>#registerResourceDelta</code>.
     */
    @Override
    public void registerModelDelta(final IErlElementDelta delta) {
        deltaManager.erlModelDeltas.add(delta);
    }

    public void notifyListeners(final IErlElementDelta deltaToNotify,
            final int eventType, final IElementChangedListener[] listeners,
            final int[] listenerMask, final int listenerCount) {

        final ElementChangedEvent extraEvent = new ElementChangedEvent(deltaToNotify,
                eventType);
        for (int i = 0; i < listenerCount; i++) {
            if (listenerMask == null || (listenerMask[i] & eventType) != 0) {
                final IElementChangedListener listener = listeners[i];
                long start = -1;
                if (ModelConfig.verbose) {
                    ErlLogger.debug("Listener #" + (i + 1) + "=" + listener.toString());//$NON-NLS-1$//$NON-NLS-2$
                    start = System.currentTimeMillis();
                }
                // wrap callbacks with Safe runnable for subsequent listeners to
                // be called
                // when some are causing grief
                SafeRunner.run(new ISafeRunnable() {

                    @Override
                    public void handleException(final Throwable exception) {
                        // CCorePlugin.log(exception, "Exception occurred in
                        // listener of C
                        // element change notification"); //$NON-NLS-1$
                        ErlLogger.error(exception);
                    }

                    @Override
                    public void run() throws Exception {
                        listener.elementChanged(extraEvent);
                    }
                });
                if (ModelConfig.verbose) {
                    ErlLogger.debug(" -> " + (System.currentTimeMillis() - start) + "ms"); //$NON-NLS-1$ //$NON-NLS-2$
                }
            }
        }
    }

    public IErlElement create(final IResource resource, final IParent parent) {
        if (resource == null) {
            return null;
        }
        final IErlElement e = findElement(resource);
        if (e != null) {
            return e;
        }
        final int type = resource.getType();
        switch (type) {
        case IResource.PROJECT:
            return createProject((IProject) resource);
        case IResource.FILE:
            return createFile((IFile) resource, parent);
        case IResource.FOLDER:
            return createFolder((IFolder) resource, parent);
        case IResource.ROOT:
            return createRoot((IWorkspaceRoot) resource);
        default:
            return null;
        }
        // TODO should we make Erlidemodelevents and fire them?
    }

    void remove(final IResource rsrc) {
        final IErlElement element = findElement(rsrc);
        if (element != null) {
            final IParent p = element.getParent();
            p.removeChild(element);
            if (element instanceof IOpenable) {
                final IOpenable openable = (IOpenable) element;
                try {
                    openable.close();
                } catch (final ErlModelException e) {
                    ErlLogger.error(e);
                }
            }
        }
        // TODO should we make Erlidemodelevents and fire them?
    }

    void change(final IResource rsrc, final IResourceDelta delta) {
        final IErlElement e = findElement(rsrc);
        if (e != null) {
            e.resourceChanged(delta);
        }
        // TODO should we make Erlidemodelevents and fire them?
    }

    /**
     * Returns the Erlang element corresponding to the given file, its project
     * being the given project. Returns <code>null</code> if unable to associate
     * the given file with a Erlang element.
     *
     * <p>
     * The file must be one of:
     * <ul>
     * <li>a <code>.erl</code> file - the element returned is the corresponding
     * <code>IErlModule</code></li>
     * <li>a <code>.beam</code> file - the element returned is the corresponding
     * <code>IBeamFile</code></li>
     * </ul>
     * <p>
     * Creating a Erlang element has the side effect of creating and opening all
     * of the element's parents if they are not yet open.
     */
    public IErlElement createFile(final IFile file, final IParent parent0) {
        if (file == null) {
            return null;
        }
        IParent parent = parent0;
        if (parent == null) {
            final IContainer parentResource = file.getParent();
            if (parentResource != null) {
                final IErlElement element = findElement(parentResource);
                if (element instanceof IParent) {
                    parent = (IParent) element;
                }
            }
        }
        if (CommonUtils.isErlangFileContentFileName(file.getName())) {
            return createModuleFromFile(file, parent);
        }
        return null;
    }

    public IErlFolder createFolder(final IFolder folder, final IParent parent) {
        if (folder == null) {
            return null;
        }
        final IErlFolder f = new ErlFolder(folder, parent);
        final IParent p = parent;
        if (p != null) {
            p.addChild(f);
        } else {
            // ErlLogger.warn("creating folder %s in null parent?!", folder
            // .getName());
        }
        return f;
    }

    public IErlModule createModuleFromFile(final IFile file, final IParent parent) {
        if (file == null) {
            return null;
        }
        final String name = file.getName();
        if (CommonUtils.isErlangFileContentFileName(name)) {
            final IErlModule module = new ErlModule(parent, name, file);
            if (parent != null) {
                parent.addChild(module);
            }
            return module;
        }
        return null;
    }

    /**
     * Returns the Erlang project corresponding to the given project.
     * <p>
     * Creating a Erlang Project has the side effect of creating and opening all
     * of the project's parents if they are not yet open.
     * <p>
     * Note that no check is done at this time on the existence or the Erlang
     * nature of this project.
     *
     * @param project
     *            the given project
     * @return the Erlang project corresponding to the given project, null if
     *         the given project is null
     */
    public IErlProject createProject(final IProject project) {
        if (project == null) {
            return null;
        }
        return createErlangProject(project);
    }

    public IErlLibrary createLibrary(final RuntimeVersion version) {
        if (version == null) {
            return null;
        }
        final IErlLibrary ep = new ErlOtpLibrary(version, this);
        addChild(ep);
        return ep;
    }

    /**
     * Returns the Erlang element corresponding to the given resource, or
     * <code>null</code> if unable to associate the given resource with a Erlang
     * element.
     * <p>
     * The resource must be one of:
     * <ul>
     * <li>a project - the element returned is the corresponding
     * <code>IErlProject</code></li>
     * <li>a <code>.erl</code> file - the element returned is the corresponding
     * <code>IErlModule</code></li>
     * <li>a folder - the element returned is the corresponding
     * <code>IErlFolder</code></li>
     * <li>the workspace root resource - the element returned is the
     * <code>IErlModel</code></li>
     * </ul>
     * <p>
     * Creating a Erlang element has the side effect of creating and opening all
     * of the element's parents if they are not yet open.
     *
     * @param resource
     *            the given resource
     * @return the Erlang element corresponding to the given resource, or
     *         <code>null</code> if unable to associate the given resource with
     *         a Erlang element
     */
    @Override
    public IErlElement create(final IResource resource) {
        IParent parent = null;
        final IContainer resourceParent = resource.getParent();
        if (resourceParent != null) {
            IErlElement element = findElement(resourceParent);
            if (element == null) {
                element = create(resourceParent);
            }
            if (element instanceof IParent) {
                parent = (IParent) element;
            }
        }
        return create(resource, parent);
    }

    /**
     * Returns the Erlang model.
     *
     * @param root
     *            the given root
     * @return the Erlang model, or <code>null</code> if the root is null
     */
    private IErlModel createRoot(final IWorkspaceRoot root) {
        if (root == null) {
            return null;
        }
        return this;
    }

    class ResourceChangeListener implements IResourceChangeListener {
        private final class NoOpVisitor implements IResourceDeltaVisitor {
            @Override
            public boolean visit(final IResourceDelta delta) throws CoreException {
                return false;
            }
        }

        private final class PreCloseVisitor implements IResourceDeltaVisitor {
            private final List<IResource> removed;

            private PreCloseVisitor(final List<IResource> removed) {
                this.removed = removed;
            }

            @Override
            public boolean visit(final IResourceDelta delta) throws CoreException {
                final IResource resource = delta.getResource();
                final boolean erlangProject = resource.getType() == IResource.PROJECT
                        && NatureUtil.hasErlangNature((IProject) resource);
                if (erlangProject) {
                    removed.add(resource);
                }
                return false;
            }
        }

        private final class PostChangeVisitor implements IResourceDeltaVisitor {
            private final List<IResource> removed;
            private final List<IResource> added;
            private final List<IResource> changed;
            private final Map<IResource, IResourceDelta> changedDelta;

            private PostChangeVisitor(final List<IResource> removed,
                    final List<IResource> added, final List<IResource> changed,
                    final Map<IResource, IResourceDelta> changedDelta) {
                this.removed = removed;
                this.added = added;
                this.changed = changed;
                this.changedDelta = changedDelta;
            }

            @Override
            public boolean visit(final IResourceDelta delta) {
                final IResource resource = delta.getResource();
                if (ModelConfig.verbose) {
                    ErlLogger.debug("delta " + delta.getKind() + " for "
                            + resource.getLocation());
                }
                final boolean erlangFile = resource.getType() == IResource.FILE
                        && CommonUtils.isErlangFileContentFileName(resource.getName());
                final boolean erlangProject = resource.getType() == IResource.PROJECT;
                final boolean erlangFolder = resource.getType() == IResource.FOLDER;
                // &&
                // ErlideUtil.isOnSourcePathOrParentToFolderOnSourcePath((
                // IFolder)
                // resource);
                if (erlangFile || erlangProject || erlangFolder) {
                    if (delta.getKind() == IResourceDelta.ADDED) {
                        added.add(resource);
                    }
                    if (delta.getKind() == IResourceDelta.CHANGED) {
                        changed.add(resource);
                        changedDelta.put(resource, delta);
                    }
                    if (delta.getKind() == IResourceDelta.REMOVED) {
                        removed.add(resource);
                    }
                }
                return !erlangFile;
            }
        }

        @Override
        public void resourceChanged(final IResourceChangeEvent event) {
            final IResourceDelta rootDelta = event.getDelta();
            final List<IResource> added = Lists.newArrayList();
            final List<IResource> changed = Lists.newArrayList();
            final List<IResource> removed = Lists.newArrayList();
            final Map<IResource, IResourceDelta> changedDelta = Maps.newHashMap();
            final IResourceDeltaVisitor visitor;
            switch (event.getType()) {
            case IResourceChangeEvent.POST_CHANGE:
                visitor = new PostChangeVisitor(removed, added, changed, changedDelta);
                break;
            case IResourceChangeEvent.PRE_CLOSE:
                visitor = new PreCloseVisitor(removed);
                final IResource resource = event.getResource();
                final boolean erlangProject = resource.getType() == IResource.PROJECT
                        && NatureUtil.hasErlangNature((IProject) resource);
                if (erlangProject) {
                    removed.add(resource);
                }
                break;
            default:
                visitor = new NoOpVisitor();
            }
            if (rootDelta != null) {
                try {
                    rootDelta.accept(visitor);
                } catch (final CoreException e) {
                    ErlLogger.warn(e);
                }
            }
            final Set<IProject> prjs = Sets.newHashSet();
            for (final IResource rsrc : added) {
                prjs.add(rsrc.getProject());
                create(rsrc);
            }
            for (final IResource rsrc : changed) {
                prjs.add(rsrc.getProject());
                change(rsrc, changedDelta.get(rsrc));
            }
            // make sure we don't dispose trees before leaves...
            Collections.sort(removed, new Comparator<IResource>() {

                @Override
                public int compare(final IResource o1, final IResource o2) {
                    if (o1.equals(o2)) {
                        return 0;
                    } else if (o1.getFullPath().isPrefixOf(o2.getFullPath())) {
                        return 1;
                    } else {
                        return -1;
                    }
                }

            });
            for (final IResource rsrc : removed) {
                remove(rsrc);
            }

            for (final IProject prj : prjs) {
                notifyProject(prj);
            }
        }

        private void notifyProject(final IProject prj0) {
            if (!prj0.exists()) {
                return;
            }
            final IErlProject prj = findProject(prj0);
            if (prj instanceof ProjectConfigurationChangeListener) {
                ((ProjectConfigurationChangeListener) prj).configurationChanged();
            }
        }
    }

    private static IErlModule getModuleFromCacheByNameOrPath(final ErlProject project,
            final String moduleName, final String modulePath,
            final IErlElementLocator.Scope scope) {
        final ErlModelCache erlModelCache = ErlModelCache.getDefault();
        if (modulePath != null) {
            final IErlModule module = erlModelCache.getModuleByPath(modulePath);
            if (module != null && (project == null || project.moduleInProject(module))) {
                return module;
            }
        }
        return null;
    }

    private Collection<IErlModule> getAllIncludes(final IErlProject project,
            final boolean checkExternals, final IErlElementLocator.Scope scope)
            throws ErlModelException {
        final List<IErlProject> projects = Lists.newArrayList();
        final List<IErlModule> result = Lists.newArrayList();
        final Set<String> paths = Sets.newHashSet();
        if (project != null) {
            projects.add(project);
            if (scope == IErlElementLocator.Scope.REFERENCED_PROJECTS) {
                projects.addAll(project.getReferencedProjects());
            }
        }
        if (scope == IErlElementLocator.Scope.ALL_PROJECTS) {
            for (final IErlProject project2 : getErlangProjects()) {
                if (!projects.contains(project2)) {
                    projects.add(project2);
                }
            }
        }
        for (final IErlProject project2 : projects) {
            getAllModulesAux(project2.getIncludes(), result, paths);
        }
        if (checkExternals && project != null) {
            getAllModulesAux(project.getExternalIncludes(), result, paths);
        }
        return result;
    }

    static void getAllModulesAux(final Collection<IErlModule> modules,
            final Collection<IErlModule> result, final Set<String> paths) {
        for (final IErlModule module : modules) {
            final String path = module.getFilePath();
            if (path != null) {
                if (paths.contains(path)) {
                    continue;
                }
                paths.add(path);
            }
            result.add(module);
        }
    }

    private IErlModule findIncludeFromProject(final IErlProject project,
            final String includeName, final String includePath,
            final boolean checkExternals, final IErlElementLocator.Scope scope)
            throws ErlModelException {
        if (project != null) {
            final IErlModule module = getModuleFromCacheByNameOrPath(
                    (ErlProject) project, includeName, includePath, scope);
            if (module != null && module.isOnIncludePath()) {
                return module;
            }
        }
        final Collection<IErlModule> includes = getAllIncludes(project, checkExternals,
                scope);
        ErlModelCache.getDefault().putModules(includes);
        if (includePath != null) {
            for (final IErlModule module2 : includes) {
                final String path2 = module2.getFilePath();
                if (path2 != null && includePath.equals(path2)) {
                    return module2;
                }
            }
        }
        if (includeName != null) {
            final boolean hasExtension = SystemConfiguration.hasExtension(includeName);
            for (final IErlModule module2 : includes) {
                final String name = hasExtension ? module2.getName() : module2
                        .getModuleName();
                if (ResourceUtil.samePath(includeName, name)) {
                    return module2;
                }
            }
        }
        return null;
    }

    @Override
    public IErlModule findModuleFromProject(final IErlProject project,
            final String moduleName, final String modulePath,
            final IErlElementLocator.Scope scope) throws ErlModelException {
        return findModuleFromProject(project, moduleName, modulePath, true, scope);
    }

    @Override
    public IErlModule findIncludeFromProject(final IErlProject project,
            final String moduleName, final String modulePath,
            final IErlElementLocator.Scope scope) throws ErlModelException {
        return findIncludeFromProject(project, moduleName, modulePath, true, scope);
    }

    @Override
    public IErlModule findModuleFromProject(final IErlProject project,
            final String moduleName, final String modulePath,
            final boolean checkExternals, final IErlElementLocator.Scope scope)
            throws ErlModelException {
        if (project != null) {
            final IErlModule module = getModuleFromCacheByNameOrPath(
                    (ErlProject) project, moduleName, modulePath, scope);
            if (module != null && module.isOnSourcePath()) {
                return module;
            }
        }
        final List<IErlModule> allModules = Lists.newArrayList();
        final Set<String> paths = Sets.newHashSet();
        try {
            for (int i = 0; i < 2; ++i) {
                final boolean externalModules = i > 0;
                if (externalModules && !checkExternals) {
                    break;
                }
                if (project != null) {
                    final IErlModule module = tryFindModule(Sets.newHashSet(project),
                            moduleName, modulePath, allModules, paths, externalModules);
                    if (module != null) {
                        return module;
                    }
                }
                if ((scope == Scope.REFERENCED_PROJECTS || scope == Scope.ALL_PROJECTS)
                        && project != null) {
                    final Collection<IErlProject> projects = project
                            .getReferencedProjects();
                    final IErlModule module = tryFindModule(projects, moduleName,
                            modulePath, allModules, paths, externalModules);
                    if (module != null) {
                        return module;
                    }
                }

                if (scope == Scope.ALL_PROJECTS) {
                    final Collection<IErlProject> projects = getErlangProjects();
                    final IErlModule module = tryFindModule(projects, moduleName,
                            modulePath, allModules, paths, externalModules);
                    if (module != null) {
                        return module;
                    }
                }
            }
            return null;
        } finally {
            ErlModelCache.getDefault().putModules(allModules);
        }
    }

    private IErlModule tryFindModule(final Collection<IErlProject> projects,
            final String moduleName, final String modulePath,
            final List<IErlModule> allModules, final Set<String> paths,
            final boolean externalModules) throws ErlModelException {
        IErlModule module;
        for (final IErlProject project : projects) {
            final Collection<IErlModule> modules = Lists.newArrayList();
            final Collection<IErlModule> modulesOrExternals = externalModules ? project
                    .getExternalModules() : project.getModules();
            getAllModulesAux(modulesOrExternals, modules, paths);
            allModules.addAll(modules);
            module = findModule(modules, moduleName, modulePath);
            if (module != null) {
                return module;
            }
        }
        return null;
    }

    private IErlModule findModule(final Collection<IErlModule> modules,
            final String moduleName, final String modulePath) {
        if (modulePath != null) {
            for (final IErlModule module : modules) {
                final String path = module.getFilePath();
                if (path != null && ResourceUtil.samePath(modulePath, path)) {
                    return module;
                }
            }
        }
        if (moduleName != null) {
            final boolean hasExtension = SystemConfiguration.hasExtension(moduleName);
            for (final IErlModule module : modules) {
                final String name = hasExtension ? module.getName() : module
                        .getModuleName();
                if (moduleName.equals(name)) {
                    return module;
                }
            }
        }
        return null;
    }

    @Override
    public IErlModule findIncludeFromModule(final IErlModule module,
            final String includeName, final String includePath,
            final IErlElementLocator.Scope scope) throws ErlModelException {
        final IParent parent = module.getParent();
        if (parent instanceof IErlFolder) {
            final IErlFolder folder = (IErlFolder) parent;
            folder.open(null);
            final IErlModule include = folder.findInclude(includeName, includePath);
            if (include != null) {
                return include;
            }
        }
        return findIncludeFromProject(ErlangEngine.getInstance().getModelUtilService()
                .getProject(module), includeName, includePath, true, scope);
    }

    private final Object fModelLock = new Object();

    @Override
    public Object getModelLock() {
        return fModelLock;
    }

    @Override
    public IErlElementDelta createElementDelta(final int kind, final int flags,
            final IErlElement element) {
        return new ErlElementDelta(kind, flags, element);
    }

    @Override
    public Collection<IErlLibrary> getLibraries() throws ErlModelException {
        final Collection<IErlElement> list = getChildrenOfKind(ErlElementKind.LIBRARY);
        final Collection<IErlLibrary> result = Lists.newArrayList();
        for (final IErlElement e : list) {
            result.add((IErlLibrary) e);
        }
        return result;
    }

    @Override
    public IErlLibrary getLibrary(final String name) throws ErlModelException {
        return IterableExtensions.findFirst(getLibraries(),
                new Function1<IErlLibrary, Boolean>() {
                    @Override
                    public Boolean apply(final IErlLibrary input) {
                        return input.getName().equals(name);
                    }
                });
    }

}
