/*******************************************************************************
 * Copyright (c) 2000, 2008 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.dialogs;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.text.Collator;
import java.text.ParseException;
import java.text.RuleBasedCollator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceProxy;
import org.eclipse.core.resources.IResourceProxyVisitor;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.DelegatingStyledCellLabelProvider.IStyledLabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkingSet;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ResourceWorkingSetFilter;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.XMLMemento;
import org.eclipse.ui.actions.WorkingSetFilterActionGroup;
import org.eclipse.ui.dialogs.FilteredItemsSelectionDialog;
import org.eclipse.ui.dialogs.SearchPattern;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.statushandlers.StatusManager;
import org.erlide.backend.BackendUtils;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElementLocator;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.model.util.PluginUtils;
import org.erlide.core.model.util.ResourceUtil;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.utils.CommonUtils;
import org.erlide.utils.PreferencesUtils;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

/**
 * Shows a list of resources to the user with a text entry field for a string
 * pattern used to filter the list of resources.
 * 
 */
public class FilteredModulesSelectionDialog extends
        FilteredItemsSelectionDialog {

    private static final String DIALOG_SETTINGS = "org.eclipse.ui.dialogs.FilteredResourcesSelectionDialog"; //$NON-NLS-1$
    private static final String WORKINGS_SET_SETTINGS = "WorkingSet"; //$NON-NLS-1$

    private final ModuleItemLabelProvider moduleItemLabelProvider;
    private final ModuleItemDetailsLabelProvider moduleItemDetailsLabelProvider;
    private WorkingSetFilterActionGroup workingSetFilterActionGroup;
    final CustomWorkingSetFilter workingSetFilter = new CustomWorkingSetFilter();
    private String title;
    final IContainer container;
    final int typeMask;
    private Comparator<Object> fComparator = null;
    private Collator fCollator = null;

    /**
     * Creates a new instance of the class
     * 
     * @param shell
     *            the parent shell
     * @param multi
     *            the multi selection flag
     * @param container
     *            the container
     * @param typesMask
     *            the types mask
     */
    public FilteredModulesSelectionDialog(final Shell shell,
            final boolean multi, final IContainer container, final int typesMask) {
        super(shell, multi);

        setSelectionHistory(new ModuleSelectionHistory());

        setTitle("Open Module");
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(shell, IErlangHelpContextIds.OPEN_MODULE_DIALOG);

        this.container = container;
        typeMask = typesMask;

        moduleItemLabelProvider = new ModuleItemLabelProvider();
        moduleItemDetailsLabelProvider = new ModuleItemDetailsLabelProvider();
        setListLabelProvider(moduleItemLabelProvider);
        setDetailsLabelProvider(moduleItemDetailsLabelProvider);
    }

    @Override
    public void setTitle(final String title) {
        super.setTitle(title);
        this.title = title;
    }

    /**
     * Adds or replaces subtitle of the dialog
     * 
     * @param text
     *            the new subtitle
     */
    void setSubtitle(final String text) {
        if (text == null || text.length() == 0) {
            getShell().setText(title);
        } else {
            getShell().setText(title + " - " + text); //$NON-NLS-1$
        }
    }

    @Override
    protected IDialogSettings getDialogSettings() {
        IDialogSettings settings = ErlideUIPlugin.getDefault()
                .getDialogSettings().getSection(DIALOG_SETTINGS);

        if (settings == null) {
            settings = ErlideUIPlugin.getDefault().getDialogSettings()
                    .addNewSection(DIALOG_SETTINGS);
        }

        return settings;
    }

    @Override
    protected void storeDialog(final IDialogSettings settings) {
        super.storeDialog(settings);

        final XMLMemento memento = XMLMemento.createWriteRoot("workingSet"); //$NON-NLS-1$
        workingSetFilterActionGroup.saveState(memento);
        workingSetFilterActionGroup.dispose();
        final StringWriter writer = new StringWriter();
        try {
            memento.save(writer);
            settings.put(WORKINGS_SET_SETTINGS, writer.getBuffer().toString());
        } catch (final IOException e) {
            StatusManager.getManager().handle(
                    new Status(IStatus.ERROR, ErlideUIPlugin.PLUGIN_ID,
                            IStatus.ERROR, "", e)); //$NON-NLS-1$
            // don't do anything. Simply don't store the settings
        }
    }

    @Override
    protected void restoreDialog(final IDialogSettings settings) {
        super.restoreDialog(settings);

        final String setting = settings.get(WORKINGS_SET_SETTINGS);
        if (setting != null) {
            try {
                final IMemento memento = XMLMemento
                        .createReadRoot(new StringReader(setting));
                workingSetFilterActionGroup.restoreState(memento);
            } catch (final WorkbenchException e) {
                StatusManager.getManager().handle(
                        new Status(IStatus.ERROR, ErlideUIPlugin.PLUGIN_ID,
                                IStatus.ERROR, "", e)); //$NON-NLS-1$
                // don't do anything. Simply don't restore the settings
            }
        }

        addListFilter(workingSetFilter);

        applyFilter();
    }

    @Override
    protected void fillViewMenu(final IMenuManager menuManager) {
        super.fillViewMenu(menuManager);

        workingSetFilterActionGroup = new WorkingSetFilterActionGroup(
                getShell(), new IPropertyChangeListener() {
                    @Override
                    public void propertyChange(final PropertyChangeEvent event) {
                        final String property = event.getProperty();

                        if (WorkingSetFilterActionGroup.CHANGE_WORKING_SET
                                .equals(property)) {

                            IWorkingSet workingSet = (IWorkingSet) event
                                    .getNewValue();

                            if (workingSet != null
                                    && !(workingSet.isAggregateWorkingSet() && workingSet
                                            .isEmpty())) {
                                workingSetFilter.setWorkingSet(workingSet);
                                setSubtitle(workingSet.getLabel());
                            } else {
                                final IWorkbenchWindow window = PlatformUI
                                        .getWorkbench()
                                        .getActiveWorkbenchWindow();

                                if (window != null) {
                                    final IWorkbenchPage page = window
                                            .getActivePage();
                                    workingSet = page.getAggregateWorkingSet();

                                    if (workingSet.isAggregateWorkingSet()
                                            && workingSet.isEmpty()) {
                                        workingSet = null;
                                    }
                                }

                                workingSetFilter.setWorkingSet(workingSet);
                                setSubtitle(null);
                            }

                            scheduleRefresh();
                        }
                    }
                });

        menuManager.add(new Separator());
        workingSetFilterActionGroup.fillContextMenu(menuManager);
    }

    @Override
    protected Control createExtendedContentArea(final Composite parent) {
        return null;
    }

    private boolean isLostFound(final IPath projectRelativePath) {
        for (final String s : projectRelativePath.segments()) {
            if ("lost+found".equals(s)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public int open() {
        if (getInitialPattern() == null) {
            final IWorkbenchWindow window = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow();
            if (window != null) {
                final ISelection selection = window.getSelectionService()
                        .getSelection();
                if (selection instanceof ITextSelection) {
                    String text = ((ITextSelection) selection).getText();
                    if (text != null) {
                        text = text.trim();
                        if (text.length() > 0) {
                            final IWorkspace workspace = ResourcesPlugin
                                    .getWorkspace();
                            final IStatus result = workspace.validateName(text,
                                    IResource.FILE);
                            if (result.isOK()) {
                                setInitialPattern(text);
                            }
                        }
                    }
                }
            }
        }
        return super.open();
    }

    @Override
    public String getElementName(final Object item) {
        if (item instanceof String) {
            return (String) item;
        }
        final IResource resource = (IResource) item;
        return resource.getName();
    }

    @Override
    protected IStatus validateItem(final Object item) {
        return new Status(IStatus.OK, ErlideUIPlugin.PLUGIN_ID, 0, "", null); //$NON-NLS-1$
    }

    @Override
    protected ItemsFilter createFilter() {
        return new ModuleFilter(container, typeMask);
    }

    @Override
    protected Comparator<Object> getItemsComparator() {
        if (fComparator == null) {
            final Collator collator = Collator.getInstance();
            if (collator instanceof RuleBasedCollator) {
                final RuleBasedCollator rbc = (RuleBasedCollator) collator;
                final String rules = rbc.getRules();
                final String newRules = rules.replaceFirst("<\'.\'<", "<")
                        .replaceFirst("<\'_\'<", "<\'.\'<\'_\'<");
                try {
                    fCollator = new RuleBasedCollator(newRules);
                } catch (final ParseException e) {
                    e.printStackTrace();
                    fCollator = collator;
                }
            }

            fComparator = new Comparator<Object>() {

                @Override
                public int compare(final Object o1, final Object o2) {
                    final String s1 = o1 instanceof IResource ? ((IResource) o1)
                            .getName() : (String) o1;
                    final String s2 = o2 instanceof IResource ? ((IResource) o2)
                            .getName() : (String) o2;
                    final int comparability = fCollator.compare(s1, s2);
                    return comparability;
                }
            };
        }
        return fComparator;
    }

    @Override
    protected void fillContentProvider(
            final AbstractContentProvider contentProvider,
            final ItemsFilter itemsFilter,
            final IProgressMonitor progressMonitor) throws CoreException {
        if (itemsFilter instanceof ModuleFilter) {

            container.accept(new ModuleProxyVisitor(contentProvider,
                    (ModuleFilter) itemsFilter, progressMonitor),
                    IResource.NONE);
        }
        if (progressMonitor != null) {
            progressMonitor.done();
        }

    }

    /**
     * A label provider for ResourceDecorator objects. It creates labels with a
     * resource full path for duplicates. It uses the Platform UI label
     * decorator for providing extra resource info.
     */
    private class ModuleItemLabelProvider extends LabelProvider implements
            ILabelProviderListener, IStyledLabelProvider {

        // Need to keep our own list of listeners
        final ListenerList listeners = new ListenerList();

        WorkbenchLabelProvider provider = new WorkbenchLabelProvider();

        public ModuleItemLabelProvider() {
            super();
            provider.addListener(this);
        }

        @Override
        public Image getImage(final Object element) {
            if (!(element instanceof IResource)) {
                return super.getImage(element);
            }

            final IResource res = (IResource) element;

            return provider.getImage(res);
        }

        @Override
        public String getText(final Object element) {
            if (!(element instanceof IResource)) {
                return super.getText(element);
            }

            final IResource res = (IResource) element;
            String str = res.getName();

            // extra info for duplicates
            if (isDuplicateElement(element)) {
                str = str
                        + " - " + res.getParent().getFullPath().makeRelative().toString(); //$NON-NLS-1$
            }

            return str;
        }

        @Override
        public StyledString getStyledText(final Object element) {
            if (!(element instanceof IResource)) {
                return new StyledString(super.getText(element));
            }

            final String text = getText(element);
            final StyledString str = new StyledString(text);

            final int index = text.indexOf(" - ");
            if (index != -1) {
                str.setStyle(index, text.length() - index,
                        StyledString.QUALIFIER_STYLER);
            }
            return str;
        }

        @Override
        public void dispose() {
            provider.removeListener(this);
            provider.dispose();

            super.dispose();
        }

        @Override
        public void addListener(final ILabelProviderListener listener) {
            listeners.add(listener);
        }

        @Override
        public void removeListener(final ILabelProviderListener listener) {
            listeners.remove(listener);
        }

        @Override
        public void labelProviderChanged(final LabelProviderChangedEvent event) {
            final Object[] l = listeners.getListeners();
            for (int i = 0; i < listeners.size(); i++) {
                ((ILabelProviderListener) l[i]).labelProviderChanged(event);
            }
        }

    }

    /**
     * A label provider for details of ResourceItem objects.
     */
    class ModuleItemDetailsLabelProvider extends ModuleItemLabelProvider {

        @Override
        public Image getImage(final Object element) {
            if (!(element instanceof IResource)) {
                return super.getImage(element);
            }

            final IResource parent = ((IResource) element).getParent();
            return provider.getImage(parent);
        }

        @Override
        public String getText(final Object element) {
            if (!(element instanceof IResource)) {
                return super.getText(element);
            }

            final IResource parent = ((IResource) element).getParent();

            if (parent.getType() == IResource.ROOT) {
                // Get readable name for workspace root ("Workspace"), without
                // duplicating language-specific string here.
                return null;
            }

            return parent.getProjectRelativePath().makeRelative().toString()
                    + " - " + parent.getProject().getName();
        }

        @Override
        public void labelProviderChanged(final LabelProviderChangedEvent event) {
            final Object[] l = super.listeners.getListeners();
            for (int i = 0; i < super.listeners.size(); i++) {
                ((ILabelProviderListener) l[i]).labelProviderChanged(event);
            }
        }
    }

    /**
     * Viewer filter which filters resources due to current working set
     */
    static class CustomWorkingSetFilter extends ViewerFilter {
        private final ResourceWorkingSetFilter resourceWorkingSetFilter = new ResourceWorkingSetFilter();

        /**
         * Returns the active working set the filter is working with.
         * 
         * @return the active working set
         */
        public IWorkingSet getWorkingSet() {
            return resourceWorkingSetFilter.getWorkingSet();
        }

        /**
         * Sets the active working set.
         * 
         * @param workingSet
         *            the working set the filter should work with
         */
        public void setWorkingSet(final IWorkingSet workingSet) {
            resourceWorkingSetFilter.setWorkingSet(workingSet);
        }

        @Override
        public boolean select(final Viewer viewer, final Object parentElement,
                final Object element) {
            return resourceWorkingSetFilter.select(viewer, parentElement,
                    element);
        }
    }

    /**
     * ResourceProxyVisitor to visit resource tree and get matched resources.
     * During visit resources it updates progress monitor and adds matched
     * resources to ContentProvider instance.
     */
    private class ModuleProxyVisitor implements IResourceProxyVisitor {

        private final AbstractContentProvider proxyContentProvider;
        private final ModuleFilter moduleFilter;
        private final IProgressMonitor progressMonitor;
        private final List<IResource> projects;
        private final Set<IPath> validPaths = new HashSet<IPath>();
        private final Set<IPath> extraLocations = Sets.newHashSet();

        /**
         * Creates new ResourceProxyVisitor instance.
         * 
         * @param contentProvider
         * @param moduleFilter
         * @param progressMonitor
         * @throws CoreException
         */
        public ModuleProxyVisitor(
                final AbstractContentProvider contentProvider,
                final ModuleFilter moduleFilter,
                final IProgressMonitor progressMonitor) throws CoreException {
            super();
            proxyContentProvider = contentProvider;
            this.moduleFilter = moduleFilter;
            this.progressMonitor = progressMonitor;
            final IResource[] resources = container.members();
            projects = new ArrayList<IResource>(Arrays.asList(resources));
            extraLocations.addAll(BackendUtils.getExtraSourcePaths());
            if (progressMonitor != null) {
                progressMonitor.beginTask("Searching", projects.size());
            }
        }

        @Override
        public boolean visit(final IResourceProxy proxy) {

            if (progressMonitor.isCanceled()) {
                return false;
            }

            final IResource resource = proxy.requestResource();
            if (!resource.isAccessible()) {
                return false;
            }
            final IProject project = resource.getProject();
            final boolean accessible = project != null
                    && project.isAccessible();
            if (project != null && !accessible) {
                return false;
            }
            if (projects.remove(project) || projects.remove(resource)) {
                progressMonitor.worked(1);
                if (accessible) {
                    addPaths(project);
                }
            }

            // FIXME (JC) all this seems too much... is it really necessary?
            // couldn't we just assume all links in external files should be
            // matchable?
            if (project == resource && accessible) {
                final IErlElementLocator model = ErlModelManager
                        .getErlangModel();
                final IErlProject erlProject = model.findProject(project);
                final String extMods = erlProject.getExternalModulesString();
                final List<String> files = new ArrayList<String>();
                files.addAll(PreferencesUtils.unpackList(extMods));
                final String extIncs = erlProject.getExternalIncludesString();
                files.addAll(PreferencesUtils.unpackList(extIncs));

                final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
                        .getPathVariableManager();
                for (final String str : files) {
                    IResource fres;
                    try {
                        fres = ResourceUtil.recursiveFindNamedResource(project,
                                str, null);
                    } catch (final CoreException e) {
                        fres = null;
                    }
                    if (fres != null) {
                        final List<String> lines = PreferencesUtils
                                .readFile(fres.getLocation().toString());
                        for (final String pref : lines) {

                            String path;
                            final IPath p = new Path(pref);
                            final IPath v = PluginUtils.resolvePVMPath(pvm, p);
                            if (v.isAbsolute()) {
                                path = v.toString();
                            } else {
                                path = project.getLocation().append(v)
                                        .toString();
                            }
                            proxyContentProvider.add(path, moduleFilter);
                        }
                    }
                }
            }

            if (resource.isDerived()) {
                return false;
            }

            if (CommonUtils.isErlangFileContentFileName(resource.getName())
                    && !isLostFound(resource.getProjectRelativePath())) {
                final IContainer my_container = resource.getParent();
                if (validPaths.contains(my_container.getFullPath())
                        || !extraLocations.isEmpty()
                        && extraLocations.contains(my_container.getLocation())) {
                    proxyContentProvider.add(resource, moduleFilter);
                }
            }

            if (resource.getType() == IResource.FILE) {
                return false;
            }

            return true;
        }

        private void addPaths(final IProject project) {
            final IErlProject erlProject = ErlModelManager.getErlangModel()
                    .getErlangProject(project);
            if (erlProject != null) {
                validPaths.addAll(getFullPaths(project,
                        erlProject.getIncludeDirs()));
                validPaths.addAll(getFullPaths(project,
                        erlProject.getSourceDirs()));
                final Collection<IPath> extras = Lists.newArrayList();
                for (final IPath p : BackendUtils
                        .getExtraSourcePathsForModel(project)) {
                    extras.add(p);
                }
                validPaths.addAll(getFullPaths(project, extras));
            }
        }

        private Set<IPath> getFullPaths(final IProject project,
                final Collection<IPath> sourcePaths) {
            final HashSet<IPath> result = new HashSet<IPath>();
            for (final IPath path : sourcePaths) {
                final String path_string = path.toString();
                if (path_string.equals(".")) {
                    result.add(project.getFullPath());
                } else {
                    result.add(project.getFolder(path).getFullPath());
                }
            }
            return result;
        }

    }

    protected static class MatchAnySearchPattern extends SearchPattern {

        public MatchAnySearchPattern() {
            super(SearchPattern.RULE_PATTERN_MATCH);
        }

        @Override
        public void setPattern(final String stringPattern) {
            if ("".equals(stringPattern)) {
                super.setPattern(stringPattern);
            } else {
                super.setPattern("*" + stringPattern + "*");
            }
        }

    }

    /**
     * Filters resources using pattern and showDerived flag. It overrides
     * ItemsFilter.
     */
    protected class ModuleFilter extends ItemsFilter {

        private final IContainer filterContainer;
        private final int filterTypeMask;

        /**
         * Creates new ResourceFilter instance
         * 
         * @param container
         * @param showDerived
         *            flag which determine showing derived elements
         * @param typeMask
         */
        public ModuleFilter(final IContainer container, final int typeMask) {
            super(new MatchAnySearchPattern());
            filterContainer = container;
            filterTypeMask = typeMask;
        }

        /**
         * Creates new ResourceFilter instance
         */
        public ModuleFilter() {
            super();
            filterContainer = container;
            filterTypeMask = typeMask;
        }

        /**
         * @param item
         *            Must be instance of IResource, otherwise
         *            <code>false</code> will be returned.
         * @see org.eclipse.ui.dialogs.FilteredItemsSelectionDialog.ItemsFilter#isConsistentItem(java.lang.Object)
         */
        @Override
        public boolean isConsistentItem(final Object item) {
            if (item instanceof String) {
                return true;
            }
            if (!(item instanceof IResource)) {
                return false;
            }
            final IResource resource = (IResource) item;
            if (filterContainer.findMember(resource.getFullPath()) != null) {
                return true;
            }
            return false;
        }

        /**
         * @param item
         *            Must be instance of IResource, otherwise
         *            <code>false</code> will be returned.
         * @see org.eclipse.ui.dialogs.FilteredItemsSelectionDialog.ItemsFilter#matchItem(java.lang.Object)
         */
        @Override
        public boolean matchItem(final Object item) {
            if (item instanceof String) {
                final Path path = new Path((String) item);
                return matches(path.lastSegment().toString());
            }
            if (!(item instanceof IResource)) {
                return false;
            }
            final IResource resource = (IResource) item;
            if ((filterTypeMask & resource.getType()) == 0) {
                return false;
            }
            if (matches(resource.getName())) {
                return !resource.getResourceAttributes().isSymbolicLink();
            }
            return false;
        }

        @Override
        public boolean isSubFilter(final ItemsFilter filter) {
            if (!super.isSubFilter(filter)) {
                return false;
            }
            if (filter instanceof ModuleFilter) {
                return true;
            }
            return false;
        }

        @Override
        public boolean equalsFilter(final ItemsFilter iFilter) {
            if (!super.equalsFilter(iFilter)) {
                return false;
            }
            if (iFilter instanceof ModuleFilter) {
                return true;
            }
            return false;
        }

    }

    /**
     * Extends the <code>SelectionHistory</code>, providing support for
     * <code>OpenTypeHistory</code>.
     */
    protected class ModuleSelectionHistory extends SelectionHistory {

        /**
         * Creates new instance of TypeSelectionHistory
         */
        public ModuleSelectionHistory() {
            super();
        }

        @Override
        public synchronized void accessed(final Object object) {
            super.accessed(object);
        }

        @Override
        public synchronized boolean remove(final Object element) {
            // OpenModuleHistory.getInstance().remove((TypeNameMatch) element);
            return super.remove(element);
        }

        @Override
        public void load(final IMemento memento) {
            // TypeNameMatch[] types = OpenTypeHistory.getInstance()
            // .getTypeInfos();
            //
            // for (int i = types.length - 1; i >= 0; i--) { // see
            // // https://bugs.eclipse.org/bugs/show_bug.cgi?id=205314
            // TypeNameMatch type = types[i];
            // accessed(type);
            // }
        }

        @Override
        public void save(final IMemento memento) {
            persistHistory();
        }

        /**
         * Stores contents of the local history into persistent history
         * container.
         */
        private synchronized void persistHistory() {
            // if (getReturnCode() == OK) {
            // Object[] items = getHistoryItems();
            // for (int i = 0; i < items.length; i++) {
            // OpenTypeHistory.getInstance().accessed(
            // (TypeNameMatch) items[i]);
            // }
            // }
        }

        @Override
        protected Object restoreItemFromMemento(final IMemento element) {
            return null;
        }

        @Override
        protected void storeItemToMemento(final Object item,
                final IMemento element) {

        }

    }

}
