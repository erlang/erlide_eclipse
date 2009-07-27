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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.resources.IContainer;
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
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.viewers.DelegatingStyledCellLabelProvider.IStyledLabelProvider;
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
import org.eclipse.ui.internal.WorkbenchMessages;
import org.eclipse.ui.internal.ide.IDEWorkbenchMessages;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.statushandlers.StatusManager;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;

/**
 * Shows a list of resources to the user with a text entry field for a string
 * pattern used to filter the list of resources.
 * 
 * @since 3.3
 */
public class FilteredModulesSelectionDialog extends
		FilteredItemsSelectionDialog {

	private static final String DIALOG_SETTINGS = "org.eclipse.ui.dialogs.FilteredResourcesSelectionDialog"; //$NON-NLS-1$
	private static final String WORKINGS_SET_SETTINGS = "WorkingSet"; //$NON-NLS-1$
	private static final String SHOW_DERIVED = "ShowDerived"; //$NON-NLS-1$

	private ShowDerivedModulesAction showDerivedModulesAction;
	private final ModuleItemLabelProvider moduleItemLabelProvider;
	private final ModuleItemDetailsLabelProvider moduleItemDetailsLabelProvider;
	private WorkingSetFilterActionGroup workingSetFilterActionGroup;
	private final CustomWorkingSetFilter workingSetFilter = new CustomWorkingSetFilter();
	private String title;
	private final IContainer container;
	private final int typeMask;
	private boolean isDerived;

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
	public FilteredModulesSelectionDialog(Shell shell, boolean multi,
			IContainer container, int typesMask) {
		super(shell, multi);

		setSelectionHistory(new ModuleSelectionHistory());

		setTitle("Open Module");
		PlatformUI.getWorkbench().getHelpSystem().setHelp(shell,
				IErlangHelpContextIds.OPEN_MODULE_DIALOG);

		this.container = container;
		this.typeMask = typesMask;

		moduleItemLabelProvider = new ModuleItemLabelProvider();

		moduleItemDetailsLabelProvider = new ModuleItemDetailsLabelProvider();

		setListLabelProvider(moduleItemLabelProvider);
		setDetailsLabelProvider(moduleItemDetailsLabelProvider);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.dialogs.SelectionDialog#setTitle(java.lang.String)
	 */
	@Override
	public void setTitle(String title) {
		super.setTitle(title);
		this.title = title;
	}

	/**
	 * Adds or replaces subtitle of the dialog
	 * 
	 * @param text
	 *            the new subtitle
	 */
	private void setSubtitle(String text) {
		if (text == null || text.length() == 0) {
			getShell().setText(title);
		} else {
			getShell().setText(title + " - " + text); //$NON-NLS-1$
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.dialogs.FilteredItemsSelectionDialog#getDialogSettings()
	 */
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.dialogs.FilteredItemsSelectionDialog#storeDialog(org.eclipse
	 * .jface.dialogs.IDialogSettings)
	 */
	@Override
	protected void storeDialog(IDialogSettings settings) {
		super.storeDialog(settings);

		settings.put(SHOW_DERIVED, showDerivedModulesAction.isChecked());

		XMLMemento memento = XMLMemento.createWriteRoot("workingSet"); //$NON-NLS-1$
		workingSetFilterActionGroup.saveState(memento);
		workingSetFilterActionGroup.dispose();
		StringWriter writer = new StringWriter();
		try {
			memento.save(writer);
			settings.put(WORKINGS_SET_SETTINGS, writer.getBuffer().toString());
		} catch (IOException e) {
			StatusManager.getManager().handle(
					new Status(IStatus.ERROR, ErlideUIPlugin.PLUGIN_ID,
							IStatus.ERROR, "", e)); //$NON-NLS-1$
			// don't do anything. Simply don't store the settings
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.dialogs.FilteredItemsSelectionDialog#restoreDialog(org
	 * .eclipse.jface.dialogs.IDialogSettings)
	 */
	@Override
	protected void restoreDialog(IDialogSettings settings) {
		super.restoreDialog(settings);

		boolean showDerived = settings.getBoolean(SHOW_DERIVED);
		showDerivedModulesAction.setChecked(showDerived);
		this.isDerived = showDerived;

		String setting = settings.get(WORKINGS_SET_SETTINGS);
		if (setting != null) {
			try {
				IMemento memento = XMLMemento.createReadRoot(new StringReader(
						setting));
				workingSetFilterActionGroup.restoreState(memento);
			} catch (WorkbenchException e) {
				StatusManager.getManager().handle(
						new Status(IStatus.ERROR, ErlideUIPlugin.PLUGIN_ID,
								IStatus.ERROR, "", e)); //$NON-NLS-1$
				// don't do anything. Simply don't restore the settings
			}
		}

		addListFilter(workingSetFilter);

		applyFilter();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.dialogs.FilteredItemsSelectionDialog#fillViewMenu(org.
	 * eclipse.jface.action.IMenuManager)
	 */
	@Override
	protected void fillViewMenu(IMenuManager menuManager) {
		super.fillViewMenu(menuManager);

		showDerivedModulesAction = new ShowDerivedModulesAction();
		menuManager.add(showDerivedModulesAction);

		workingSetFilterActionGroup = new WorkingSetFilterActionGroup(
				getShell(), new IPropertyChangeListener() {
					public void propertyChange(PropertyChangeEvent event) {
						String property = event.getProperty();

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
								IWorkbenchWindow window = PlatformUI
										.getWorkbench()
										.getActiveWorkbenchWindow();

								if (window != null) {
									IWorkbenchPage page = window
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.dialogs.FilteredItemsSelectionDialog#createExtendedContentArea
	 * (org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createExtendedContentArea(Composite parent) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.dialogs.SelectionDialog#getResult()
	 */
	@Override
	public Object[] getResult() {
		Object[] result = super.getResult();

		if (result == null) {
			return null;
		}

		List resultToReturn = new ArrayList();

		for (int i = 0; i < result.length; i++) {
			if (result[i] instanceof IResource) {
				resultToReturn.add((result[i]));
			}
		}

		return resultToReturn.toArray();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.window.Window#open()
	 */
	@Override
	public int open() {
		if (getInitialPattern() == null) {
			IWorkbenchWindow window = PlatformUI.getWorkbench()
					.getActiveWorkbenchWindow();
			if (window != null) {
				ISelection selection = window.getSelectionService()
						.getSelection();
				if (selection instanceof ITextSelection) {
					String text = ((ITextSelection) selection).getText();
					if (text != null) {
						text = text.trim();
						if (text.length() > 0) {
							IWorkspace workspace = ResourcesPlugin
									.getWorkspace();
							IStatus result = workspace.validateName(text,
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.dialogs.FilteredItemsSelectionDialog#getElementName(java
	 * .lang.Object)
	 */
	@Override
	public String getElementName(Object item) {
		IResource resource = (IResource) item;
		return resource.getName();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.dialogs.FilteredItemsSelectionDialog#validateItem(java
	 * .lang.Object)
	 */
	@Override
	protected IStatus validateItem(Object item) {
		return new Status(IStatus.OK, ErlideUIPlugin.PLUGIN_ID, 0, "", null); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.dialogs.FilteredItemsSelectionDialog#createFilter()
	 */
	@Override
	protected ItemsFilter createFilter() {
		return new ModuleFilter(container, isDerived, typeMask);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.dialogs.FilteredItemsSelectionDialog#applyFilter()
	 */
	@Override
	protected void applyFilter() {
		super.applyFilter();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.dialogs.FilteredItemsSelectionDialog#getItemsComparator()
	 */
	@Override
	protected Comparator getItemsComparator() {
		return new Comparator() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.util.Comparator#compare(java.lang.Object,
			 * java.lang.Object)
			 */
			public int compare(Object o1, Object o2) {
				Collator collator = Collator.getInstance();
				IResource resource1 = (IResource) o1;
				IResource resource2 = (IResource) o2;
				String s1 = resource1.getName();
				String s2 = resource2.getName();
				int comparability = collator.compare(s1, s2);
				if (comparability == 0) {
					IPath p1 = resource1.getFullPath();
					IPath p2 = resource2.getFullPath();
					int c1 = p1.segmentCount();
					int c2 = p2.segmentCount();
					for (int i = 0; i < c1 && i < c2; i++) {
						comparability = collator.compare(p1.segment(i), p2
								.segment(i));
						if (comparability != 0) {
							return comparability;
						}
					}
					comparability = c2 - c1;
				}

				return comparability;
			}
		};
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.dialogs.FilteredItemsSelectionDialog#fillContentProvider
	 * (org
	 * .eclipse.ui.dialogs.FilteredItemsSelectionDialog.AbstractContentProvider,
	 * org.eclipse.ui.dialogs.FilteredItemsSelectionDialog.ItemsFilter,
	 * org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	protected void fillContentProvider(AbstractContentProvider contentProvider,
			ItemsFilter itemsFilter, IProgressMonitor progressMonitor)
			throws CoreException {
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
	 * Sets the derived flag on the ResourceFilter instance
	 */
	private class ShowDerivedModulesAction extends Action {

		/**
		 * Creates a new instance of the action.
		 */
		public ShowDerivedModulesAction() {
			super(
					IDEWorkbenchMessages.FilteredResourcesSelectionDialog_showDerivedResourcesAction,
					IAction.AS_CHECK_BOX);
		}

		@Override
		public void run() {
			FilteredModulesSelectionDialog.this.isDerived = isChecked();
			applyFilter();
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
		private final ListenerList listeners = new ListenerList();

		WorkbenchLabelProvider provider = new WorkbenchLabelProvider();

		/**
		 * Creates a new instance of the class
		 */
		public ModuleItemLabelProvider() {
			super();
			provider.addListener(this);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
		 */
		@Override
		public Image getImage(Object element) {
			if (!(element instanceof IResource)) {
				return super.getImage(element);
			}

			IResource res = (IResource) element;

			return provider.getImage(res);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
		 */
		@Override
		public String getText(Object element) {
			if (!(element instanceof IResource)) {
				return super.getText(element);
			}

			IResource res = (IResource) element;

			String str = res.getName();

			// extra info for duplicates
			if (isDuplicateElement(element)) {
				str = str
						+ " - " + res.getParent().getFullPath().makeRelative().toString(); //$NON-NLS-1$
			}

			return str;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @seeorg.eclipse.jface.viewers.DelegatingStyledCellLabelProvider.
		 * IStyledLabelProvider#getStyledText(java.lang.Object)
		 */
		public StyledString getStyledText(Object element) {
			if (!(element instanceof IResource)) {
				return new StyledString(super.getText(element));
			}

			IResource res = (IResource) element;

			StyledString str = new StyledString(res.getName());

			// extra info for duplicates
			if (isDuplicateElement(element)) {
				str.append(" - ", StyledString.QUALIFIER_STYLER); //$NON-NLS-1$
				str.append(res.getParent().getFullPath().makeRelative()
						.toString(), StyledString.QUALIFIER_STYLER);
			}
			return str;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.jface.viewers.LabelProvider#dispose()
		 */
		@Override
		public void dispose() {
			provider.removeListener(this);
			provider.dispose();

			super.dispose();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse
		 * .jface.viewers.ILabelProviderListener)
		 */
		@Override
		public void addListener(ILabelProviderListener listener) {
			listeners.add(listener);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jface.viewers.LabelProvider#removeListener(org.eclipse
		 * .jface.viewers.ILabelProviderListener)
		 */
		@Override
		public void removeListener(ILabelProviderListener listener) {
			listeners.remove(listener);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jface.viewers.ILabelProviderListener#labelProviderChanged
		 * (org.eclipse.jface.viewers.LabelProviderChangedEvent)
		 */
		public void labelProviderChanged(LabelProviderChangedEvent event) {
			Object[] l = listeners.getListeners();
			for (int i = 0; i < listeners.size(); i++) {
				((ILabelProviderListener) l[i]).labelProviderChanged(event);
			}
		}

	}

	/**
	 * A label provider for details of ResourceItem objects.
	 */
	private class ModuleItemDetailsLabelProvider extends
			ModuleItemLabelProvider {
		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
		 */
		@Override
		public Image getImage(Object element) {
			if (!(element instanceof IResource)) {
				return super.getImage(element);
			}

			IResource parent = ((IResource) element).getParent();
			return provider.getImage(parent);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
		 */
		@Override
		public String getText(Object element) {
			if (!(element instanceof IResource)) {
				return super.getText(element);
			}

			IResource parent = ((IResource) element).getParent();

			if (parent.getType() == IResource.ROOT) {
				// Get readable name for workspace root ("Workspace"), without
				// duplicating language-specific string here.
				return null;
			}

			return parent.getFullPath().makeRelative().toString();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jface.viewers.ILabelProviderListener#labelProviderChanged
		 * (org.eclipse.jface.viewers.LabelProviderChangedEvent)
		 */
		@Override
		public void labelProviderChanged(LabelProviderChangedEvent event) {
			Object[] l = super.listeners.getListeners();
			for (int i = 0; i < super.listeners.size(); i++) {
				((ILabelProviderListener) l[i]).labelProviderChanged(event);
			}
		}
	}

	/**
	 * Viewer filter which filters resources due to current working set
	 */
	private class CustomWorkingSetFilter extends ViewerFilter {
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
		public void setWorkingSet(IWorkingSet workingSet) {
			resourceWorkingSetFilter.setWorkingSet(workingSet);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jface.viewers.ViewerFilter#select(org.eclipse.jface.viewers
		 * .Viewer, java.lang.Object, java.lang.Object)
		 */
		@Override
		public boolean select(Viewer viewer, Object parentElement,
				Object element) {
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

		private final ModuleFilter resourceFilter;

		private final IProgressMonitor progressMonitor;

		private final List projects;

		/**
		 * Creates new ResourceProxyVisitor instance.
		 * 
		 * @param contentProvider
		 * @param resourceFilter
		 * @param progressMonitor
		 * @throws CoreException
		 */
		public ModuleProxyVisitor(AbstractContentProvider contentProvider,
				ModuleFilter resourceFilter, IProgressMonitor progressMonitor)
				throws CoreException {
			super();
			this.proxyContentProvider = contentProvider;
			this.resourceFilter = resourceFilter;
			this.progressMonitor = progressMonitor;
			IResource[] resources = container.members();
			this.projects = new ArrayList(Arrays.asList(resources));

			if (progressMonitor != null) {
				progressMonitor
						.beginTask(
								WorkbenchMessages.FilteredItemsSelectionDialog_searchJob_taskName,
								projects.size());
			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.core.resources.IResourceProxyVisitor#visit(org.eclipse
		 * .core.resources.IResourceProxy)
		 */
		public boolean visit(IResourceProxy proxy) {

			if (progressMonitor.isCanceled()) {
				return false;
			}

			IResource resource = proxy.requestResource();

			if (this.projects.remove((resource.getProject()))
					|| this.projects.remove((resource))) {
				progressMonitor.worked(1);
			}

			proxyContentProvider.add(resource, resourceFilter);

			if (resource.getType() == IResource.FOLDER && resource.isDerived()
					&& !resourceFilter.isShowDerived()) {

				return false;
			}

			if (resource.getType() == IResource.FILE) {
				return false;
			}

			return true;
		}
	}

	/**
	 * Filters resources using pattern and showDerived flag. It overrides
	 * ItemsFilter.
	 */
	protected class ModuleFilter extends ItemsFilter {

		private boolean showDerived = false;

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
		public ModuleFilter(IContainer container, boolean showDerived,
				int typeMask) {
			super();
			this.filterContainer = container;
			this.showDerived = showDerived;
			this.filterTypeMask = typeMask;
		}

		/**
		 * Creates new ResourceFilter instance
		 */
		public ModuleFilter() {
			super();
			this.filterContainer = container;
			this.showDerived = isDerived;
			this.filterTypeMask = typeMask;
		}

		/**
		 * @param item
		 *            Must be instance of IResource, otherwise
		 *            <code>false</code> will be returned.
		 * @see org.eclipse.ui.dialogs.FilteredItemsSelectionDialog.ItemsFilter#isConsistentItem(java.lang.Object)
		 */
		@Override
		public boolean isConsistentItem(Object item) {
			if (!(item instanceof IResource)) {
				return false;
			}
			IResource resource = (IResource) item;
			if (this.filterContainer.findMember(resource.getFullPath()) != null) {
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
		public boolean matchItem(Object item) {
			if (!(item instanceof IResource)) {
				return false;
			}
			IResource resource = (IResource) item;
			if ((!this.showDerived && resource.isDerived())
					|| ((this.filterTypeMask & resource.getType()) == 0)) {
				return false;
			}
			return matches(resource.getName());
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @seeorg.eclipse.ui.dialogs.FilteredItemsSelectionDialog.ItemsFilter#
		 * isSubFilter
		 * (org.eclipse.ui.dialogs.FilteredItemsSelectionDialog.ItemsFilter)
		 */
		@Override
		public boolean isSubFilter(ItemsFilter filter) {
			if (!super.isSubFilter(filter)) {
				return false;
			}
			if (filter instanceof ModuleFilter) {
				if (this.showDerived == ((ModuleFilter) filter).showDerived) {
					return true;
				}
			}
			return false;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @seeorg.eclipse.ui.dialogs.FilteredItemsSelectionDialog.ItemsFilter#
		 * equalsFilter
		 * (org.eclipse.ui.dialogs.FilteredItemsSelectionDialog.ItemsFilter)
		 */
		@Override
		public boolean equalsFilter(ItemsFilter iFilter) {
			if (!super.equalsFilter(iFilter)) {
				return false;
			}
			if (iFilter instanceof ModuleFilter) {
				if (this.showDerived == ((ModuleFilter) iFilter).showDerived) {
					return true;
				}
			}
			return false;
		}

		/**
		 * Check show derived flag for a filter
		 * 
		 * @return true if filter allow derived resources false if not
		 */
		public boolean isShowDerived() {
			return showDerived;
		}

	}

	/**
	 * <code>ResourceSelectionHistory</code> provides behavior specific to
	 * resources - storing and restoring <code>IResource</code>s state to/from
	 * XML (memento).
	 */
	private class ModuleSelectionHistory extends SelectionHistory {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.ui.dialogs.FilteredItemsSelectionDialog.SelectionHistory
		 * #restoreItemFromMemento(org.eclipse.ui.IMemento)
		 */
		@Override
		protected Object restoreItemFromMemento(IMemento element) {
			return null;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.ui.dialogs.FilteredItemsSelectionDialog.SelectionHistory
		 * #storeItemToMemento(java.lang.Object, org.eclipse.ui.IMemento)
		 */
		@Override
		protected void storeItemToMemento(Object item, IMemento element) {
		}

	}

}
