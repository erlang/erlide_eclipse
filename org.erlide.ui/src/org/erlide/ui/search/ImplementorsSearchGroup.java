package org.erlide.ui.search;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.search.ui.IContextMenuConstants;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.erlide.ui.ErlideUIConstants;
import org.erlide.ui.actions.SelectionDispatchAction;
import org.erlide.ui.editors.erl.ErlangEditor;

public class ImplementorsSearchGroup extends ActionGroup {

	private static final String MENU_TEXT = "Declarations";

	private final IWorkbenchSite fSite;
	private ErlangEditor fEditor;
	private IActionBars fActionBars;

	private final String fGroupId;

	private FindImplementorsAction fFindImplementorsAction;
	private FindImplementorsInProjectAction fFindImplementorsInProjectAction;

	// private FindReferencesInWorkingSetAction
	// fFindReferencesInWorkingSetAction;

	/**
	 * Creates a new <code>ReferencesSearchGroup</code>. The group requires that
	 * the selection provided by the site's selection provider is of type <code>
	 * org.eclipse.jface.viewers.IStructuredSelection</code>
	 * .
	 * 
	 * @param site
	 *            the view part that owns this action group
	 */
	public ImplementorsSearchGroup(final IWorkbenchSite site) {
		fSite = site;
		fGroupId = IContextMenuConstants.GROUP_SEARCH;

		fFindImplementorsAction = new FindImplementorsAction(site);
		fFindImplementorsAction
				.setActionDefinitionId("org.erlide.ui.search.implementors.in.workspace");

		fFindImplementorsInProjectAction = new FindImplementorsInProjectAction(
				site);
		fFindImplementorsInProjectAction
				.setActionDefinitionId("org.erlide.ui.search.implementors.in.project");

		// fFindReferencesInWorkingSetAction = new
		// FindReferencesInWorkingSetAction(
		// site);
		// fFindReferencesInWorkingSetAction
		// .setActionDefinitionId("org.erlide.ui.search.references.in.workset");

		// register the actions as selection listeners
		final ISelectionProvider provider = fSite.getSelectionProvider();
		final ISelection selection = provider.getSelection();
		registerAction(fFindImplementorsAction, provider, selection);
		registerAction(fFindImplementorsInProjectAction, provider, selection);
		// registerAction(fFindReferencesInWorkingSetAction, provider,
		// selection);
	}

	/**
	 * Note: This constructor is for internal use only. Clients should not call
	 * this constructor.
	 * 
	 * @param editor
	 *            the Erlang editor
	 */
	public ImplementorsSearchGroup(final ErlangEditor editor) {
		Assert.isNotNull(editor);
		fEditor = editor;
		fSite = fEditor.getSite();
		fGroupId = ITextEditorActionConstants.GROUP_FIND;

		fFindImplementorsAction = new FindImplementorsAction(editor);
		fFindImplementorsAction
				.setActionDefinitionId("org.erlide.ui.search.implementors.in.workspace");
		fEditor.setAction(
				"SearchReferencesInWorkspace", fFindImplementorsAction); //$NON-NLS-1$

		fFindImplementorsInProjectAction = new FindImplementorsInProjectAction(
				fEditor);
		fFindImplementorsInProjectAction
				.setActionDefinitionId("org.erlide.ui.search.implementors.in.project");
		fEditor.setAction(
				"SearchReferencesInProject", fFindImplementorsInProjectAction); //$NON-NLS-1$

		// fFindReferencesInWorkingSetAction = new
		// FindReferencesInWorkingSetAction(
		// fEditor);
		// fFindReferencesInWorkingSetAction
		// .setActionDefinitionId("org.erlide.ui.search.references.in.workset");
		// fEditor
		// .setAction(
		// "SearchReferencesInWorkingSet", fFindReferencesInWorkingSetAction);
		// //$NON-NLS-1$
	}

	private void registerAction(final SelectionDispatchAction action,
			final ISelectionProvider provider, final ISelection selection) {
		action.update(selection);
		provider.addSelectionChangedListener(action);
	}

	/**
	 * Note: this method is for internal use only. Clients should not call this
	 * method.
	 * 
	 * @return the menu label
	 */
	protected String getName() {
		return MENU_TEXT;
	}

	/*
	 * (non-Javadoc) Method declared in ActionGroup
	 */
	@Override
	public void fillActionBars(final IActionBars actionBars) {
		Assert.isNotNull(actionBars);
		super.fillActionBars(actionBars);
		fActionBars = actionBars;
		updateGlobalActionHandlers();
	}

	private void addAction(final IAction action, final IMenuManager manager) {
		if (action.isEnabled()) {
			manager.add(action);
		}
	}

	// private void addWorkingSetAction(final IWorkingSet[] workingSets,
	// final IMenuManager manager) {
	// FindAction action;
	// if (fEditor != null) {
	// action = new WorkingSetFindAction(fEditor,
	// new FindReferencesInWorkingSetAction(fEditor, workingSets),
	// SearchUtil.toString(workingSets));
	// } else {
	// action = new WorkingSetFindAction(fSite,
	// new FindReferencesInWorkingSetAction(fSite, workingSets),
	// SearchUtil.toString(workingSets));
	// }
	// action.update(getContext().getSelection());
	// addAction(action, manager);
	// }

	/*
	 * (non-Javadoc) Method declared on ActionGroup.
	 */
	@Override
	public void fillContextMenu(final IMenuManager manager) {
		final MenuManager erlangSearchMM = new MenuManager(getName(),
				IContextMenuConstants.GROUP_SEARCH);
		addAction(fFindImplementorsAction, erlangSearchMM);
		addAction(fFindImplementorsInProjectAction, erlangSearchMM);
		// addAction(fFindReferencesInHierarchyAction, javaSearchMM);

		// erlangSearchMM.add(new Separator());
		//
		// final Iterator iter =
		// SearchUtil.getLRUWorkingSets().sortedIterator();
		// while (iter.hasNext()) {
		// addWorkingSetAction((IWorkingSet[]) iter.next(), erlangSearchMM);
		// }
		// addAction(fFindReferencesInWorkingSetAction, erlangSearchMM);
		//
		if (!erlangSearchMM.isEmpty()) {
			manager.appendToGroup(fGroupId, erlangSearchMM);
		}
	}

	/*
	 * Overrides method declared in ActionGroup
	 */
	@Override
	public void dispose() {
		final ISelectionProvider provider = fSite.getSelectionProvider();
		if (provider != null) {
			disposeAction(fFindImplementorsAction, provider);
			disposeAction(fFindImplementorsInProjectAction, provider);
			// disposeAction(fFindReferencesInWorkingSetAction, provider);
		}
		fFindImplementorsAction = null;
		fFindImplementorsInProjectAction = null;
		// fFindReferencesInWorkingSetAction = null;
		updateGlobalActionHandlers();
		super.dispose();
	}

	private void updateGlobalActionHandlers() {
		if (fActionBars != null) {
			fActionBars.setGlobalActionHandler(
					ErlideUIConstants.FIND_IMPLEMENTORS_IN_WORKSPACE,
					fFindImplementorsAction);
			fActionBars.setGlobalActionHandler(
					ErlideUIConstants.FIND_IMPLEMENTORS_IN_PROJECT,
					fFindImplementorsInProjectAction);
			// fActionBars.setGlobalActionHandler(
			// IErlideUIConstants.FIND_REFERENCES_IN_WORKING_SET,
			// fFindReferencesInWorkingSetAction);
		}
	}

	private void disposeAction(final ISelectionChangedListener action,
			final ISelectionProvider provider) {
		if (action != null) {
			provider.removeSelectionChangedListener(action);
		}
	}

}
