/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.search;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.search.ui.ISearchPage;
import org.eclipse.search.ui.ISearchPageContainer;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkingSet;
import org.eclipse.ui.dialogs.SearchPattern;
import org.erlide.core.erlang.ErlScanner;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.search.ErlangExternalFunctionCallRef;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.Tuple;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BuildBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.editors.erl.ErlangEditor;

import erlang.ErlideOpen;
import erlang.OpenResult;

public class ErlangSearchPage extends DialogPage implements ISearchPage {

	private static class SearchPatternData {
		private final int searchFor;
		private final int limitTo;
		private final String pattern;
		private final boolean isCaseSensitive;
		private ErlangExternalFunctionCallRef ref;
		// private final int includeMask;
		private final int scope;
		private final IWorkingSet[] workingSets;

		public SearchPatternData(final int searchFor, final int limitTo,
				final boolean isCaseSensitive, final String pattern,
				final ErlangExternalFunctionCallRef ref) { // , final int
			// includeMask) {
			this(searchFor, limitTo, pattern, isCaseSensitive, ref,
					ISearchPageContainer.WORKSPACE_SCOPE, null); // ,
			// includeMask);
		}

		public SearchPatternData(final int searchFor, final int limitTo,
				final String pattern, final boolean isCaseSensitive,
				final ErlangExternalFunctionCallRef ref, final int scope,
				final IWorkingSet[] workingSets) {// , final int includeMask)
			// {
			this.searchFor = searchFor;
			this.limitTo = limitTo;
			this.pattern = pattern;
			this.isCaseSensitive = isCaseSensitive;
			this.scope = scope;
			this.workingSets = workingSets;
			// this.includeMask = includeMask;

			setRef(ref);
		}

		public void setRef(final ErlangExternalFunctionCallRef ref) {
			this.ref = ref;
		}

		public boolean isCaseSensitive() {
			return isCaseSensitive;
		}

		public ErlangExternalFunctionCallRef getRef() {
			return ref;
		}

		public int getLimitTo() {
			return limitTo;
		}

		public String getPattern() {
			return pattern;
		}

		public int getScope() {
			return scope;
		}

		public int getSearchFor() {
			return searchFor;
		}

		public IWorkingSet[] getWorkingSets() {
			return workingSets;
		}

		// public int getIncludeMask() {
		// return includeMask;
		// }

		public void store(final IDialogSettings settings) {
			settings.put("searchFor", searchFor); //$NON-NLS-1$
			settings.put("scope", scope); //$NON-NLS-1$
			settings.put("pattern", pattern); //$NON-NLS-1$
			settings.put("limitTo", limitTo); //$NON-NLS-1$
			settings.put("ref", ref != null ? ref.toString() : ""); //$NON-NLS-1$ //$NON-NLS-2$
			settings.put("isCaseSensitive", isCaseSensitive); //$NON-NLS-1$
			if (workingSets != null) {
				final String[] wsIds = new String[workingSets.length];
				for (int i = 0; i < workingSets.length; i++) {
					wsIds[i] = workingSets[i].getName();
				}
				settings.put("workingSets", wsIds); //$NON-NLS-1$
			} else {
				settings.put("workingSets", new String[0]); //$NON-NLS-1$
			}
			// settings.put("includeMask", includeMask); //$NON-NLS-1$
		}

		public static SearchPatternData create(final IDialogSettings settings) {
			// final String pattern = settings.get("pattern"); //$NON-NLS-1$
			// if (pattern.length() == 0) {
			// return null;
			// }
			// IErlElement elem = null;
			// final String handleId = settings.get("javaElement");
			// //$NON-NLS-1$
			// if (handleId != null && handleId.length() > 0) {
			// final IErlElement restored = JavaCore.create(handleId);
			// if (restored != null && isSearchableType(restored)
			// && restored.exists()) {
			// elem = restored;
			// }
			// }
			// final String[] wsIds = settings.getArray("workingSets");
			// //$NON-NLS-1$
			// IWorkingSet[] workingSets = null;
			// if (wsIds != null && wsIds.length > 0) {
			// final IWorkingSetManager workingSetManager = PlatformUI
			// .getWorkbench().getWorkingSetManager();
			// workingSets = new IWorkingSet[wsIds.length];
			// for (int i = 0; workingSets != null && i < wsIds.length; i++) {
			// workingSets[i] = workingSetManager.getWorkingSet(wsIds[i]);
			// if (workingSets[i] == null) {
			// workingSets = null;
			// }
			// }
			// }
			//
			// try {
			// final int searchFor = settings.getInt("searchFor"); //$NON-NLS-1$
			// final int scope = settings.getInt("scope"); //$NON-NLS-1$
			// final int limitTo = settings.getInt("limitTo"); //$NON-NLS-1$
			// final boolean isCaseSensitive = settings
			// .getBoolean("isCaseSensitive"); //$NON-NLS-1$
			//
			// int includeMask;
			// if (settings.get("includeMask") != null) { //$NON-NLS-1$
			// includeMask = settings.getInt("includeMask"); //$NON-NLS-1$
			// } else {
			// includeMask = JavaSearchScopeFactory.NO_JRE;
			// if (settings.get("includeJRE") == null ? forceIncludeAll(limitTo,
			// elem) :
			// settings.getBoolean("includeJRE")) { //$NON-NLS-1$ //$NON-NLS-2$
			// includeMask = JavaSearchScopeFactory.ALL;
			// }
			// }
			// return new SearchPatternData(searchFor, limitTo, pattern,
			// isCaseSensitive, elem, scope, workingSets, includeMask);
			// } catch (final NumberFormatException e) {
			// return null;
			// }
			return null;
		}
	}

	// search for
	private final static int MODULE = IErlSearchConstants.MODULE;
	private final static int FUNCTION = IErlSearchConstants.FUNCTION;
	private final static int RECORD = IErlSearchConstants.RECORD;
	private final static int MACRO = IErlSearchConstants.MACRO;

	// limit to
	private final static int DECLARATIONS = IErlSearchConstants.DECLARATIONS;
	private final static int REFERENCES = IErlSearchConstants.REFERENCES;
	private final static int ALL_OCCURRENCES = IErlSearchConstants.ALL_OCCURRENCES;

	public static final String PARTICIPANT_EXTENSION_POINT = "org.eclipse.jdt.ui.queryParticipants"; //$NON-NLS-1$

	public static final String EXTENSION_POINT_ID = "org.eclipse.jdt.ui.JavaSearchPage"; //$NON-NLS-1$

	private static final int HISTORY_SIZE = 12;

	// Dialog store id constants
	private final static String PAGE_NAME = "JavaSearchPage"; //$NON-NLS-1$
	private final static String STORE_CASE_SENSITIVE = "CASE_SENSITIVE"; //$NON-NLS-1$
	// private final static String STORE_INCLUDE_MASK = "INCLUDE_MASK";
	// //$NON-NLS-1$
	private final static String STORE_HISTORY = "HISTORY"; //$NON-NLS-1$
	private final static String STORE_HISTORY_SIZE = "HISTORY_SIZE"; //$NON-NLS-1$

	private final List<SearchPatternData> fPreviousSearchPatterns;

	private SearchPatternData fInitialData;
	private ErlangExternalFunctionCallRef fRef;
	private boolean fFirstTime = true;
	private IDialogSettings fDialogSettings;
	private boolean fIsCaseSensitive;

	private Combo fPattern;
	private ISearchPageContainer fContainer;
	private Button fCaseSensitive;

	private Button[] fSearchFor;
	private Button[] fLimitTo;

	// private Button[] fIncludeMasks;

	/**
	 * 
	 */
	public ErlangSearchPage() {
		fPreviousSearchPatterns = new ArrayList<SearchPatternData>();
	}

	// ---- Action Handling ------------------------------------------------

	public boolean performAction() {
		return performNewSearch();
	}

	private boolean performNewSearch() {
		final SearchPatternData data = getPatternData();

		// Setup search scope
		String[] scope = null;
		switch (getContainer().getSelectedScope()) {
		case ISearchPageContainer.WORKSPACE_SCOPE:
			scope = SearchUtil.getWorkspaceScope();
			break;
		case ISearchPageContainer.SELECTED_PROJECTS_SCOPE:
			final String[] projectNames = getContainer()
					.getSelectedProjectNames();
			scope = SearchUtil.getProjectsScope(projectNames);
			break;
		case ISearchPageContainer.SELECTION_SCOPE:
			scope = SearchUtil.getSelectionScope(getContainer().getSelection());
			// case ISearchPageContainer.WORKING_SET_SCOPE: {
			// final IWorkingSet[] workingSets = getContainer()
			// .getSelectedWorkingSets();
			// // should not happen - just to be sure
			// if (workingSets == null || workingSets.length < 1) {
			// return false;
			// }
			// scopeDescription = factory.getWorkingSetScopeDescription(
			// workingSets, includeMask);
			// scope = factory.createJavaSearchScope(workingSets, includeMask);
			// SearchUtil.updateLRUWorkingSets(workingSets);
			// }
		}

		final int searchFor = data.getSearchFor();
		final int limitTo = data.getLimitTo();

		final ErlSearchQuery query = new ErlSearchQuery(fRef, searchFor,
				limitTo, scope);
		NewSearchUI.runQueryInBackground(query);
		return true;
	}

	private int getLimitTo() {
		for (int i = 0; i < fLimitTo.length; i++) {
			final Button button = fLimitTo[i];
			if (button.getSelection()) {
				return getIntData(button);
			}
		}
		return -1;
	}

	private void setLimitTo(final int searchFor, final int limitTo) {
		// if (searchFor != TYPE && limitTo == IMPLEMENTORS) {
		// limitTo = REFERENCES;
		// }
		//
		// if (searchFor != FIELD
		// && (limitTo == READ_ACCESSES || limitTo == WRITE_ACCESSES)) {
		// limitTo = REFERENCES;
		// }

		for (int i = 0; i < fLimitTo.length; i++) {
			final Button button = fLimitTo[i];
			final int val = getIntData(button);
			button.setSelection(limitTo == val);

			switch (val) {
			case DECLARATIONS:
			case REFERENCES:
			case ALL_OCCURRENCES:
				button.setEnabled(true);
				break;
			// case IMPLEMENTORS:
			// button.setEnabled(searchFor == TYPE);
			// break;
			// case READ_ACCESSES:
			// case WRITE_ACCESSES:
			// button.setEnabled(searchFor == FIELD);
			// break;
			}
		}
		// return limitTo;
	}

	// private int getIncludeMask() {
	// int mask = 0;
	// for (int i = 0; i < fIncludeMasks.length; i++) {
	// final Button button = fIncludeMasks[i];
	// if (button.getSelection()) {
	// mask |= getIntData(button);
	// }
	// }
	// return mask;
	// }

	// private void setIncludeMask(final int includeMask, final int limitTo) {
	// for (int i = 0; i < fIncludeMasks.length; i++) {
	// final Button button = fIncludeMasks[i];
	// button.setSelection((includeMask & getIntData(button)) != 0);
	// }
	// }

	private String[] getPreviousSearchPatterns() {
		// Search results are not persistent
		final int patternCount = fPreviousSearchPatterns.size();
		final String[] patterns = new String[patternCount];
		for (int i = 0; i < patternCount; i++) {
			patterns[i] = fPreviousSearchPatterns.get(i).getPattern();
		}
		return patterns;
	}

	private int getSearchFor() {
		for (int i = 0; i < fSearchFor.length; i++) {
			final Button button = fSearchFor[i];
			if (button.getSelection()) {
				return getIntData(button);
			}
		}
		Assert.isTrue(false, "shouldNeverHappen"); //$NON-NLS-1$
		return -1;
	}

	private void setSearchFor(final int searchFor) {
		for (int i = 0; i < fSearchFor.length; i++) {
			final Button button = fSearchFor[i];
			button.setSelection(searchFor == getIntData(button));
		}
	}

	private int getIntData(final Button button) {
		return ((Integer) button.getData()).intValue();
	}

	private String getPattern() {
		return fPattern.getText();
	}

	private SearchPatternData findInPrevious(final String pattern) {
		for (final SearchPatternData i : fPreviousSearchPatterns) {
			if (pattern.equals(i.getPattern())) {
				return i;
			}
		}
		return null;
	}

	/**
	 * Return search pattern data and update previous searches. An existing
	 * entry will be updated.
	 * 
	 * @return the pattern data
	 */
	private SearchPatternData getPatternData() {
		final String pattern = getPattern();
		SearchPatternData match = findInPrevious(pattern);
		if (match != null) {
			fPreviousSearchPatterns.remove(match);
		}
		match = new SearchPatternData(getSearchFor(), getLimitTo(), pattern,
				fCaseSensitive.getSelection(), fRef, getContainer()
						.getSelectedScope(), getContainer()
						.getSelectedWorkingSets());// , getIncludeMask());

		fPreviousSearchPatterns.add(0, match); // insert on top
		return match;
	}

	/*
	 * Implements method from IDialogPage
	 */
	@Override
	public void setVisible(final boolean visible) {
		if (visible && fPattern != null) {
			if (fFirstTime) {
				fFirstTime = false;
				// Set item and text here to prevent page from resizing
				fPattern.setItems(getPreviousSearchPatterns());
				initSelections();
			}
			fPattern.setFocus();
		}
		updateOKStatus();
		super.setVisible(visible);
	}

	public boolean isValid() {
		return true;
	}

	// ---- Widget creation ------------------------------------------------

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets
	 *      .Composite)
	 */
	public void createControl(final Composite parent) {
		initializeDialogUnits(parent);
		readConfiguration();

		final Composite result = new Composite(parent, SWT.NONE);

		final GridLayout layout = new GridLayout(2, false);
		layout.horizontalSpacing = 10;
		result.setLayout(layout);

		final Control expressionComposite = createExpression(result);
		expressionComposite.setLayoutData(new GridData(GridData.FILL,
				GridData.CENTER, true, false, 2, 1));

		final Label separator = new Label(result, SWT.NONE);
		separator.setVisible(false);
		final GridData data = new GridData(GridData.FILL, GridData.FILL, false,
				false, 2, 1);
		data.heightHint = convertHeightInCharsToPixels(1) / 3;
		separator.setLayoutData(data);

		final Control searchFor = createSearchFor(result);
		searchFor.setLayoutData(new GridData(GridData.FILL, GridData.FILL,
				true, false, 1, 1));

		final Control limitTo = createLimitTo(result);
		limitTo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true,
				false, 1, 1));

		// final Control includeMask = createIncludeMask(result);
		// includeMask.setLayoutData(new GridData(GridData.FILL, GridData.FILL,
		// true, false, 2, 1));

		// createParticipants(result);

		final SelectionAdapter javaElementInitializer = new SelectionAdapter() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void widgetSelected(SelectionEvent event) {
				if (getSearchFor() == fInitialData.getSearchFor()) {
					fRef = fInitialData.getRef();
				} else {
					fRef = null;
				}
				setLimitTo(getSearchFor(), getLimitTo());
				// int limitToVal = setLimitTo(getSearchFor(), getLimitTo());
				// setIncludeMask(getIncludeMask(), limitToVal);
				doPatternModified();
			}
		};

		for (int i = 0; i < fSearchFor.length; i++) {
			fSearchFor[i].addSelectionListener(javaElementInitializer);
		}

		setControl(result);

		Dialog.applyDialogFont(result);
		// FIXME PlatformUI.getWorkbench().getHelpSystem().setHelp(result,
		// IJavaHelpContextIds.JAVA_SEARCH_PAGE);
	}

	/*
	 * private Control createParticipants(Composite result) { if
	 * (!SearchParticipantsExtensionPoint.hasAnyParticipants()) return new
	 * Composite(result, SWT.NULL); Button selectParticipants= new
	 * Button(result, SWT.PUSH);
	 * selectParticipants.setText(SearchMessages.getString
	 * ("SearchPage.select_participants.label")); //$NON-NLS-1$ GridData gd= new
	 * GridData(); gd.verticalAlignment= GridData.VERTICAL_ALIGN_BEGINNING;
	 * gd.horizontalAlignment= GridData.HORIZONTAL_ALIGN_END;
	 * gd.grabExcessHorizontalSpace= false; gd.horizontalAlignment=
	 * GridData.END; gd.horizontalSpan= 2; selectParticipants.setLayoutData(gd);
	 * selectParticipants.addSelectionListener(new SelectionAdapter() { public
	 * void widgetSelected(SelectionEvent e) {
	 * PreferencePageSupport.showPreferencePage(getShell(),
	 * "org.eclipse.jdt.ui.preferences.SearchParticipantsExtensionPoint", new
	 * SearchParticipantsExtensionPoint()); //$NON-NLS-1$ }
	 * 
	 * }); return selectParticipants; }
	 */

	private Control createExpression(final Composite parent) {
		final Composite result = new Composite(parent, SWT.NONE);
		final GridLayout layout = new GridLayout(2, false);
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		result.setLayout(layout);

		// Pattern text + info
		final Label label = new Label(result, SWT.LEFT);
		label.setText("Expression"); // SearchMessages.SearchPage_expression_label
		// );
		label.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false,
				false, 2, 1));

		// Pattern combo
		fPattern = new Combo(result, SWT.SINGLE | SWT.BORDER);
		fPattern.addSelectionListener(new SelectionAdapter() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void widgetSelected(final SelectionEvent e) {
				handlePatternSelected();
				updateOKStatus();
			}
		});
		fPattern.addModifyListener(new ModifyListener() {
			@SuppressWarnings("synthetic-access")
			public void modifyText(final ModifyEvent e) {
				doPatternModified();
				updateOKStatus();

			}
		});
		// TextFieldNavigationHandler.install(fPattern);
		final GridData data = new GridData(GridData.FILL, GridData.FILL, true,
				false, 1, 1);
		data.widthHint = convertWidthInCharsToPixels(50);
		fPattern.setLayoutData(data);

		// Ignore case checkbox
		fCaseSensitive = new Button(result, SWT.CHECK);
		fCaseSensitive.setText("Case sensitive");// SearchMessages.
		// SearchPage_expression_caseSensitive
		// );
		fCaseSensitive.addSelectionListener(new SelectionAdapter() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void widgetSelected(final SelectionEvent e) {
				fIsCaseSensitive = fCaseSensitive.getSelection();
			}
		});
		fCaseSensitive.setLayoutData(new GridData(GridData.FILL, GridData.FILL,
				false, false, 1, 1));

		return result;
	}

	final void updateOKStatus() {
		final boolean isValid = isValidSearchPattern();
		getContainer().setPerformActionEnabled(isValid);
	}

	private boolean isValidSearchPattern() {
		if (getPattern().length() == 0) {
			return false;
		}
		if (fRef != null) {
			return true;
		}
		return ErlSearchPattern.createPattern(getPattern(), getSearchFor(),
				getLimitTo(), SearchPattern.RULE_EXACT_MATCH) != null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.DialogPage#dispose()
	 */
	@Override
	public void dispose() {
		writeConfiguration();
		super.dispose();
	}

	private void doPatternModified() {
		if (fInitialData != null
				&& getPattern().equals(fInitialData.getPattern())
				&& fInitialData.getRef() != null
				&& fInitialData.getSearchFor() == getSearchFor()) {
			fCaseSensitive.setEnabled(false);
			fCaseSensitive.setSelection(true);
			fRef = fInitialData.getRef();
		} else {
			fCaseSensitive.setEnabled(true);
			fCaseSensitive.setSelection(fIsCaseSensitive);
			fRef = null;
		}
	}

	private void handlePatternSelected() {
		final int selectionIndex = fPattern.getSelectionIndex();
		if (selectionIndex < 0
				|| selectionIndex >= fPreviousSearchPatterns.size()) {
			return;
		}

		final SearchPatternData initialData = fPreviousSearchPatterns
				.get(selectionIndex);

		// setSearchFor(initialData.getSearchFor());
		// final int limitToVal = setLimitTo(initialData.getSearchFor(),
		// initialData.getLimitTo());
		// setIncludeMask(initialData.getIncludeMask(), limitToVal);

		fPattern.setText(initialData.getPattern());
		fIsCaseSensitive = initialData.isCaseSensitive();
		fRef = initialData.getRef();
		fCaseSensitive.setEnabled(fRef == null);
		fCaseSensitive.setSelection(initialData.isCaseSensitive());

		if (initialData.getWorkingSets() != null) {
			getContainer().setSelectedWorkingSets(initialData.getWorkingSets());
		} else {
			getContainer().setSelectedScope(initialData.getScope());
		}

		fInitialData = initialData;
	}

	private Control createSearchFor(final Composite parent) {
		final Group result = new Group(parent, SWT.NONE);
		result.setText("Search"); // SearchMessages.SearchPage_searchFor_label);
		result.setLayout(new GridLayout(2, true));

		fSearchFor = new Button[] {
				// createButton(result, SWT.RADIO,
				// SearchMessages.SearchPage_searchFor_type, TYPE, true),
				createButton(result, SWT.RADIO, "Function", FUNCTION, false),
				createButton(result, SWT.RADIO, "Module", MODULE, false),
				createButton(result, SWT.RADIO, "Record", RECORD, false),
				createButton(result, SWT.RADIO, "Macro", MACRO, false) };

		// Fill with dummy radio buttons
		final Label filler = new Label(result, SWT.NONE);
		filler.setVisible(false);
		filler.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false, 1,
				1));

		return result;
	}

	private Control createLimitTo(final Composite parent) {
		final Group result = new Group(parent, SWT.NONE);
		result.setText("Limit to");
		result.setLayout(new GridLayout(2, true));

		fLimitTo = new Button[] {
				createButton(result, SWT.RADIO, "Declarations", DECLARATIONS,
						false),
				createButton(result, SWT.RADIO, "References", REFERENCES, true),
				createButton(result, SWT.RADIO, "All occurrences",
						ALL_OCCURRENCES, false) };

		// final SelectionAdapter listener = new SelectionAdapter() {
		// @Override
		// public void widgetSelected(SelectionEvent e) {
		// updateUseJRE();
		// }
		// };
		// for (int i = 0; i < fLimitTo.length; i++) {
		// fLimitTo[i].addSelectionListener(listener);
		// }
		return result;
	}

	// private Control createIncludeMask(final Composite parent) {
	// final Group result = new Group(parent, SWT.NONE);
	// result.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2,
	// 1));
	// result.setText("Search in");
	// result.setLayout(new GridLayout(4, false));
	// fIncludeMasks = new Button[] {
	// createButton(result, SWT.CHECK,
	// SearchMessages.SearchPage_searchIn_sources,
	// JavaSearchScopeFactory.SOURCES, true),
	// createButton(result, SWT.CHECK,
	// SearchMessages.SearchPage_searchIn_projects,
	// JavaSearchScopeFactory.PROJECTS, true),
	// createButton(result, SWT.CHECK,
	// SearchMessages.SearchPage_searchIn_jre,
	// JavaSearchScopeFactory.JRE, false),
	// createButton(result, SWT.CHECK,
	// SearchMessages.SearchPage_searchIn_libraries,
	// JavaSearchScopeFactory.LIBS, true), };
	// return result;
	// }

	private Button createButton(final Composite parent, final int style,
			final String text, final int data, final boolean isSelected) {
		final Button button = new Button(parent, style);
		button.setText(text);
		button.setData(new Integer(data));
		button.setLayoutData(new GridData());
		button.setSelection(isSelected);
		return button;
	}

	private void initSelections() {
		final ISelection sel = getContainer().getSelection();
		SearchPatternData initData = null;

		if (sel instanceof IStructuredSelection) {
			initData = tryStructuredSelection((IStructuredSelection) sel);
		} else if (sel instanceof ITextSelection) {
			final IEditorPart activePart = getActiveEditor();
			if (activePart instanceof ErlangEditor) {
				final ErlangEditor erlangEditor = (ErlangEditor) activePart;
				// TODO how in the world can we find the proper build backend?
				final BuildBackend b = BackendManager.getDefault()
						.getIdeBackend().asBuild();
				final ISelection ssel = erlangEditor.getSite()
						.getSelectionProvider().getSelection();
				final ITextSelection textSel = (ITextSelection) ssel;
				final int offset = textSel.getOffset();
				OpenResult res;
				try {
					res = ErlideOpen.open(b, ErlScanner
							.createScannerModuleName(erlangEditor.getModule()),
							offset, "", new ArrayList<Tuple>(0));
				} catch (final RpcException e) {
					res = null;
				} catch (final BackendException e) {
					res = null;
				}
				ErlLogger.debug("searchPage(open) " + res);

				// final String title =
				// "SearchMessages.SearchElementSelectionDialog_title";
				// final String message =
				// "SearchMessages.SearchElementSelectionDialog_message";
				initData = determineInitValuesFrom(res);
			}
			if (initData == null) {
				initData = trySimpleTextSelection((ITextSelection) sel);
			}
		}
		if (initData == null) {
			initData = getDefaultInitValues();
		}

		fInitialData = initData;
		fRef = initData.getRef();
		fCaseSensitive.setSelection(initData.isCaseSensitive());
		fCaseSensitive.setEnabled(fRef == null);

		setSearchFor(initData.getSearchFor());
		// final int limitToVal =
		setLimitTo(initData.getSearchFor(), initData.getLimitTo());
		// setIncludeMask(initData.getIncludeMask(), limitToVal);

		fPattern.setText(initData.getPattern());
	}

	// private void updateUseJRE() {
	// setIncludeMask(getIncludeMask(), getLimitTo());
	// }

	// private static boolean forceIncludeAll(final int limitTo, IJavaElement
	// elem) {
	// return elem != null
	// && (limitTo == DECLARATIONS || limitTo == IMPLEMENTORS);
	// }

	private SearchPatternData tryStructuredSelection(
			final IStructuredSelection selection) {
		if (selection == null || selection.size() > 1) {
			return null;
		}

		final Object o = selection.getFirstElement();
		SearchPatternData res = null;
		if (o instanceof IErlElement) {
			res = determineInitValuesFrom((IErlElement) o);
		}
		// else if (o instanceof LogicalPackage) {
		// LogicalPackage lp = (LogicalPackage) o;
		// return new SearchPatternData(PACKAGE, REFERENCES, fIsCaseSensitive,
		// lp.getElementName(), null, getLastIncludeMask());
		// } else if (o instanceof IAdaptable) {
		// IJavaElement element = (IJavaElement) ((IAdaptable) o)
		// .getAdapter(IJavaElement.class);
		// if (element != null) {
		// res = determineInitValuesFrom(element);
		// }
		// }
		// if (res == null && o instanceof IAdaptable) {
		// final IWorkbenchAdapter adapter = (IWorkbenchAdapter) ((IAdaptable)
		// o)
		// .getAdapter(IWorkbenchAdapter.class);
		// if (adapter != null) {
		// return new SearchPatternData(TYPE, REFERENCES,
		// fIsCaseSensitive, adapter.getLabel(o), null,
		// getLastIncludeMask());
		// }
		// }
		return res;
	}

	private SearchPatternData determineInitValuesFrom(final IErlElement o) {
		final ErlangExternalFunctionCallRef ref = SearchUtil
				.getRefFromErlElement(o);
		if (ref == null) {
			return null;
		}
		return new SearchPatternData(FUNCTION, REFERENCES, true, "", ref);
	}

	// final static boolean isSearchableType(IJavaElement element) {
	// switch (element.getElementType()) {
	// case IJavaElement.PACKAGE_FRAGMENT:
	// case IJavaElement.PACKAGE_DECLARATION:
	// case IJavaElement.IMPORT_DECLARATION:
	// case IJavaElement.TYPE:
	// case IJavaElement.FIELD:
	// case IJavaElement.METHOD:
	// return true;
	// }
	// return false;
	// }

	private SearchPatternData determineInitValuesFrom(final OpenResult res) {
		// try {
		// JavaSearchScopeFactory factory=
		// JavaSearchScopeFactory.getInstance();
		// boolean isInsideJRE= factory.isInsideJRE(element);
		if (res == null) {
			return null;
		}
		// final int includeMask = getLastIncludeMask();

		if (res.isExternalCall()) {
			return new SearchPatternData(FUNCTION, REFERENCES, true, "",
					SearchUtil.getRefFromOpenRes(res));
		}
		// switch (element.getElementType()) {
		// case IJavaElement.PACKAGE_FRAGMENT:
		// case IJavaElement.PACKAGE_DECLARATION:
		// return new SearchPatternData(PACKAGE, REFERENCES, true, element
		// .getElementName(), element, includeMask);
		// case IJavaElement.IMPORT_DECLARATION: {
		// IImportDeclaration declaration = (IImportDeclaration) element;
		// if (declaration.isOnDemand()) {
		// final String name = Signature.getQualifier(declaration
		// .getElementName());
		// return new SearchPatternData(PACKAGE, DECLARATIONS, true,
		// name, element, JavaSearchScopeFactory.ALL);
		// }
		// return new SearchPatternData(TYPE, DECLARATIONS, true, element
		// .getElementName(), element, JavaSearchScopeFactory.ALL);
		// }
		// case IJavaElement.TYPE:
		// return new SearchPatternData(TYPE, REFERENCES, true,
		// PatternStrings.getTypeSignature((IType) element),
		// element, includeMask);
		// case IJavaElement.COMPILATION_UNIT: {
		// IType mainType = ((ICompilationUnit) element).findPrimaryType();
		// if (mainType != null) {
		// return new SearchPatternData(TYPE, REFERENCES, true,
		// PatternStrings.getTypeSignature(mainType),
		// mainType, includeMask);
		// }
		// break;
		// }
		// case IJavaElement.CLASS_FILE: {
		// IType mainType = ((IClassFile) element).getType();
		// if (mainType.exists()) {
		// return new SearchPatternData(TYPE, REFERENCES, true,
		// PatternStrings.getTypeSignature(mainType),
		// mainType, includeMask);
		// }
		// break;
		// }
		// case IJavaElement.FIELD:
		// return new SearchPatternData(FIELD, REFERENCES, true,
		// PatternStrings.getFieldSignature((IField) element),
		// element, includeMask);
		// case IJavaElement.METHOD:
		// IMethod method = (IMethod) element;
		// final int searchFor = method.isConstructor() ? CONSTRUCTOR
		// : METHOD;
		// return new SearchPatternData(searchFor, REFERENCES, true,
		// PatternStrings.getMethodSignature(method), element,
		// includeMask);
		// }
		//
		// } catch (JavaModelException e) {
		// if (!e.isDoesNotExist()) {
		// ExceptionHandler.handle(e,
		// SearchMessages.Search_Error_javaElementAccess_title,
		// SearchMessages.Search_Error_javaElementAccess_message);
		// }
		// // element might not exist
		// }
		return null;
	}

	private SearchPatternData trySimpleTextSelection(
			final ITextSelection selection) {
		final String selectedText = selection.getText();
		if (selectedText != null && selectedText.length() > 0) {
			int i = 0;
			while (i < selectedText.length()
					&& !SearchUtil.isLineDelimiterChar(selectedText.charAt(i))) {
				i++;
			}
			if (i > 0) {
				return new SearchPatternData(FUNCTION, REFERENCES,
						fIsCaseSensitive, selectedText.substring(0, i), null);
			}
		}
		return null;
	}

	private SearchPatternData getDefaultInitValues() {
		if (!fPreviousSearchPatterns.isEmpty()) {
			return fPreviousSearchPatterns.get(0);
		}

		return new SearchPatternData(FUNCTION, REFERENCES, fIsCaseSensitive,
				"", null); //$NON-NLS-1$
	}

	// private int getLastIncludeMask() {
	// try {
	// return getDialogSettings().getInt(STORE_INCLUDE_MASK);
	// } catch (final NumberFormatException e) {
	// return JavaSearchScopeFactory.NO_JRE;
	// }
	// }

	/*
	 * Implements method from ISearchPage
	 */
	public void setContainer(final ISearchPageContainer container) {
		fContainer = container;
	}

	/**
	 * Returns the search page's container.
	 * 
	 * @return the search page container
	 */
	private ISearchPageContainer getContainer() {
		return fContainer;
	}

	private IEditorPart getActiveEditor() {
		final IWorkbenchPage activePage = ErlideUIPlugin.getActivePage();
		if (activePage != null) {
			return activePage.getActiveEditor();
		}
		return null;
	}

	// --------------- Configuration handling --------------

	/**
	 * Returns the page settings for this Java search page.
	 * 
	 * @return the page settings to be used
	 */
	private IDialogSettings getDialogSettings() {
		if (fDialogSettings == null) {
			fDialogSettings = ErlideUIPlugin.getDefault()
					.getDialogSettingsSection(PAGE_NAME);
		}
		return fDialogSettings;
	}

	/**
	 * Initializes itself from the stored page settings.
	 */
	private void readConfiguration() {
		final IDialogSettings s = getDialogSettings();
		fIsCaseSensitive = s.getBoolean(STORE_CASE_SENSITIVE);

		try {
			final int historySize = s.getInt(STORE_HISTORY_SIZE);
			for (int i = 0; i < historySize; i++) {
				final IDialogSettings histSettings = s.getSection(STORE_HISTORY
						+ i);
				if (histSettings != null) {
					final SearchPatternData data = SearchPatternData
							.create(histSettings);
					if (data != null) {
						fPreviousSearchPatterns.add(data);
					}
				}
			}
		} catch (final NumberFormatException e) {
			// ignore
		}
	}

	/**
	 * Stores the current configuration in the dialog store.
	 */
	private void writeConfiguration() {
		final IDialogSettings s = getDialogSettings();
		s.put(STORE_CASE_SENSITIVE, fIsCaseSensitive);
		// s.put(STORE_INCLUDE_MASK, getIncludeMask());

		final int historySize = Math.min(fPreviousSearchPatterns.size(),
				HISTORY_SIZE);
		s.put(STORE_HISTORY_SIZE, historySize);
		for (int i = 0; i < historySize; i++) {
			final IDialogSettings histSettings = s.addNewSection(STORE_HISTORY
					+ i);
			final SearchPatternData data = fPreviousSearchPatterns.get(i);
			data.store(histSettings);
		}
	}
}
