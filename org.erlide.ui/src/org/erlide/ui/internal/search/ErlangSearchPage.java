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
package org.erlide.ui.internal.search;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.search.ui.ISearchPage;
import org.eclipse.search.ui.ISearchPageContainer;
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
import org.eclipse.ui.IWorkingSetManager;
import org.eclipse.ui.PlatformUI;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.text.ErlangToolkit;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;
import org.erlide.ui.util.ErlModelUtils;

import erlang.ErlangSearchPattern;
import erlang.ErlideOpen;
import erlang.OpenResult;

public class ErlangSearchPage extends DialogPage implements ISearchPage {

	private static class SearchPatternData {
		private final String pattern;
		private final int scope;
		private final IWorkingSet[] workingSets;
		private final int limitTo;
		private final int searchFor;

		public SearchPatternData(final String pattern, final int scope,
				final int limitTo, final int searchFor,
				final IWorkingSet[] workingSets) {
			this.pattern = pattern;
			this.scope = scope;
			this.limitTo = limitTo;
			this.workingSets = workingSets;
			this.searchFor = searchFor;
		}

		public int getScope() {
			return scope;
		}

		public int getLimitTo() {
			return limitTo;
		}

		public IWorkingSet[] getWorkingSets() {
			return workingSets;
		}

		public void store(final IDialogSettings settings) {
			settings.put("pattern", pattern); //$NON-NLS-1$
			settings.put("scope", scope); //$NON-NLS-1$
			settings.put("limitTo", limitTo); //$NON-NLS-1$
			settings.put("searchFor", searchFor); //$NON-NLS-1$
			if (workingSets != null) {
				final String[] wsIds = new String[workingSets.length];
				for (int i = 0; i < workingSets.length; i++) {
					wsIds[i] = workingSets[i].getName();
				}
				settings.put("workingSets", wsIds);
			} else {
				settings.put("workingSets", new String[0]);
			}
		}

		public static SearchPatternData create(final IDialogSettings settings) {
			final String pattern = settings.get("pattern");
			if (pattern.length() == 0) {
				return null;
			}
			final String[] wsIds = settings.getArray("workingSets");

			IWorkingSet[] workingSets = null;
			if (wsIds != null && wsIds.length > 0) {
				final IWorkingSetManager workingSetManager = PlatformUI
						.getWorkbench().getWorkingSetManager();
				workingSets = new IWorkingSet[wsIds.length];
				for (int i = 0; workingSets != null && i < wsIds.length; i++) {
					workingSets[i] = workingSetManager.getWorkingSet(wsIds[i]);
					if (workingSets[i] == null) {
						workingSets = null;
					}
				}
			}

			try {
				final int scope = settings.getInt("scope");
				final int limitTo = settings.getInt("limitTo");
				final int searchFor = settings.getInt("searchFor");
				return new SearchPatternData(pattern, scope, limitTo,
						searchFor, workingSets);

			} catch (final NumberFormatException e) {
			}
			return null;
		}

		public String getPattern() {
			return pattern;
		}

		public int getSearchFor() {
			return searchFor;
		}
	}

	// limit to
	private final static int DEFINITIONS = ErlangSearchPattern.DEFINITIONS;
	private final static int REFERENCES = ErlangSearchPattern.REFERENCES;
	private final static int ALL_OCCURRENCES = ErlangSearchPattern.ALL_OCCURRENCES;

	// search for
	private final static int SEARCHFOR_FUNCTION = ErlangSearchPattern.SEARCHFOR_FUNCTION;
	private final static int SEARCHFOR_RECORD = ErlangSearchPattern.SEARCHFOR_RECORD;
	private final static int SEARCHFOR_MACRO = ErlangSearchPattern.SEARCHFOR_MACRO;
	private final static int SEARCHFOR_TYPE = ErlangSearchPattern.SEARCHFOR_TYPE;
	private final static int SEARCHFOR_INCLUDE = ErlangSearchPattern.SEARCHFOR_INCLUDE;

	private static final int HISTORY_SIZE = 12;

	// Dialog store id constants
	private final static String PAGE_NAME = "ErlangSearchPage"; //$NON-NLS-1$
	// //$NON-NLS-1$
	private final static String STORE_HISTORY = "HISTORY"; //$NON-NLS-1$
	private final static String STORE_HISTORY_SIZE = "HISTORY_SIZE"; //$NON-NLS-1$

	private final List<SearchPatternData> fPreviousSearchPatterns;

	private SearchPatternData fInitialData;
	private boolean fFirstTime = true;
	private IDialogSettings fDialogSettings;

	private Combo fPattern;
	private ISearchPageContainer fContainer;

	private Button[] fSearchFor;
	private Button[] fLimitTo;

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
		Collection<IResource> scope = null;
		String scopeDescription = null;
		switch (getContainer().getSelectedScope()) {
		case ISearchPageContainer.WORKSPACE_SCOPE:
			scope = SearchUtil.getWorkspaceScope();
			scopeDescription = "workspace";
			break;
		case ISearchPageContainer.SELECTED_PROJECTS_SCOPE:
			final String[] projectNames = getContainer()
					.getSelectedProjectNames();
			scope = SearchUtil.getProjectsScope(projectNames);
			scopeDescription = "projects";
			break;
		case ISearchPageContainer.SELECTION_SCOPE:
			scope = SearchUtil.getSelectionScope(getContainer().getSelection());
			break;
		case ISearchPageContainer.WORKING_SET_SCOPE: {
			final IWorkingSet[] workingSets = getContainer()
					.getSelectedWorkingSets();
			// should not happen - just to be sure
			if (workingSets == null || workingSets.length < 1) {
				return false;
			}
			scopeDescription = SearchUtil
					.getWorkingSetsScopeDescription(workingSets);
			scope = SearchUtil.getWorkingSetsScope(workingSets);
			// SearchUtil.updateLRUWorkingSets(workingSets);
		}
		}
		final ErlangSearchPattern searchPattern = SearchUtil
				.getSearchPattern(null, data.getSearchFor(), data.getPattern(),
						data.getLimitTo());
		SearchUtil.runQuery(searchPattern, scope, scopeDescription, getShell());
		return true;
	}

	private String[] getPreviousSearchPatterns() {
		// Search results are not persistent
		final int patternCount = fPreviousSearchPatterns.size();
		final String[] patterns = new String[patternCount];
		for (int i = 0; i < patternCount; i++) {
			patterns[i] = fPreviousSearchPatterns.get(i).getPattern();
		}
		return patterns;
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
		match = new SearchPatternData(pattern, getContainer()
				.getSelectedScope(), getLimitTo(), getSearchFor(),
				getContainer().getSelectedWorkingSets());

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
	 * @see
	 * org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets
	 * .Composite)
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

		// final SelectionAdapter javaElementInitializer = new
		// SelectionAdapter() {
		// @SuppressWarnings("synthetic-access")
		// @Override
		// public void widgetSelected(final SelectionEvent event) {
		// if (getSearchFor() == fInitialData.getSearchFor()) {
		// fRef = fInitialData.getRef();
		// } else {
		// fRef = null;
		// }
		// setLimitTo(getSearchFor(), getLimitTo());
		// // int limitToVal = setLimitTo(getSearchFor(), getLimitTo());
		// // setIncludeMask(getIncludeMask(), limitToVal);
		// doPatternModified();
		// }
		// };
		//
		// for (int i = 0; i < fSearchFor.length; i++) {
		// fSearchFor[i].addSelectionListener(javaElementInitializer);
		// }

		setControl(result);

		Dialog.applyDialogFont(result);
		PlatformUI.getWorkbench().getHelpSystem()
				.setHelp(result, IErlangHelpContextIds.ERLANG_SEARCH_PAGE);
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

		return result;
	}

	final void updateOKStatus() {
		final boolean isValid = isValidSearchPattern();
		getContainer().setPerformActionEnabled(isValid);
	}

	private boolean isValidSearchPattern() {
		// FIXME borde kollas ordentligt! kanske via open
		if (getPattern().length() > 0) {
			return true;
		}
		return false;
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
				&& getPattern().equals(fInitialData.getPattern())) {
			;
			;
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

		setSearchFor(initialData.getSearchFor());
		// final int limitToVal = setLimitTo(initialData.getSearchFor(),
		// initialData.getLimitTo());
		// setIncludeMask(initialData.getIncludeMask(), limitToVal);

		fPattern.setText(initialData.getPattern());

		if (initialData.getWorkingSets() != null) {
			getContainer().setSelectedWorkingSets(initialData.getWorkingSets());
		} else {
			getContainer().setSelectedScope(initialData.getScope());
		}

		fInitialData = initialData;
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

	private Control createSearchFor(final Composite parent) {
		final Group result = new Group(parent, SWT.NONE);
		result.setText("Search For");
		result.setLayout(new GridLayout(2, true));

		fSearchFor = new Button[] {
				createRadioButton(result, "Function", SEARCHFOR_FUNCTION),
				createRadioButton(result, "Record", SEARCHFOR_RECORD),
				createRadioButton(result, "Macro", SEARCHFOR_MACRO),
				createRadioButton(result, "Type", SEARCHFOR_TYPE),
				createRadioButton(result, "Include", SEARCHFOR_INCLUDE) };

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
				createRadioButton(result, "Definitions", DEFINITIONS),
				createRadioButton(result, "References", REFERENCES, true),
				createRadioButton(result, "All occurrences", ALL_OCCURRENCES) };

		return result;
	}

	private Button createRadioButton(final Composite parent, final String text,
			final int data) {
		return createRadioButton(parent, text, data, false);
	}

	private Button createRadioButton(final Composite parent, final String text,
			final int data, final boolean isSelected) {
		final Button button = new Button(parent, SWT.RADIO);
		button.setText(text);
		button.setData(Integer.valueOf(data));
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
				final IErlModule module = erlangEditor.getModule();
				if (module != null) {
					// TODO how in the world can we find the proper build
					// backend?
					final Backend b = ErlangCore.getBackendManager()
							.getIdeBackend();
					final ISelection ssel = erlangEditor.getSite()
							.getSelectionProvider().getSelection();
					final ITextSelection textSel = (ITextSelection) ssel;
					final int offset = textSel.getOffset();
					OpenResult res;
					try {
						final String scannerModuleName = ErlangToolkit
								.createScannerModuleName(module);
						res = ErlideOpen.open(b, scannerModuleName, offset,
								ErlModelUtils.getImportsAsList(module), "",
								ErlangCore.getModel().getPathVars());
					} catch (final BackendException e) {
						res = null;
					}
					ErlLogger.debug("searchPage(open) " + res);
					initData = determineInitValuesFrom(module, offset, res);
				}
			}
			if (initData == null) {
				initData = trySimpleTextSelection((ITextSelection) sel);
			}
		}
		if (initData == null) {
			initData = getDefaultInitValues();
		}

		fInitialData = initData;

		fPattern.setText(initData.getPattern());
		setSearchFor(initData.getSearchFor());
	}

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
		return res;
	}

	private int getIntData(final Button button) {
		return ((Integer) button.getData()).intValue();
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

	private SearchPatternData determineInitValuesFrom(final IErlModule module,
			final int offset, final OpenResult res) {
		if (res == null) {
			return null;
		}
		final ErlangSearchPattern pattern = SearchUtil
				.getSearchPatternFromOpenResultAndLimitTo(module, offset, res,
						REFERENCES, true);
		final String patternString = (pattern == null) ? "" : pattern
				.patternString();
		final int searchFor = (pattern == null) ? SEARCHFOR_FUNCTION : pattern
				.getSearchFor();
		final SearchPatternData searchPatternData = new SearchPatternData(
				patternString, ISearchPageContainer.WORKSPACE_SCOPE,
				REFERENCES, searchFor, null);
		return searchPatternData;
	}

	private SearchPatternData determineInitValuesFrom(final IErlElement e) {
		final ErlangSearchPattern pattern = SearchUtil
				.getSearchPatternFromErlElementAndLimitTo(e, getLimitTo());
		if (pattern == null) {
			return null;
		}
		return new SearchPatternData(pattern.patternString(),
				ISearchPageContainer.WORKSPACE_SCOPE, REFERENCES,
				pattern.getSearchFor(), null);
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
				final String s = selectedText.substring(0, i);
				// FIXME find the module from the editor somehow
				return new SearchPatternData(s, getContainer()
						.getSelectedScope(), getLimitTo(), getSearchFor(),
						getContainer().getSelectedWorkingSets());
			}
		}
		return null;
	}

	private SearchPatternData getDefaultInitValues() {
		if (!fPreviousSearchPatterns.isEmpty()) {
			return fPreviousSearchPatterns.get(0);
		}

		return new SearchPatternData("", ISearchPageContainer.WORKSPACE_SCOPE,
				REFERENCES, SEARCHFOR_FUNCTION, null);
	}

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
