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

import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.dialogs.ErrorDialog;
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
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.util.ModelUtils;
import org.erlide.core.services.search.ErlSearchScope;
import org.erlide.core.services.search.ErlangSearchPattern;
import org.erlide.core.services.search.ErlangSearchPattern.LimitTo;
import org.erlide.core.services.search.ErlangSearchPattern.SearchFor;
import org.erlide.core.services.search.ErlideOpen;
import org.erlide.core.services.search.OpenResult;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;
import org.erlide.ui.internal.ErlideUIPlugin;

import com.google.common.collect.Lists;

public class ErlangSearchPage extends DialogPage implements ISearchPage {

    private static final ErlSearchScope EMPTY_SCOPE = new ErlSearchScope();

    private static class SearchPatternData {
        private final String pattern;
        private final int scope;
        private final IWorkingSet[] workingSets;
        private final LimitTo limitTo;
        private final SearchFor searchFor;
        private final int includeMask;

        public SearchPatternData(final String pattern, final int scope,
                final LimitTo limitTo, final SearchFor searchFor,
                final IWorkingSet[] workingSets, final int includeMask) {
            this.pattern = pattern;
            this.scope = scope;
            this.limitTo = limitTo;
            this.workingSets = workingSets;
            this.searchFor = searchFor;
            this.includeMask = includeMask;
        }

        public int getScope() {
            return scope;
        }

        public LimitTo getLimitTo() {
            return limitTo;
        }

        public IWorkingSet[] getWorkingSets() {
            return workingSets;
        }

        public void store(final IDialogSettings settings) {
            settings.put("pattern", pattern); //$NON-NLS-1$
            settings.put("scope", scope); //$NON-NLS-1$
            settings.put("limitTo", limitTo.toString()); //$NON-NLS-1$
            settings.put("searchFor", searchFor.toString()); //$NON-NLS-1$
            settings.put("includeMask", getIncludeMask()); //$NON-NLS-1$
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
                final int scope = settings.getInt("scope"); //$NON-NLS-1$
                LimitTo limitTo;
                try {
                    limitTo = LimitTo.valueOf(settings.get("limitTo")); // //$NON-NLS-1$
                } catch (final IllegalArgumentException e) {
                    limitTo = LimitTo.REFERENCES;
                }
                SearchFor searchFor;
                try {
                    searchFor = SearchFor.valueOf(settings.get("searchFor")); // //$NON-NLS-1$
                } catch (final IllegalArgumentException e) {
                    searchFor = SearchFor.FUNCTION;
                }
                final int includeMask = settings.getInt("includeMask"); //$NON-NLS-1$
                return new SearchPatternData(pattern, scope, limitTo,
                        searchFor, workingSets, includeMask);

            } catch (final NumberFormatException e) {
            }
            return null;
        }

        public String getPattern() {
            return pattern;
        }

        public SearchFor getSearchFor() {
            return searchFor;
        }

        public int getIncludeMask() {
            return includeMask;
        }
    }

    private static final int HISTORY_SIZE = 12;

    // Dialog store id constants
    private final static String PAGE_NAME = "ErlangSearchPage"; //$NON-NLS-1$
    private final static String STORE_INCLUDE_MASK = "INCLUDE_MASK"; //$NON-NLS-1$
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
    private Button[] fIncludeMasks;

    /**
	 *
	 */
    public ErlangSearchPage() {
        fPreviousSearchPatterns = Lists.newArrayList();
    }

    // ---- Action Handling ------------------------------------------------

    @Override
    public boolean performAction() {
        try {
            return performNewSearch();
        } catch (final CoreException e) {
            ErrorDialog.openError(getShell(), "Search",
                    "Problems occurred while searching. "
                            + "The affected files will be skipped.",
                    e.getStatus());
            return false;
        }
    }

    private boolean performNewSearch() throws CoreException {
        final SearchPatternData data = getPatternData();
        final int includeMask = getIncludeMask();
        // Setup search scope
        ErlSearchScope scope = EMPTY_SCOPE;
        String scopeDescription = null;
        final boolean searchSources = (includeMask & SearchUtil.SEARCH_IN_SOURCES) != 0;
        final boolean searchExternals = (includeMask & SearchUtil.SEARCH_IN_EXTERNALS) != 0;
        final boolean searchOtp = (includeMask & SearchUtil.SEARCH_IN_OTP_LIBRARIES) != 0;
        final int selectedScope = getContainer().getSelectedScope();
        switch (selectedScope) {
        case ISearchPageContainer.WORKSPACE_SCOPE:
            if (searchSources) {
                scope = SearchUtil
                        .getWorkspaceScope(searchExternals, searchOtp);
            }
            scopeDescription = SearchUtil.getWorkspaceScopeDescription();
            break;
        case ISearchPageContainer.SELECTED_PROJECTS_SCOPE:
            final String[] projectNames = getContainer()
                    .getSelectedProjectNames();
            if (searchSources) {
                scope = SearchUtil.getProjectsScope(
                        SearchUtil.getProjects(projectNames), searchExternals,
                        searchOtp);
            }
            scopeDescription = SearchUtil.getProjectScopeDescription(SearchUtil
                    .getProjects(projectNames));
            break;
        case ISearchPageContainer.SELECTION_SCOPE:
            if (searchSources) {
                scope = SearchUtil.getSelectionScope(getContainer()
                        .getSelection(), searchExternals, searchOtp);
            }
            scopeDescription = SearchUtil
                    .getSelectionScopeDescription(getContainer().getSelection());
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
            if (searchSources) {
                scope = SearchUtil.getWorkingSetsScope(workingSets,
                        searchExternals, searchOtp);
            }
            SearchUtil.updateLRUWorkingSets(workingSets);
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
                getContainer().getSelectedWorkingSets(), getIncludeMask());

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
                try {
                    initSelections();
                } catch (final ErlModelException e) {
                }
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

    @Override
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

        final Control includeMask = createIncludeMask(result);
        includeMask.setLayoutData(new GridData(GridData.FILL, GridData.FILL,
                true, false, 2, 1));

        setControl(result);

        Dialog.applyDialogFont(result);
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(result, IErlangHelpContextIds.ERLANG_SEARCH_PAGE);
    }

    private Control createIncludeMask(final Composite parent) {
        final Group result = new Group(parent, SWT.NONE);
        result.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2,
                1));
        result.setText("Search In");
        result.setLayout(new GridLayout(3, false));
        fIncludeMasks = new Button[] {
                createCheckButton(result, "Sourc&es",
                        SearchUtil.SEARCH_IN_SOURCES, true),
                createCheckButton(result, "E&xternals",
                        SearchUtil.SEARCH_IN_EXTERNALS, false),
                createCheckButton(result, "&OTP libraries",
                        SearchUtil.SEARCH_IN_OTP_LIBRARIES, false) };
        return result;
    }

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
            @Override
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

    @Override
    public void dispose() {
        writeConfiguration();
        super.dispose();
    }

    private void doPatternModified() {
        if (fInitialData != null
                && getPattern().equals(fInitialData.getPattern())) {
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
        setIncludeMask(initialData.getIncludeMask());
        setLimitTo(initialData.getLimitTo());
        // final int limitToVal = setLimitTo(initialData.getSearchFor(),
        // initialData.getLimitTo());

        fPattern.setText(initialData.getPattern());

        if (initialData.getWorkingSets() != null) {
            getContainer().setSelectedWorkingSets(initialData.getWorkingSets());
        } else {
            getContainer().setSelectedScope(initialData.getScope());
        }

        fInitialData = initialData;
    }

    private SearchFor getSearchFor() {
        for (int i = 0; i < fSearchFor.length; i++) {
            final Button button = fSearchFor[i];
            if (button.getSelection()) {
                return (SearchFor) button.getData();
            }
        }
        Assert.isTrue(false, "shouldNeverHappen"); //$NON-NLS-1$
        return SearchFor.FUNCTION;
    }

    private void setSearchFor(final SearchFor searchFor) {
        for (final Button button : fSearchFor) {
            button.setSelection(searchFor == button.getData());
        }
    }

    private void setLimitTo(final LimitTo limitTo) {
        for (final Button button : fLimitTo) {
            button.setSelection(limitTo == button.getData());
        }
    }

    private void setIncludeMask(final int includeMask) {
        for (int i = 0; i < fIncludeMasks.length; i++) {
            final Button button = fIncludeMasks[i];
            button.setSelection((includeMask & getIntData(button)) != 0);
        }
    }

    private Control createSearchFor(final Composite parent) {
        final Group result = new Group(parent, SWT.NONE);
        result.setText("Search For");
        result.setLayout(new GridLayout(2, true));

        fSearchFor = new Button[] {
                createRadioButton(result, "Function", SearchFor.FUNCTION),
                createRadioButton(result, "Record", SearchFor.RECORD),
                createRadioButton(result, "Macro", SearchFor.MACRO),
                createRadioButton(result, "Type", SearchFor.TYPE),
                createRadioButton(result, "Include", SearchFor.INCLUDE),
                createRadioButton(result, "Record field",
                        SearchFor.RECORD_FIELD) };

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
                createRadioButton(result, "Definitions", LimitTo.DEFINITIONS),
                createRadioButton(result, "References", LimitTo.REFERENCES,
                        true),
                createRadioButton(result, "All occurrences",
                        LimitTo.ALL_OCCURRENCES) };
        return result;
    }

    private Button createRadioButton(final Composite parent, final String text,
            final Object data) {
        return createRadioButton(parent, text, data, false);
    }

    private Button createRadioButton(final Composite parent, final String text,
            final Object data, final boolean isSelected) {
        final Button button = new Button(parent, SWT.RADIO);
        button.setText(text);
        button.setData(data);
        button.setLayoutData(new GridData());
        button.setSelection(isSelected);
        return button;
    }

    private Button createCheckButton(final Composite parent, final String text,
            final int data, final boolean isSelected) {
        final Button button = new Button(parent, SWT.CHECK);
        button.setText(text);
        button.setData(Integer.valueOf(data));
        button.setLayoutData(new GridData());
        button.setSelection(isSelected);
        return button;
    }

    private void initSelections() throws ErlModelException {
        final ISelection sel = getContainer().getSelection();
        SearchPatternData initData = null;
        if (sel instanceof IStructuredSelection) {
            initData = tryStructuredSelection((IStructuredSelection) sel);
        } else if (sel instanceof ITextSelection) {
            final IEditorPart activePart = getActiveEditor();
            if (activePart instanceof ErlangEditor) {
                initData = tryErlangTextSelection(initData, activePart);
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

    public SearchPatternData tryErlangTextSelection(SearchPatternData initData,
            final IEditorPart activePart) throws ErlModelException {
        final ErlangEditor erlangEditor = (ErlangEditor) activePart;
        final IErlModule module = erlangEditor.getModule();
        if (module != null) {
            final IBackend backend = BackendCore.getBackendManager()
                    .getIdeBackend();
            final ISelection ssel = erlangEditor.getSite()
                    .getSelectionProvider().getSelection();
            final ITextSelection textSel = (ITextSelection) ssel;
            final int offset = textSel.getOffset();
            OpenResult res;
            try {
                res = ErlideOpen.open(backend, module, offset,
                        ModelUtils.getImportsAsList(module), "",
                        ErlModelManager.getErlangModel().getPathVars());
            } catch (final RpcException e) {
                res = null;
            }
            ErlLogger.debug("searchPage(open) " + res);
            initData = determineInitValuesFrom(module, offset, res);
        }
        return initData;
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

    private LimitTo getLimitTo() {
        for (int i = 0; i < fLimitTo.length; i++) {
            final Button button = fLimitTo[i];
            if (button.getSelection()) {
                return (LimitTo) button.getData();
            }
        }
        Assert.isTrue(false, "shouldNeverHappen"); //$NON-NLS-1$
        return LimitTo.ALL_OCCURRENCES;
    }

    private int getIncludeMask() {
        int mask = 0;
        for (int i = 0; i < fIncludeMasks.length; i++) {
            final Button button = fIncludeMasks[i];
            if (button.getSelection()) {
                mask |= getIntData(button);
            }
        }
        return mask;
    }

    private int getLastIncludeMask() {
        try {
            return getDialogSettings().getInt(STORE_INCLUDE_MASK);
        } catch (final NumberFormatException e) {
        }
        return 0;
    }

    private SearchPatternData determineInitValuesFrom(final IErlModule module,
            final int offset, final OpenResult res) throws ErlModelException {
        if (res == null) {
            return null;
        }
        final ErlangSearchPattern pattern = SearchUtil
                .getSearchPatternFromOpenResultAndLimitTo(module, offset, res,
                        LimitTo.REFERENCES, true);
        final String patternString = pattern == null ? "" : pattern
                .patternString();
        final SearchFor searchFor = pattern == null ? SearchFor.FUNCTION
                : pattern.getSearchFor();
        final SearchPatternData searchPatternData = new SearchPatternData(
                patternString, ISearchPageContainer.WORKSPACE_SCOPE,
                LimitTo.REFERENCES, searchFor, null,
                SearchUtil.SEARCH_IN_SOURCES);
        return searchPatternData;
    }

    private SearchPatternData determineInitValuesFrom(final IErlElement e) {
        final ErlangSearchPattern pattern = ErlangSearchPattern
                .getSearchPatternFromErlElementAndLimitTo(e, getLimitTo());
        if (pattern == null) {
            return null;
        }
        return new SearchPatternData(pattern.patternString(),
                ISearchPageContainer.WORKSPACE_SCOPE, LimitTo.REFERENCES,
                pattern.getSearchFor(), null, getLastIncludeMask());
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
                        getContainer().getSelectedWorkingSets(),
                        getLastIncludeMask());
            }
        }
        return null;
    }

    private SearchPatternData getDefaultInitValues() {
        if (!fPreviousSearchPatterns.isEmpty()) {
            return fPreviousSearchPatterns.get(0);
        }

        return new SearchPatternData("", ISearchPageContainer.WORKSPACE_SCOPE,
                LimitTo.REFERENCES, SearchFor.FUNCTION, null,
                SearchUtil.SEARCH_IN_SOURCES);
    }

    /*
     * Implements method from ISearchPage
     */
    @Override
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
     * Returns the page settings for this Erlang search page.
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
