/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs.plugin;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * The editor preferences
 *
 *
 * @author Vlad Dumitrescu
 */
public class IndentationPreferencePage extends ErlidePreferencePage implements
        IWorkbenchPreferencePage {

    private static final String[] INDENT_FIELDS = new String[] {
            ErlEditorMessages.Prefs_Before_binary_op,
            ErlEditorMessages.Prefs_After_binary_op,
            ErlEditorMessages.Prefs_Before_arrow, ErlEditorMessages.Prefs_After_arrow,
            ErlEditorMessages.Prefs_After_unary_op, ErlEditorMessages.Prefs_Clause,
            ErlEditorMessages.Prefs_Case, ErlEditorMessages.Prefs_Try,
            ErlEditorMessages.Prefs_Catch, ErlEditorMessages.Prefs_Function_parameters,
            ErlEditorMessages.Prefs_Fun, ErlEditorMessages.Prefs_Fun_body,
            ErlEditorMessages.Prefs_Paren, ErlEditorMessages.Prefs_Binary_begin,
            ErlEditorMessages.Prefs_End_paren };

    private static final String[] INDENT_KEYS = new String[] {
            "before_binary_op", "after_binary_op", "before_arrow", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            "after_arrow", "after_unary_op", "clause", "case", "try", "catch", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
            "function_parameters", "fun", "fun_body", "paren", "<<", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
            "end_paren" }; //$NON-NLS-1$

    private static final String[] INDENT_DEFAULTS = new String[] { "4", "4", //$NON-NLS-1$ //$NON-NLS-2$
            "2", "4", "4", "4", "4", "4", "4", "2", "3", "5", "1", "2", "0" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$ //$NON-NLS-10$ //$NON-NLS-11$ //$NON-NLS-12$ //$NON-NLS-13$
    };

    // private static final int N_NUMERIC_KEYS = INDENT_KEYS.length - 4;

    public IndentationPreferencePage() {
        super();
        setDescription(ErlEditorMessages.IndentationPrefs_36);
    }

    /**
     * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
    public void init(final IWorkbench workbench) {
    }

    private final List<Text> textFields = new ArrayList<Text>();

    /*
     * @see PreferencePage#createContents(Composite)
     */
    @Override
    protected Control createContents(final Composite parent) {
        final Composite c = new Composite(parent, SWT.NONE);
        final GridLayout layout = new GridLayout();
        layout.numColumns = 4;
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        c.setLayout(layout);
        createMyControls(c);
        setToPreferences();
        return c;
    }

    private void createMyControls(final Composite parent) {
        for (int i = 0; i < INDENT_DEFAULTS.length; ++i) {
            final String desc = INDENT_FIELDS[i];
            final Composite c = parent;
            final Label label = new Label(c, SWT.NONE);
            label.setText(desc);
            GridData gd = new GridData(SWT.END, SWT.CENTER, true, false); // new
            // GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
            gd.horizontalIndent = 3;
            label.setLayoutData(gd);
            final Text text = new Text(c, SWT.BORDER | SWT.SINGLE);
            gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
            gd.widthHint = convertWidthInCharsToPixels(3);
            text.setLayoutData(gd);
            textFields.add(text);
            text.addModifyListener(getNumberFieldListener());
        }
    }

    static final String INDENT_KEY = "indentation"; //$NON-NLS-1$

    private void setToPreferences() {
        final List<String> l = getPreferences(INDENT_KEYS, INDENT_DEFAULTS);
        for (int i = 0; i < l.size(); ++i) {
            final String s = l.get(i);
            textFields.get(i).setText(s);
        }
    }

    @Override
    protected void putPreferences() {
        putIntPreferences(INDENT_KEYS, textFields);
    }

    /*
     * @see PreferencePage#performDefaults()
     */
    @Override
    protected void performDefaults() {
        for (int i = 0; i < INDENT_KEYS.length; ++i) {
            final String s = INDENT_DEFAULTS[i];
            textFields.get(i).setText(s);
        }
        super.performDefaults();
    }

    public static void addKeysAndPrefs(final Map<String, String> map) {
        assertThat(INDENT_KEYS.length, is(INDENT_FIELDS.length));
        assertThat(INDENT_FIELDS.length, is(INDENT_DEFAULTS.length));
        addKeysAndPrefs(INDENT_KEY, INDENT_KEYS, INDENT_DEFAULTS, map);
    }

    @Override
    protected String getDialogPreferenceKey() {
        return INDENT_KEY;
    }
}
