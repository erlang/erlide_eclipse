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

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.util.StatusInfo;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

/**
 * The editor preferences
 * 
 * 
 * @author Vlad Dumitrescu
 */
public class IndentationPreferencePage extends ErlidePreferencePage implements
        IWorkbenchPreferencePage {

    private static final String INDENT_FIELDS[] = new String[] {
            ErlEditorMessages.Prefs_Before_binary_op,
            ErlEditorMessages.Prefs_After_binary_op,
            ErlEditorMessages.Prefs_Before_arrow,
            ErlEditorMessages.Prefs_After_arrow,
            ErlEditorMessages.Prefs_After_unary_op,
            ErlEditorMessages.Prefs_Clause, ErlEditorMessages.Prefs_Case,
            ErlEditorMessages.Prefs_Try, ErlEditorMessages.Prefs_Catch,
            ErlEditorMessages.Prefs_Function_parameters,
            ErlEditorMessages.Prefs_Fun, ErlEditorMessages.Prefs_Fun_body,
            ErlEditorMessages.Prefs_Paren,
            ErlEditorMessages.Prefs_Binary_begin,
            ErlEditorMessages.Prefs_End_paren };

    private static final String INDENT_KEYS[] = new String[] {
            "before_binary_op", "after_binary_op", "before_arrow", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            "after_arrow", "after_unary_op", "clause", "case", "try", "catch", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
            "function_parameters", "fun", "fun_body", "paren", "<<", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
            "end_paren" }; //$NON-NLS-1$

    private static final String INDENT_DEFAULTS[] = new String[] { "4", "4", //$NON-NLS-1$ //$NON-NLS-2$
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

    /**
     * Tells whether the fields are initialized.
     */
    private boolean fieldsInitialized = false;

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
            text.addModifyListener(fNumberFieldListener);
        }
    }

    static final String INDENT_KEY = "indentation"; //$NON-NLS-1$

    private void setToPreferences() {
        final List<String> l = getPreferences(INDENT_KEY, INDENT_KEYS,
                INDENT_DEFAULTS);
        for (int i = 0; i < l.size(); ++i) {
            final String s = l.get(i);
            textFields.get(i).setText(s);
        }
        fieldsInitialized = true;
    }

    @Override
    protected void putPreferences() {
        final Preferences node = ErlideUIPlugin.getPrefsNode();
        for (int i = 0; i < INDENT_KEYS.length; ++i) {
            int n;
            n = Integer.parseInt(textFields.get(i).getText());
            node.putInt(INDENT_KEY + "/" + INDENT_KEYS[i], n); //$NON-NLS-1$
        }
        try {
            node.flush();
        } catch (final BackingStoreException e) {
            ErlLogger.warn(e);
        }
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

    private final ModifyListener fNumberFieldListener = new ModifyListener() {
        @Override
        public void modifyText(final ModifyEvent e) {
            numberFieldChanged((Text) e.widget);
        }
    };

    void numberFieldChanged(final Text textControl) {
        final String number = textControl.getText();
        final IStatus status = validatePositiveNumber(number);
        updateStatus(status);
    }

    private IStatus validatePositiveNumber(final String number) {
        final StatusInfo status = new StatusInfo();
        if (number.length() == 0) {
            status.setError(ErlEditorMessages.ErlEditorPreferencePage_empty_input);
        } else {
            try {
                final int value = Integer.parseInt(number);
                if (value < 0) {
                    status.setError(MessageFormat
                            .format(ErlEditorMessages.ErlEditorPreferencePage_invalid_input,
                                    (Object[]) new String[] { number }));
                }
            } catch (final NumberFormatException e) {
                status.setError(MessageFormat
                        .format(ErlEditorMessages.ErlEditorPreferencePage_invalid_input,
                                (Object[]) new String[] { number }));
            }
        }
        return status;
    }

    private void updateStatus(final IStatus status) {
        if (!fieldsInitialized) {
            return;
        }
        setValid(!status.matches(IStatus.ERROR));
        applyToStatusLine(this, status);
    }

    /**
     * Applies the status to the status line of a dialog page.
     * 
     * @param page
     *            the dialog page
     * @param status
     *            the status
     */
    public void applyToStatusLine(final DialogPage page, final IStatus status) {
        String message = status.getMessage();
        switch (status.getSeverity()) {
        case IStatus.OK:
            page.setMessage(message, IMessageProvider.NONE);
            page.setErrorMessage(null);
            break;
        case IStatus.WARNING:
            page.setMessage(message, IMessageProvider.WARNING);
            page.setErrorMessage(null);
            break;
        case IStatus.INFO:
            page.setMessage(message, IMessageProvider.INFORMATION);
            page.setErrorMessage(null);
            break;
        default:
            if (message.length() == 0) {
                message = null;
            }
            page.setMessage(null);
            page.setErrorMessage(message);
            break;
        }
    }

    public static void addKeysAndPrefs(final Map<String, String> map) {
        Assert.isTrue(INDENT_KEYS.length == INDENT_FIELDS.length);
        Assert.isTrue(INDENT_FIELDS.length == INDENT_DEFAULTS.length);
        addKeysAndPrefs(INDENT_KEY, INDENT_KEYS, INDENT_DEFAULTS, map);
    }
}
