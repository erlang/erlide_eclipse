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

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.ui.ErlideUIConstants;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.internal.ErlideUIPlugin;

/**
 * The editor preferences
 *
 *
 * @author Jakob
 */
public class EditorPreferencePage extends ErlidePreferencePage implements
        IWorkbenchPreferencePage {

    /**
     * Initialize the system preferences
     *
     */
    public EditorPreferencePage() {
        setDescription(ErlEditorMessages.ErlEditorPreferencePage_description);
        setPreferenceStore(ErlideUIPlugin.getDefault().getPreferenceStore());

    }

    /**
     * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
    public void init(final IWorkbench workbench) {
    }

    static final String EDITOR_KEY = "erlangEditor"; //$NON-NLS-1$

    private static final int DEFAULT_INDENT_WIDTH = 4;
    private static final boolean DEFAULT_ENABLE_HOVER = true;

    private void setToPreferences() {
        final IEclipsePreferences node = ErlideUIPlugin.getPrefsNode();
        final Integer i = node.getInt(indentWidthKey, DEFAULT_INDENT_WIDTH);
        indentWidthText.setText(i.toString());
        enableHoverCheckBox.setSelection(node.getBoolean(enableHoverKey,
                DEFAULT_ENABLE_HOVER));
    }

    @Override
    protected void putPreferences() {
        final IEclipsePreferences node = ErlideUIPlugin.getPrefsNode();
        node.putInt(indentWidthKey, Integer.parseInt(indentWidthText.getText()));
        node.putBoolean(enableHoverKey, enableHoverCheckBox.getSelection());
    }

    public static boolean getEnableHover() {
        final IEclipsePreferences node = ErlideUIPlugin.getPrefsNode();
        return node.getBoolean(EDITOR_KEY + "/" + ErlideUIConstants.EDITOR_ENABLE_HOVER,
                DEFAULT_ENABLE_HOVER);
    }

    @Override
    protected String getDialogPreferenceKey() {
        return EDITOR_KEY;
    }

    private Text indentWidthText;
    private Button enableHoverCheckBox;

    private String indentWidthKey;
    private String enableHoverKey;

    /*
     * @see PreferencePage#createControl(Composite)
     */
    @Override
    public void createControl(final Composite parent) {
        super.createControl(parent);
        // WorkbenchHelp.setHelp(getControl(),
        // ITextEditorHelpContextIds.TEXT_EDITOR_PREFERENCE_PAGE);
    }

    private Control createAppearancePage(final Composite parent) {

        final Composite appearanceComposite = new Composite(parent, SWT.NONE);
        final GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        appearanceComposite.setLayout(layout);

        String label = ErlEditorMessages.ErlEditorPreferencePage_indentationWidth;

        final Pair<Text, String> addTextField = addTextField(appearanceComposite, label,
                ErlangEditor.EDITOR_INDENT_WIDTH, 3, 0, true);
        indentWidthText = addTextField.getKey();
        indentWidthText.setEnabled(false);
        indentWidthKey = addTextField.getValue();

        label = ErlEditorMessages.ErlEditorPreferencePage_enable_hover;

        final Pair<Button, String> addCheckBox = addCheckBox(appearanceComposite, label,
                ErlideUIConstants.EDITOR_ENABLE_HOVER, 0);
        enableHoverCheckBox = addCheckBox.getKey();
        enableHoverKey = addCheckBox.getValue();

        return appearanceComposite;
    }

    /*
     * @see PreferencePage#createContents(Composite)
     */
    @Override
    protected Control createContents(final Composite parent) {

        final Control control = createAppearancePage(parent);
        Dialog.applyDialogFont(control);
        setToPreferences();
        return control;
    }

    /*
     * @see PreferencePage#performDefaults()
     */
    @Override
    protected void performDefaults() {

        enableHoverCheckBox.setSelection(DEFAULT_ENABLE_HOVER);
        indentWidthText.setText(Integer.toString(DEFAULT_INDENT_WIDTH));

        super.performDefaults();
    }
}
