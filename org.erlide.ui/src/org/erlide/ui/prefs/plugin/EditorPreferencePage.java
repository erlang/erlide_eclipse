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
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.ui.ErlideUIConstants;
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

    private static final int DEFAULT_TAB_WIDTH = 8;

    private static final boolean DEFAULT_ENABLE_HOVER = true;

    private void setToPreferences() {
        final IEclipsePreferences node = ErlideUIPlugin.getPrefsNode();
        final Integer i = node.getInt(tabWidthKey, DEFAULT_TAB_WIDTH);
        tabWidthText.setText(i.toString());
        enableHoverCheckBox.setSelection(node.getBoolean(enableHoverKey,
                DEFAULT_ENABLE_HOVER));
    }

    @Override
    protected void putPreferences() {
        final IEclipsePreferences node = ErlideUIPlugin.getPrefsNode();
        node.putInt(tabWidthKey, Integer.parseInt(tabWidthText.getText()));
        node.putBoolean(enableHoverKey, enableHoverCheckBox.getSelection());
    }

    public static boolean getEnableHover() {
        final IEclipsePreferences node = ErlideUIPlugin.getPrefsNode();
        return node.getBoolean(EDITOR_KEY + "/"
                + ErlideUIConstants.EDITOR_ENABLE_HOVER, DEFAULT_ENABLE_HOVER);
    }

    @Override
    protected String getDialogPreferenceKey() {
        return EDITOR_KEY;
    }

    private Text tabWidthText;
    private Button enableHoverCheckBox;

    private String tabWidthKey;
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

        String label = ErlEditorMessages.ErlEditorPreferencePage_displayedTabWidth;

        final Pair<Text, String> addTextField = addTextField(
                appearanceComposite,
                label,
                AbstractDecoratedTextEditorPreferenceConstants.EDITOR_TAB_WIDTH,
                3, 0, true);
        tabWidthText = addTextField.getKey();
        tabWidthKey = addTextField.getValue();

        label = ErlEditorMessages.ErlEditorPreferencePage_enable_hover;

        final Pair<Button, String> addCheckBox = addCheckBox(
                appearanceComposite, label,
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
        tabWidthText.setText(Integer.toString(DEFAULT_TAB_WIDTH));

        super.performDefaults();
    }
}
