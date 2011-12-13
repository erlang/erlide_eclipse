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
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.util.OverlayPreferenceStore;
import org.erlide.ui.util.StatusInfo;
import org.osgi.service.prefs.BackingStoreException;

/**
 * The editor preferences
 * 
 * 
 * @author Vlad Dumitrescu
 */
public class EditorPreferencePage extends PreferencePage implements
        IWorkbenchPreferencePage {

    /**
     * Initialize the system preferences
     * 
     */
    public EditorPreferencePage() {
        setDescription(ErlEditorMessages.ErlEditorPreferencePage_description);
        setPreferenceStore(ErlideUIPlugin.getDefault().getPreferenceStore());

        fOverlayStore = createOverlayStore();
    }

    /**
     * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
    public void init(final IWorkbench workbench) {
        setPreferenceStore(ErlideUIPlugin.getDefault().getPreferenceStore());
    }

    final String[][] fAppearanceColorListModel = new String[][] {
            {
                    ErlEditorMessages.ErlEditorPreferencePage_lineNumberForegroundColor,
                    AbstractDecoratedTextEditorPreferenceConstants.EDITOR_LINE_NUMBER_RULER_COLOR,
                    null },
            {
                    ErlEditorMessages.ErlEditorPreferencePage_currentLineHighlighColor,
                    AbstractDecoratedTextEditorPreferenceConstants.EDITOR_CURRENT_LINE_COLOR,
                    null },
            {
                    ErlEditorMessages.ErlEditorPreferencePage_printMarginColor,
                    AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN_COLOR,
                    null },
            {
                    ErlEditorMessages.ErlEditorPreferencePage_selectionForegroundColor,
                    AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SELECTION_FOREGROUND_COLOR,
                    AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SELECTION_FOREGROUND_DEFAULT_COLOR },
            {
                    ErlEditorMessages.ErlEditorPreferencePage_selectionBackgroundColor,
                    AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SELECTION_BACKGROUND_COLOR,
                    AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SELECTION_BACKGROUND_DEFAULT_COLOR }, };

    OverlayPreferenceStore fOverlayStore;

    Map<Control, String> fCheckBoxes = new HashMap<Control, String>();

    // private final SelectionListener fCheckBoxListener = new
    // SelectionListener() {
    //
    // public void widgetDefaultSelected(final SelectionEvent e) {
    // }
    //
    // public void widgetSelected(final SelectionEvent e) {
    // final Button button = (Button) e.widget;
    // fOverlayStore.setValue(fCheckBoxes.get(button), button
    // .getSelection());
    // }
    // };

    Map<Control, String> fTextFields = new HashMap<Control, String>();

    private final ModifyListener fTextFieldListener = new ModifyListener() {

        @Override
        public void modifyText(final ModifyEvent e) {
            final Text text = (Text) e.widget;
            fOverlayStore.setValue(fTextFields.get(text), text.getText());
        }
    };

    private final ArrayList<Control> fNumberFields = new ArrayList<Control>();

    private final ModifyListener fNumberFieldListener = new ModifyListener() {

        @Override
        public void modifyText(final ModifyEvent e) {
            numberFieldChanged((Text) e.widget);
        }
    };

    // List fAppearanceColorList;
    //
    // ColorEditor fAppearanceColorEditor;
    //
    // Button fAppearanceColorDefault;

    /**
     * Tells whether the fields are initialized.
     */
    private boolean fFieldsInitialized = false;

    /**
     * List of master/slave listeners when there's a dependency.
     * 
     * @see #createDependency(Button, String, Control)
     */
    private final ArrayList<SelectionListener> fMasterSlaveListeners = new ArrayList<SelectionListener>();

    private OverlayPreferenceStore createOverlayStore() {

        final ArrayList<OverlayPreferenceStore.OverlayKey> overlayKeys = new ArrayList<OverlayPreferenceStore.OverlayKey>();

        overlayKeys
                .add(new OverlayPreferenceStore.OverlayKey(
                        OverlayPreferenceStore.TypeDescriptor.STRING,
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_CURRENT_LINE_COLOR));
        overlayKeys
                .add(new OverlayPreferenceStore.OverlayKey(
                        OverlayPreferenceStore.TypeDescriptor.BOOLEAN,
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_CURRENT_LINE));

        overlayKeys
                .add(new OverlayPreferenceStore.OverlayKey(
                        OverlayPreferenceStore.TypeDescriptor.INT,
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_TAB_WIDTH));

        overlayKeys
                .add(new OverlayPreferenceStore.OverlayKey(
                        OverlayPreferenceStore.TypeDescriptor.STRING,
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN_COLOR));
        overlayKeys
                .add(new OverlayPreferenceStore.OverlayKey(
                        OverlayPreferenceStore.TypeDescriptor.INT,
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN_COLUMN));
        overlayKeys
                .add(new OverlayPreferenceStore.OverlayKey(
                        OverlayPreferenceStore.TypeDescriptor.BOOLEAN,
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN));

        overlayKeys
                .add(new OverlayPreferenceStore.OverlayKey(
                        OverlayPreferenceStore.TypeDescriptor.BOOLEAN,
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_OVERVIEW_RULER));

        overlayKeys
                .add(new OverlayPreferenceStore.OverlayKey(
                        OverlayPreferenceStore.TypeDescriptor.STRING,
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_LINE_NUMBER_RULER_COLOR));
        overlayKeys
                .add(new OverlayPreferenceStore.OverlayKey(
                        OverlayPreferenceStore.TypeDescriptor.BOOLEAN,
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_LINE_NUMBER_RULER));
        overlayKeys
                .add(new OverlayPreferenceStore.OverlayKey(
                        OverlayPreferenceStore.TypeDescriptor.BOOLEAN,
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_USE_CUSTOM_CARETS));
        overlayKeys
                .add(new OverlayPreferenceStore.OverlayKey(
                        OverlayPreferenceStore.TypeDescriptor.BOOLEAN,
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_WIDE_CARET));

        overlayKeys
                .add(new OverlayPreferenceStore.OverlayKey(
                        OverlayPreferenceStore.TypeDescriptor.STRING,
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SELECTION_FOREGROUND_COLOR));
        overlayKeys
                .add(new OverlayPreferenceStore.OverlayKey(
                        OverlayPreferenceStore.TypeDescriptor.BOOLEAN,
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SELECTION_FOREGROUND_DEFAULT_COLOR));
        overlayKeys
                .add(new OverlayPreferenceStore.OverlayKey(
                        OverlayPreferenceStore.TypeDescriptor.STRING,
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SELECTION_BACKGROUND_COLOR));
        overlayKeys
                .add(new OverlayPreferenceStore.OverlayKey(
                        OverlayPreferenceStore.TypeDescriptor.BOOLEAN,
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SELECTION_BACKGROUND_DEFAULT_COLOR));

        return new OverlayPreferenceStore(
                getPreferenceStore(),
                overlayKeys
                        .toArray(new OverlayPreferenceStore.OverlayKey[overlayKeys
                                .size()]));
    }

    /*
     * @see PreferencePage#createControl(Composite)
     */
    @Override
    public void createControl(final Composite parent) {
        super.createControl(parent);
        // WorkbenchHelp.setHelp(getControl(),
        // ITextEditorHelpContextIds.TEXT_EDITOR_PREFERENCE_PAGE);
    }

    // void handleAppearanceColorListSelection() {
    // final int i = fAppearanceColorList.getSelectionIndex();
    // final String key = fAppearanceColorListModel[i][1];
    // final RGB rgb = PreferenceConverter.getColor(fOverlayStore, key);
    // fAppearanceColorEditor.setColorValue(rgb);
    // updateAppearanceColorWidgets(fAppearanceColorListModel[i][2]);
    // }
    //
    // private void updateAppearanceColorWidgets(final String systemDefaultKey)
    // {
    // if (systemDefaultKey == null) {
    // fAppearanceColorDefault.setSelection(false);
    // fAppearanceColorDefault.setVisible(false);
    // fAppearanceColorEditor.getButton().setEnabled(true);
    // } else {
    // final boolean systemDefault = fOverlayStore
    // .getBoolean(systemDefaultKey);
    // fAppearanceColorDefault.setSelection(systemDefault);
    // fAppearanceColorDefault.setVisible(true);
    // fAppearanceColorEditor.getButton().setEnabled(!systemDefault);
    // }
    // }

    private Control createAppearancePage(final Composite parent) {

        final Composite appearanceComposite = new Composite(parent, SWT.NONE);
        final GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        appearanceComposite.setLayout(layout);

        final String label = ErlEditorMessages.ErlEditorPreferencePage_displayedTabWidth;
        addTextField(
                appearanceComposite,
                label,
                AbstractDecoratedTextEditorPreferenceConstants.EDITOR_TAB_WIDTH,
                3, 0, true);

        // label = ErlEditorMessages.ErlEditorPreferencePage_printMarginColumn;
        // addTextField(
        // appearanceComposite,
        // label,
        // AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN_COLUMN,
        // 3, 0, true);

        // label = ErlEditorMessages.ErlEditorPreferencePage_showOverviewRuler;
        // addCheckBox(
        // appearanceComposite,
        // label,
        // AbstractDecoratedTextEditorPreferenceConstants.EDITOR_OVERVIEW_RULER,
        // 0);

        // label = ErlEditorMessages.ErlEditorPreferencePage_showLineNumbers;
        // addCheckBox(
        // appearanceComposite,
        // label,
        // AbstractDecoratedTextEditorPreferenceConstants.EDITOR_LINE_NUMBER_RULER,
        // 0);
        //
        // label =
        // ErlEditorMessages.ErlEditorPreferencePage_highlightCurrentLine;
        // addCheckBox(
        // appearanceComposite,
        // label,
        // AbstractDecoratedTextEditorPreferenceConstants.EDITOR_CURRENT_LINE,
        // 0);
        //
        // label = ErlEditorMessages.ErlEditorPreferencePage_showPrintMargin;
        // addCheckBox(
        // appearanceComposite,
        // label,
        // AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN,
        // 0);

        // label =
        // ErlEditorMessages.ErlEditorPreferencePage_accessibility_disableCustomCarets;
        // final Button master = addCheckBox(
        // appearanceComposite,
        // label,
        // AbstractDecoratedTextEditorPreferenceConstants.EDITOR_USE_CUSTOM_CARETS,
        // 0);
        //
        // label =
        // ErlEditorMessages.ErlEditorPreferencePage_accessibility_wideCaret;
        // final Button slave = addCheckBox(
        // appearanceComposite,
        // label,
        // AbstractDecoratedTextEditorPreferenceConstants.EDITOR_WIDE_CARET,
        // 0);
        // createDependency(
        // master,
        // AbstractDecoratedTextEditorPreferenceConstants.EDITOR_USE_CUSTOM_CARETS,
        // slave);
        //
        // Label l = new Label(appearanceComposite, SWT.LEFT);
        // GridData gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
        // gd.horizontalSpan = 2;
        // gd.heightHint = convertHeightInCharsToPixels(1) / 2;
        // l.setLayoutData(gd);
        //
        // l = new Label(appearanceComposite, SWT.LEFT);
        // l.setText(ErlEditorMessages.ErlEditorPreferencePage_appearanceOptions);
        // gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
        // gd.horizontalSpan = 2;
        // l.setLayoutData(gd);
        //
        // final Composite editorComposite = new Composite(appearanceComposite,
        // SWT.NONE);
        // layout = new GridLayout();
        // layout.numColumns = 2;
        // layout.marginHeight = 0;
        // layout.marginWidth = 0;
        // editorComposite.setLayout(layout);
        // gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL
        // | GridData.FILL_VERTICAL);
        // gd.horizontalSpan = 2;
        // editorComposite.setLayoutData(gd);
        //
        // fAppearanceColorList = new List(editorComposite, SWT.SINGLE
        // | SWT.V_SCROLL | SWT.BORDER);
        // gd = new GridData(GridData.VERTICAL_ALIGN_BEGINNING
        // | GridData.FILL_HORIZONTAL);
        // gd.heightHint = convertHeightInCharsToPixels(5);
        // fAppearanceColorList.setLayoutData(gd);
        //
        // final Composite stylesComposite = new Composite(editorComposite,
        // SWT.NONE);
        // layout = new GridLayout();
        // layout.marginHeight = 0;
        // layout.marginWidth = 0;
        // layout.numColumns = 2;
        // stylesComposite.setLayout(layout);
        // stylesComposite.setLayoutData(new GridData(GridData.FILL_BOTH));
        //
        // l = new Label(stylesComposite, SWT.LEFT);
        // l.setText(ErlEditorMessages.ErlEditorPreferencePage_color);
        // gd = new GridData();
        // gd.horizontalAlignment = GridData.BEGINNING;
        // l.setLayoutData(gd);
        //
        // fAppearanceColorEditor = new ColorEditor(stylesComposite);
        // final Button foregroundColorButton =
        // fAppearanceColorEditor.getButton();
        // gd = new GridData(GridData.FILL_HORIZONTAL);
        // gd.horizontalAlignment = GridData.BEGINNING;
        // foregroundColorButton.setLayoutData(gd);
        //
        // final SelectionListener colorDefaultSelectionListener = new
        // SelectionListener() {
        //
        // public void widgetSelected(final SelectionEvent e) {
        // final boolean systemDefault = fAppearanceColorDefault
        // .getSelection();
        // fAppearanceColorEditor.getButton().setEnabled(!systemDefault);
        //
        // final int i = fAppearanceColorList.getSelectionIndex();
        // final String key = fAppearanceColorListModel[i][2];
        // if (key != null) {
        // fOverlayStore.setValue(key, systemDefault);
        // }
        // }
        //
        // public void widgetDefaultSelected(final SelectionEvent e) {
        // }
        // };
        //
        // fAppearanceColorDefault = new Button(stylesComposite, SWT.CHECK);
        // fAppearanceColorDefault
        // .setText(ErlEditorMessages.ErlEditorPreferencePage_systemDefault);
        // gd = new GridData(GridData.FILL_HORIZONTAL);
        // gd.horizontalAlignment = GridData.BEGINNING;
        // gd.horizontalSpan = 2;
        // fAppearanceColorDefault.setLayoutData(gd);
        // fAppearanceColorDefault.setVisible(false);
        // fAppearanceColorDefault
        // .addSelectionListener(colorDefaultSelectionListener);
        //
        // fAppearanceColorList.addSelectionListener(new SelectionListener() {
        //
        // public void widgetDefaultSelected(final SelectionEvent e) {
        // // do nothing
        // }
        //
        // public void widgetSelected(final SelectionEvent e) {
        // handleAppearanceColorListSelection();
        // }
        // });
        // foregroundColorButton.addSelectionListener(new SelectionListener() {
        //
        // public void widgetDefaultSelected(final SelectionEvent e) {
        // // do nothing
        // }
        //
        // public void widgetSelected(final SelectionEvent e) {
        // final int i = fAppearanceColorList.getSelectionIndex();
        // final String key = fAppearanceColorListModel[i][1];
        //
        // PreferenceConverter.setValue(fOverlayStore, key,
        // fAppearanceColorEditor.getColorValue());
        // }
        // });

        return appearanceComposite;
    }

    /*
     * @see PreferencePage#createContents(Composite)
     */
    @Override
    protected Control createContents(final Composite parent) {

        initializeDefaultColors();

        fOverlayStore.load();
        fOverlayStore.start();

        final Control control = createAppearancePage(parent);
        initialize();
        Dialog.applyDialogFont(control);
        return control;
    }

    private void initialize() {

        initializeFields();

        // for (final String[] element : fAppearanceColorListModel) {
        // fAppearanceColorList.add(element[0]);
        // }
        // fAppearanceColorList.getDisplay().asyncExec(new Runnable() {
        //
        // public void run() {
        // if (fAppearanceColorList != null
        // && !fAppearanceColorList.isDisposed()) {
        // fAppearanceColorList.select(0);
        // handleAppearanceColorListSelection();
        // }
        // }
        // });
    }

    private void initializeFields() {

        Iterator<Control> e = fCheckBoxes.keySet().iterator();
        while (e.hasNext()) {
            final Button b = (Button) e.next();
            final String key = fCheckBoxes.get(b);
            b.setSelection(fOverlayStore.getBoolean(key));
        }

        e = fTextFields.keySet().iterator();
        while (e.hasNext()) {
            final Text t = (Text) e.next();
            final String key = fTextFields.get(t);
            t.setText(fOverlayStore.getString(key));
        }

        fFieldsInitialized = true;
        updateStatus(validatePositiveNumber("0")); //$NON-NLS-1$

        // Update slaves
        final Iterator<SelectionListener> iter = fMasterSlaveListeners
                .iterator();
        while (iter.hasNext()) {
            final SelectionListener listener = iter.next();
            listener.widgetSelected(null);
        }
    }

    private void initializeDefaultColors() {
        if (!getPreferenceStore()
                .contains(
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SELECTION_BACKGROUND_COLOR)) {
            final RGB rgb = getControl().getDisplay()
                    .getSystemColor(SWT.COLOR_LIST_SELECTION).getRGB();
            PreferenceConverter
                    .setDefault(
                            fOverlayStore,
                            AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SELECTION_BACKGROUND_COLOR,
                            rgb);
            PreferenceConverter
                    .setDefault(
                            getPreferenceStore(),
                            AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SELECTION_BACKGROUND_COLOR,
                            rgb);
        }
        if (!getPreferenceStore()
                .contains(
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SELECTION_FOREGROUND_COLOR)) {
            final RGB rgb = getControl().getDisplay()
                    .getSystemColor(SWT.COLOR_LIST_SELECTION_TEXT).getRGB();
            PreferenceConverter
                    .setDefault(
                            fOverlayStore,
                            AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SELECTION_FOREGROUND_COLOR,
                            rgb);
            PreferenceConverter
                    .setDefault(
                            getPreferenceStore(),
                            AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SELECTION_FOREGROUND_COLOR,
                            rgb);
        }
    }

    /*
     * @see PreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        fOverlayStore.propagate();
        final IEclipsePreferences prefsNode = ErlideUIPlugin.getPrefsNode();
        try {
            prefsNode.flush();
        } catch (final BackingStoreException e) {
            e.printStackTrace();
        }
        return true;
    }

    /*
     * @see PreferencePage#performDefaults()
     */
    @Override
    protected void performDefaults() {

        // fOverlayStore.loadDefaults();
        fOverlayStore.load();

        initializeFields();

        // handleAppearanceColorListSelection();

        super.performDefaults();
    }

    /*
     * @see DialogPage#dispose()
     */
    @Override
    public void dispose() {

        if (fOverlayStore != null) {
            fOverlayStore.stop();
            fOverlayStore = null;
        }

        super.dispose();
    }

    // private Button addCheckBox(final Composite parent, final String label,
    // final String key, final int indentation) {
    // final Button checkBox = new Button(parent, SWT.CHECK);
    // checkBox.setText(label);
    //
    // final GridData gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
    // gd.horizontalIndent = indentation;
    // gd.horizontalSpan = 2;
    // checkBox.setLayoutData(gd);
    // checkBox.addSelectionListener(fCheckBoxListener);
    //
    // fCheckBoxes.put(checkBox, key);
    //
    // return checkBox;
    // }

    private Control addTextField(final Composite composite, final String label,
            final String key, final int textLimit, final int indentation,
            final boolean isNumber) {

        final Label labelControl = new Label(composite, SWT.NONE);
        labelControl.setText(label);
        GridData gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
        gd.horizontalIndent = indentation;
        labelControl.setLayoutData(gd);

        final Text textControl = new Text(composite, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
        gd.widthHint = convertWidthInCharsToPixels(textLimit + 1);
        textControl.setLayoutData(gd);
        textControl.setTextLimit(textLimit);
        fTextFields.put(textControl, key);
        if (isNumber) {
            fNumberFields.add(textControl);
            textControl.addModifyListener(fNumberFieldListener);
        } else {
            textControl.addModifyListener(fTextFieldListener);
        }

        return textControl;
    }

    // private void createDependency(final Button master, final String
    // masterKey,
    // final Control slave) {
    // indent(slave);
    //
    // final boolean masterState = fOverlayStore.getBoolean(masterKey);
    // slave.setEnabled(masterState);
    //
    // final SelectionListener listener = new SelectionListener() {
    //
    // public void widgetSelected(final SelectionEvent e) {
    // slave.setEnabled(master.getSelection());
    // }
    //
    // public void widgetDefaultSelected(final SelectionEvent e) {
    // }
    // };
    // master.addSelectionListener(listener);
    // fMasterSlaveListeners.add(listener);
    // }

    // private static void indent(final Control control) {
    // final GridData gridData = new GridData();
    // gridData.horizontalIndent = 20;
    // control.setLayoutData(gridData);
    // }

    void numberFieldChanged(final Text textControl) {
        final String number = textControl.getText();
        final IStatus status = validatePositiveNumber(number);
        if (!status.matches(IStatus.ERROR)) {
            fOverlayStore.setValue(fTextFields.get(textControl), number);
        }
        updateStatus(status);
    }

    public static IStatus validatePositiveNumber(final String number) {
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

    void updateStatus(IStatus status) {
        if (!fFieldsInitialized) {
            return;
        }

        if (!status.matches(IStatus.ERROR)) {
            for (int i = 0; i < fNumberFields.size(); i++) {
                final Text text = (Text) fNumberFields.get(i);
                final IStatus s = validatePositiveNumber(text.getText());
                status = s.getSeverity() > status.getSeverity() ? s : status;
            }
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
    public static void applyToStatusLine(final DialogPage page,
            final IStatus status) {
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
}
