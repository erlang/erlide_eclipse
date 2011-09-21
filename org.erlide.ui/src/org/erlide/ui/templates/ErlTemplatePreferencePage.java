package org.erlide.ui.templates;

import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.texteditor.templates.TemplatePreferencePage;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.prefs.plugin.ColoringPreferencePage;

public class ErlTemplatePreferencePage extends TemplatePreferencePage {

    private Button fIndentCodeCheckBox;

    @Override
    protected Control createContents(final Composite ancestor) {
        final Control contents = super.createContents(ancestor);
        fIndentCodeCheckBox = new Button(ancestor, SWT.CHECK);
        fIndentCodeCheckBox.setText("Indent code");
        fIndentCodeCheckBox.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(final SelectionEvent e) {
                ErlTemplateCompletionPreferences
                        .setIndentCode(fIndentCodeCheckBox.getSelection());
            }
        });
        getPrefs();
        fIndentCodeCheckBox.setSelection(ErlTemplateCompletionPreferences
                .getIndentCode());
        return contents;
    }

    public ErlTemplatePreferencePage() {
        setPreferenceStore(ErlideUIPlugin.getDefault().getPreferenceStore());
        setTemplateStore(ErlideUIPlugin.getDefault().getTemplateStore());
        setContextTypeRegistry(ErlideUIPlugin.getDefault()
                .getContextTypeRegistry());
    }

    @Override
    protected void performDefaults() {
        ErlTemplateCompletionPreferences.getDefaults();
        fIndentCodeCheckBox.setSelection(ErlTemplateCompletionPreferences
                .getIndentCode());
        super.performDefaults();
    }

    private static void putPrefs() {
        ErlTemplateCompletionPreferences.putPrefs();
    }

    private void getPrefs() {
        ErlTemplateCompletionPreferences.getPrefs();
    }

    @Override
    protected void performApply() {
        ErlTemplatePreferencePage.putPrefs();
        super.performApply();
    }

    @Override
    protected boolean isShowFormatterSetting() {
        return false;
    }

    @Override
    public boolean performOk() {
        final boolean ok = super.performOk();

        if (ok) {
            ErlTemplatePreferencePage.putPrefs();
        }

        return ok;
    }

    @Override
    protected SourceViewer createViewer(final Composite parent) {
        final SourceViewer viewer = ColoringPreferencePage
                .createErlangPreviewer(parent, null, null, "");
        // new SourceViewer(parent, null, null, false, SWT.BORDER | SWT.V_SCROLL
        // | SWT.H_SCROLL);
        // final SourceViewerConfiguration configuration= new
        // SourceViewerConfiguration();
        // viewer.configure(configuration);
        final IDocument document = new Document();
        viewer.setDocument(document);
        return viewer;
    }
}
