package org.erlide.ui.prefs.plugin;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.layout.PixelConverter;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.util.StatusInfo;
import org.osgi.service.prefs.BackingStoreException;

public class CodeAssistPreferencePage extends PreferencePage implements
        IWorkbenchPreferencePage {

    private Button autoActivateButton;
    private Text delayText;
    private Text erlangTriggersText;
    private Text eDocTriggersText;
    private CodeAssistPreferences prefs;
    private Label labelControl;

    @Override
    protected Control createContents(final Composite parent) {
        final Composite composite = new Composite(parent, SWT.NONE);
        // composite.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true,
        // false));
        // composite.setLayout(new GridLayout(3, false));
        final GridData data = new GridData(SWT.FILL, SWT.CENTER, true, false);
        composite.setLayoutData(data);
        final GridLayout layout = new GridLayout();
        layout.numColumns = 3;
        composite.setLayout(layout);
        addAutoActivationSection(composite);

        performDefaults();

        updateAutoActivationControls();
        return composite;
    }

    private void addAutoActivationSection(final Composite composite) {
        autoActivateButton = new Button(composite, SWT.CHECK);
        autoActivateButton.setText("&Enable auto activation");
        GridData gd;
        // gd = new GridData(SWT.FILL, SWT.FILL, true,
        // false, 3, 1);
        gd = new GridData();
        gd.horizontalSpan = 3;
        autoActivateButton.setLayoutData(gd);

        autoActivateButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                updateAutoActivationControls();
            }
        });

        delayText = addLabelledTextField(composite, "Auto activation dela&y:",
                4, 0);
        erlangTriggersText = addLabelledTextField(composite,
                "Auto activation triggers for &Erlang:", 100, 4, 0);
        eDocTriggersText = addLabelledTextField(composite,
                "Auto activation triggers for E&Doc:", 100, 4, 0);
        eDocTriggersText.setVisible(false);
        labelControl.setVisible(false);
    }

    protected void updateAutoActivationControls() {
        final boolean enabled = autoActivateButton.getSelection();
        delayText.setEnabled(enabled);
        erlangTriggersText.setEnabled(enabled);
        eDocTriggersText.setEnabled(enabled);
    }

    protected Text addLabelledTextField(final Composite parent,
            final String label, final int textlimit, final int indent) {
        return addLabelledTextField(parent, label, textlimit, textlimit, indent);
    }

    protected Text addLabelledTextField(final Composite parent,
            final String label, final int modelTextLimit,
            final int fieldTextLimit, final int indent) {
        final PixelConverter pixelConverter = new PixelConverter(parent);

        labelControl = new Label(parent, SWT.WRAP);
        labelControl.setText(label);
        labelControl.setLayoutData(new GridData());

        final Text textBox = new Text(parent, SWT.BORDER | SWT.SINGLE);
        textBox.setLayoutData(new GridData());

        textBox.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(final ModifyEvent e) {
                validateSettings();
            }
        });

        final GridData data = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
        if (modelTextLimit != 0) {
            textBox.setTextLimit(modelTextLimit);
        }

        if (fieldTextLimit != 0) {
            data.widthHint = pixelConverter
                    .convertWidthInCharsToPixels(fieldTextLimit + 1);
        }

        data.horizontalIndent = indent;
        data.horizontalSpan = 2;
        textBox.setLayoutData(data);

        return textBox;
    }

    protected void validateSettings() {
        statusChanged(EditorPreferencePage.validatePositiveNumber(delayText
                .getText()));
        statusChanged(validateCommaSeparatedCharacters(erlangTriggersText
                .getText()));
        statusChanged(validateCommaSeparatedCharacters(eDocTriggersText
                .getText()));
    }

    private IStatus validateCommaSeparatedCharacters(final String text) {
        final StatusInfo status = new StatusInfo();
        status.setOK();
        if (text.length() > 1) {
            final String chars[] = text.split(",");
            for (final String c : chars) {
                if (c.trim().length() != 1) {
                    status.setError("Trigger keys should be a list of comma-separated characters");
                    break;
                }
            }
        }
        return status;
    }

    private void statusChanged(final IStatus status) {
        final boolean valid = !status.matches(IStatus.ERROR);
        setValid(valid);
        EditorPreferencePage.applyToStatusLine(this, status);
    }

    @Override
    public boolean performOk() {
        try {
            prefs.setAutoActivate(autoActivateButton.getSelection());
            final Integer i = new Integer(delayText.getText());
            prefs.setDelayInMS(i.intValue());
            prefs.setErlangTriggers(erlangTriggersText.getText());
            prefs.seteDocTriggers(eDocTriggersText.getText());
            prefs.store();
        } catch (final BackingStoreException e) {
            ErlLogger.warn(e);
        }
        return super.performOk();
    }

    @Override
    protected void performDefaults() {
        prefs = new CodeAssistPreferences();
        try {
            prefs.load();
            if (autoActivateButton == null) {
                return;
            }
            autoActivateButton.setSelection(prefs.isAutoActivate());
            delayText.setText(Integer.toString(prefs.getDelayInMS()));
            erlangTriggersText.setText(prefs.getErlangTriggers());
            eDocTriggersText.setText(prefs.geteDocTriggers());
        } catch (final BackingStoreException e) {
            ErlLogger.warn(e);
        }
        super.performDefaults();
    }

    @Override
    public void init(final IWorkbench workbench) {
    }
}
