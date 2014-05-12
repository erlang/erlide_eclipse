package org.erlide.ui.prefs.plugin;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.util.StatusInfo;
import org.erlide.util.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

import com.google.common.collect.Lists;

public abstract class ErlidePreferencePage extends PreferencePage {

    protected static List<String> getPreferences(final String dialogKey,
            final String[] keys, final String[] defaults) {
        final List<String> l = new ArrayList<String>(keys.length);
        final Preferences node = ErlideUIPlugin.getPrefsNode();
        for (int i = 0; i < keys.length; ++i) {
            final String s = node.get(dialogKey + "/" + keys[i], //$NON-NLS-1$
                    defaults[i]);
            l.add(s);
        }
        return l;
    }

    protected static void addKeysAndPrefs(final String dialogKey, final String[] keys,
            final String[] defaults, final Map<String, String> m) {
        final List<String> prefs = getPreferences(dialogKey, keys, defaults);
        for (int i = 0; i < keys.length; ++i) {
            m.put(keys[i], prefs.get(i));
        }
    }

    public void setToPreferences(final String[] keys, final String[] defaults,
            final List<Button> buttons) {
        final List<String> p = getPreferences(getDialogPreferenceKey(), keys, defaults);
        final List<Boolean> l = getBooleanPreferences(p);
        for (int i = 0; i < l.size(); ++i) {
            final boolean b = l.get(i);
            buttons.get(i).setSelection(b);
        }
    }

    @SuppressWarnings("boxing")
    protected static List<Boolean> getBooleanPreferences(final List<String> p) {
        final List<Boolean> l = new ArrayList<Boolean>(p.size());
        for (final String i : p) {
            l.add(!"0".equals(i) && !"false".equals(i)); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return l;
    }

    public static IStatus validatePositiveNumber(final String number) {
        final StatusInfo status = new StatusInfo();
        if (number.length() == 0) {
            status.setError(ErlEditorMessages.ErlEditorPreferencePage_empty_input);
        } else {
            try {
                final int value = Integer.parseInt(number);
                if (value < 0) {
                    status.setError(MessageFormat.format(
                            ErlEditorMessages.ErlEditorPreferencePage_invalid_input,
                            (Object[]) new String[] { number }));
                }
            } catch (final NumberFormatException e) {
                status.setError(MessageFormat.format(
                        ErlEditorMessages.ErlEditorPreferencePage_invalid_input,
                        (Object[]) new String[] { number }));
            }
        }
        return status;
    }

    /**
     * Applies the status to the status line of a dialog page.
     *
     * @param page
     *            the dialog page
     * @param status
     *            the status
     */
    public static void applyToStatusLine(final DialogPage page, final IStatus status) {
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

    protected Collection<Text> fNumberFields = Lists.newArrayList();
    private final ModifyListener fNumberFieldListener = new ModifyListener() {

        @Override
        public void modifyText(final ModifyEvent e) {
            numberFieldChanged((Text) e.widget);
        }
    };

    public ErlidePreferencePage() {
        super();
    }

    public ErlidePreferencePage(final String title) {
        super(title);
    }

    public ErlidePreferencePage(final String title, final ImageDescriptor image) {
        super(title, image);
    }

    @Override
    public boolean performOk() {
        putPreferences();
        try {
            final Preferences node = ErlideUIPlugin.getPrefsNode();
            node.flush();
        } catch (final BackingStoreException e) {
            ErlLogger.warn(e);
        }
        return true;
    }

    protected abstract void putPreferences();

    protected void putBooleanPreferences(final String[] keys, final List<Button> buttons) {
        final Preferences node = ErlideUIPlugin.getPrefsNode();
        for (int i = 0; i < keys.length; ++i) {
            final boolean b = buttons.get(i).getSelection();
            node.putBoolean(getDialogPreferenceKey() + "/" + keys[i], b); //$NON-NLS-1$
        }
    }

    protected void setToDefaults(final String[] keys, final String[] defaults,
            final List<Button> buttons) {
        for (int i = 0; i < keys.length; ++i) {
            buttons.get(i).setSelection(!defaults[i].equals("0")); //$NON-NLS-1$
        }
    }

    protected void addCheckboxes(final Composite composite, final String[] nlStrings,
            final List<Button> list) {
        for (final String s : nlStrings) {
            final Button b = addCheckBox(composite, s);
            list.add(b);
        }
    }

    protected Button addCheckBox(final Composite composite, final String label) {
        final Button checkBox = new Button(composite, SWT.CHECK);
        checkBox.setText(label);
        return checkBox;
    }

    protected Pair<Button, String> addCheckBox(final Composite parent,
            final String label, final String key, final int indentation) {
        final Button checkBox = new Button(parent, SWT.CHECK);
        checkBox.setText(label);

        final GridData gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
        gd.horizontalIndent = indentation;
        gd.horizontalSpan = 2;
        checkBox.setLayoutData(gd);

        return new Pair<Button, String>(checkBox, getDialogPreferenceKey() + "/" + key);
    }

    protected abstract String getDialogPreferenceKey();

    protected Pair<Text, String> addTextField(final Composite composite,
            final String label, final String key, final int textLimit,
            final int indentation, final boolean isNumber) {

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
        if (isNumber) {
            textControl.addModifyListener(getNumberFieldListener());
            fNumberFields.add(textControl);
        }
        return new Pair<Text, String>(textControl, getDialogPreferenceKey() + "/" + key);
    }

    protected void numberFieldChanged(final Text textControl) {
        final String number = textControl.getText();
        final IStatus status = validatePositiveNumber(number);
        updateStatus(status);
    }

    protected void updateStatus(final IStatus status0) {
        IStatus status = status0;
        if (!status.matches(IStatus.ERROR)) {
            for (final Text text : fNumberFields) {
                final IStatus s = validatePositiveNumber(text.getText());
                status = s.getSeverity() > status.getSeverity() ? s : status;
            }
        }
        setValid(!status.matches(IStatus.ERROR));
        applyToStatusLine(this, status);
    }

    public ModifyListener getNumberFieldListener() {
        return fNumberFieldListener;
    }

    protected void putIntPreferences(final String[] keys, final List<Text> textFields) {
        final Preferences node = ErlideUIPlugin.getPrefsNode();
        for (int i = 0; i < keys.length; ++i) {
            int n;
            n = Integer.parseInt(textFields.get(i).getText());
            node.putInt(getDialogPreferenceKey() + "/" + keys[i], n); //$NON-NLS-1$
        }
    }

    protected List<String> getPreferences(final String[] indentKeys,
            final String[] indentDefaults) {
        return getPreferences(getDialogPreferenceKey(), indentKeys, indentDefaults);
    }

}
