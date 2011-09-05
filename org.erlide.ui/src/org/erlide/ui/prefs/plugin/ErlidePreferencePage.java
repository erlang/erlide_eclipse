package org.erlide.ui.prefs.plugin;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public abstract class ErlidePreferencePage extends PreferencePage {

    protected static List<String> getPreferences(final String key,
            final String[] keys, final String[] defaults) {
        final List<String> l = new ArrayList<String>(keys.length);
        final Preferences node = ErlideUIPlugin.getPrefsNode();
        for (int i = 0; i < keys.length; ++i) {
            final String s = node.get(key + "/" + keys[i], //$NON-NLS-1$
                    defaults[i]);
            l.add(s);
        }
        return l;
    }

    protected static void addKeysAndPrefs(final String key,
            final String[] keys, final String[] defaults,
            final Map<String, String> m) {
        final List<String> prefs = getPreferences(key, keys, defaults);
        for (int i = 0; i < keys.length; ++i) {
            m.put(keys[i], prefs.get(i));
        }
    }

    @SuppressWarnings("boxing")
    public static void setToPreferences(final String key, final String[] keys,
            final String[] defaults, final List<Button> buttons) {
        final List<String> p = getPreferences(key, keys, defaults);
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
            l.add(!i.equals("0") && !i.equals("false")); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return l;
    }

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
        return true;
    }

    protected abstract void putPreferences();

    protected void putPreferences(final String key, final String[] keys,
            final List<Button> buttons) {
        final Preferences node = ErlideUIPlugin.getPrefsNode();
        for (int i = 0; i < keys.length; ++i) {
            final boolean b = buttons.get(i).getSelection();
            node.putBoolean(key + "/" + keys[i], b); //$NON-NLS-1$
        }
        try {
            node.flush();
        } catch (final BackingStoreException e) {
            ErlLogger.warn(e);
        }
    }

    protected void setToDefaults(final String[] keys, final String[] defaults,
            final List<Button> buttons) {
        for (int i = 0; i < keys.length; ++i) {
            buttons.get(i).setSelection(!defaults[i].equals("0")); //$NON-NLS-1$
        }
    }

    protected void addCheckboxes(final Composite composite,
            final String[] nlStrings, final List<Button> list) {
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

}
