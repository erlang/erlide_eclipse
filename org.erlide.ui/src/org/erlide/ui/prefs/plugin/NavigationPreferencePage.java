package org.erlide.ui.prefs.plugin;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.erlide.ui.internal.ErlideUIPlugin;

public class NavigationPreferencePage extends ErlidePreferencePage implements
        IWorkbenchPreferencePage {

    private static Boolean fCachedCheckAllProjects = null;
    private final List<Button> buttons = new ArrayList<Button>();

    private void addCheckAllSection(final Composite composite) {
        final String[] ss = { "Consider all projects on open" };
        addCheckboxes(composite, ss, buttons);
    }

    @Override
    protected Control createContents(final Composite parent) {
        final Composite control = new Composite(parent, SWT.NONE);
        final GridLayout layout = new GridLayout();
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        control.setLayout(layout);

        addCheckAllSection(control);

        setToPreferences();

        return control;
    }

    private static final String NAVIGATION_KEY = "erlangNavigation"; //$NON-NLS-1$
    private static final String CHECK_ALL_PROJECTS_KEY = "checkAllProjects"; //$NON-NLS-1$
    private static final String[] NAVIGATION_KEYS = { CHECK_ALL_PROJECTS_KEY };
    private static final String[] NAVIGATION_DEFAULTS = { "1" };

    @Override
    protected void performDefaults() {
        setToDefaults(NAVIGATION_KEYS, NAVIGATION_DEFAULTS, buttons);
        super.performDefaults();
    }

    @Override
    protected void putPreferences() {
        putBooleanPreferences(NAVIGATION_KEYS, buttons);
        fCachedCheckAllProjects = null;
    }

    private void setToPreferences() {
        setToPreferences(NAVIGATION_KEYS, NAVIGATION_DEFAULTS, buttons);
    }

    public static boolean getCheckAllProjects() {
        if (fCachedCheckAllProjects == null) {
            final IEclipsePreferences node = ErlideUIPlugin.getPrefsNode();
            final boolean checkAllProjects = node.getBoolean(NAVIGATION_KEY + "/"
                    + CHECK_ALL_PROJECTS_KEY, true);
            fCachedCheckAllProjects = checkAllProjects;
        }
        return fCachedCheckAllProjects.booleanValue();
    }

    @Override
    public void init(final IWorkbench workbench) {
    }

    @Override
    protected String getDialogPreferenceKey() {
        return NAVIGATION_KEY;
    }

}
