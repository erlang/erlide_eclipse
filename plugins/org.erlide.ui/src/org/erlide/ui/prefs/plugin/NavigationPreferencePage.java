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

public class NavigationPreferencePage extends ErlidePreferencePage
        implements IWorkbenchPreferencePage {

    private static Boolean fCachedCheckAllProjects;
    private final List<Button> buttons = new ArrayList<>();

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
    private static final String[] NAVIGATION_KEYS = {
            NavigationPreferencePage.CHECK_ALL_PROJECTS_KEY };
    private static final String[] NAVIGATION_DEFAULTS = { "1" };

    @Override
    protected void performDefaults() {
        setToDefaults(NavigationPreferencePage.NAVIGATION_KEYS,
                NavigationPreferencePage.NAVIGATION_DEFAULTS, buttons);
        super.performDefaults();
    }

    @Override
    protected void putPreferences() {
        putBooleanPreferences(NavigationPreferencePage.NAVIGATION_KEYS, buttons);
        NavigationPreferencePage.fCachedCheckAllProjects = null;
    }

    private void setToPreferences() {
        setToPreferences(NavigationPreferencePage.NAVIGATION_KEYS,
                NavigationPreferencePage.NAVIGATION_DEFAULTS, buttons);
    }

    public static boolean getCheckAllProjects() {
        if (NavigationPreferencePage.fCachedCheckAllProjects == null) {
            final IEclipsePreferences node = ErlideUIPlugin.getPrefsNode();
            final boolean checkAllProjects = node
                    .getBoolean(
                            NavigationPreferencePage.NAVIGATION_KEY + "/"
                                    + NavigationPreferencePage.CHECK_ALL_PROJECTS_KEY,
                            true);
            NavigationPreferencePage.fCachedCheckAllProjects = checkAllProjects;
        }
        return NavigationPreferencePage.fCachedCheckAllProjects;
    }

    @Override
    public void init(final IWorkbench workbench) {
    }

    @Override
    protected String getDialogPreferenceKey() {
        return NavigationPreferencePage.NAVIGATION_KEY;
    }

}
