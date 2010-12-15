package org.erlide.core.builder;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.preferences.PreferencesHelper;
import org.osgi.service.prefs.BackingStoreException;

public class DialyzerPreferences {

    private static final String QUALIFIER = ErlangPlugin.PLUGIN_ID
            + "/dialyzer";

    private final PreferencesHelper helper;

    private String pltPath;
    private boolean fromSource;
    private boolean dialyzeOnCompile;

    public static DialyzerPreferences get(final IProject project)
            throws CoreException {
        final DialyzerPreferences prefs = new DialyzerPreferences(project);
        try {
            prefs.load();
        } catch (final BackingStoreException e1) {
            e1.printStackTrace();
            throw new CoreException(new Status(IStatus.ERROR,
                    ErlangPlugin.PLUGIN_ID,
                    "could not retrieve compiler options"));
        }
        return prefs;
    }

    public DialyzerPreferences() {
        helper = PreferencesHelper.getHelper(QUALIFIER);
    }

    public DialyzerPreferences(final IProject project) {
        helper = PreferencesHelper.getHelper(QUALIFIER, project);
    }

    public boolean hasOptionsAtLowestScope() {
        return helper.hasAnyAtLowestScope();
    }

    public void store() throws BackingStoreException {
        helper.putString(DialyzerPreferencesConstants.PLT_PATH, getPltPath());
        helper.putBoolean(DialyzerPreferencesConstants.FROM_SOURCE,
                getFromSource());
        helper.putBoolean(DialyzerPreferencesConstants.DIALYZE_ON_COMPILE,
                getDialyzeOnCompile());
        helper.flush();
    }

    @SuppressWarnings("boxing")
    public void load() throws BackingStoreException {
        setPltPath(helper.getString(DialyzerPreferencesConstants.PLT_PATH, ""));
        setFromSource(helper.getBoolean(
                DialyzerPreferencesConstants.FROM_SOURCE, true));
        setDialyzeOnCompile(helper.getBoolean(
                DialyzerPreferencesConstants.DIALYZE_ON_COMPILE, false));
    }

    @Override
    public String toString() {
        return getPltPath().toString() + ", " + getFromSource() + ", "
                + getDialyzeOnCompile();
    }

    public void removeAllProjectSpecificSettings() {
        helper.removeAllAtLowestScope();
    }

    public void setFromSource(final boolean fromSource) {
        this.fromSource = fromSource;
    }

    public boolean getFromSource() {
        return fromSource;
    }

    public void setPltPath(final String pltPath) {
        this.pltPath = pltPath;
    }

    public String getPltPath() {
        return pltPath;
    }

    public void setDialyzeOnCompile(final boolean dialyzeOnCompile) {
        this.dialyzeOnCompile = dialyzeOnCompile;
    }

    public boolean getDialyzeOnCompile() {
        return dialyzeOnCompile;
    }
}
