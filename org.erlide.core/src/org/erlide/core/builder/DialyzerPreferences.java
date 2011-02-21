package org.erlide.core.builder;

import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.preferences.PreferencesHelper;
import org.erlide.jinterface.backend.util.PreferencesUtils;
import org.osgi.service.prefs.BackingStoreException;

public class DialyzerPreferences {

    private static final String QUALIFIER = ErlangPlugin.PLUGIN_ID
            + "/dialyzer";

    private final PreferencesHelper helper;

    private String pltPaths;
    private String enabledPltPaths;
    private boolean fromSource;
    private boolean dialyzeOnCompile;

    public static DialyzerPreferences get(final IProject project)
            throws CoreException {
        try {
            final DialyzerPreferences prefs = new DialyzerPreferences();
            prefs.load();
            if (project != null) {
                final DialyzerPreferences projectPrefs = new DialyzerPreferences(
                        project);
                projectPrefs.load();
                projectPrefs.pltPaths = prefs.pltPaths;
                return projectPrefs;
            } else {
                return prefs;
            }
        } catch (final BackingStoreException e1) {
            e1.printStackTrace();
            throw new CoreException(new Status(IStatus.ERROR,
                    ErlangPlugin.PLUGIN_ID,
                    "could not retrieve dialyzer options"));
        }
    }

    private DialyzerPreferences() {
        helper = PreferencesHelper.getHelper(QUALIFIER);
    }

    private DialyzerPreferences(final IProject project) {
        helper = PreferencesHelper.getHelper(QUALIFIER, project);
    }

    public boolean hasOptionsAtLowestScope() {
        return helper.hasAnyAtLowestScope();
    }

    public void store() throws BackingStoreException {
        helper.putString(DialyzerPreferencesConstants.PLT_PATHS, pltPaths);
        helper.putString(DialyzerPreferencesConstants.ENABLED_PLT_PATHS,
                enabledPltPaths);
        helper.putBoolean(DialyzerPreferencesConstants.FROM_SOURCE,
                getFromSource());
        helper.putBoolean(DialyzerPreferencesConstants.DIALYZE_ON_COMPILE,
                getDialyzeOnCompile());
        helper.flush();
    }

    @SuppressWarnings("boxing")
    public void load() throws BackingStoreException {
        pltPaths = helper.getString(DialyzerPreferencesConstants.PLT_PATHS, "");
        enabledPltPaths = helper.getString(
                DialyzerPreferencesConstants.ENABLED_PLT_PATHS, pltPaths);
        setFromSource(helper.getBoolean(
                DialyzerPreferencesConstants.FROM_SOURCE, true));
        setDialyzeOnCompile(helper.getBoolean(
                DialyzerPreferencesConstants.DIALYZE_ON_COMPILE, false));
    }

    @Override
    public String toString() {
        return pltPaths + ", " + getFromSource() + ", " + getDialyzeOnCompile();
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

    public void setPltPaths(final List<String> pltPaths) {
        this.pltPaths = PreferencesUtils.packList(pltPaths);
    }

    public List<String> getPltPaths() {
        return PreferencesUtils.unpackList(pltPaths);
    }

    public void setDialyzeOnCompile(final boolean dialyzeOnCompile) {
        this.dialyzeOnCompile = dialyzeOnCompile;
    }

    public boolean getDialyzeOnCompile() {
        return dialyzeOnCompile;
    }

    public void setEnabledPltPaths(final List<String> enabledPltPaths) {
        this.enabledPltPaths = PreferencesUtils.packList(enabledPltPaths);
    }

    public List<String> getEnabledPltPaths() {
        return PreferencesUtils.unpackList(enabledPltPaths);
    }
}
