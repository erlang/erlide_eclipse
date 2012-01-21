package org.erlide.core.services.builder;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.erlide.backend.BackendCore;
import org.erlide.core.ErlangCore;
import org.erlide.core.internal.model.erlang.PreferencesHelper;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.utils.PreferencesUtils;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.collect.Lists;

public class DialyzerPreferences {

    private static final String QUALIFIER = ErlangCore.PLUGIN_ID + "/dialyzer";

    private final PreferencesHelper helper;

    private String pltPaths;
    private Collection<String> pltPathsFromPrefs;
    private String enabledPltPaths;
    private boolean fromSource;
    private boolean dialyzeOnCompile;
    private boolean noCheckPLT;

    public static DialyzerPreferences get(final IProject project)
            throws CoreException, RpcException {
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
            throw new CoreException(
                    new Status(IStatus.ERROR, ErlangCore.PLUGIN_ID,
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
        helper.putBoolean(DialyzerPreferencesConstants.NO_CHECK_PLT, noCheckPLT);
        helper.flush();
    }

    private static Collection<String> getPLTPathsFromPreferences()
            throws RpcException {
        final IPreferencesService service = Platform.getPreferencesService();
        final String key = "default_plt_files";
        final String pluginId = "org.erlide.ui";
        final String pltFilesString = service
                .getString(pluginId, key, "", null);
        return ErlideDialyze.getPltFiles(BackendCore.getBackendManager()
                .getIdeBackend(), pltFilesString);
    }

    public static String getAlternatePLTFileDirectoryFromPreferences()
            throws RpcException {
        final IPreferencesService service = Platform.getPreferencesService();
        final String key = "alternate_plt_file_directory";
        final String pluginId = "org.erlide.ui";
        return service.getString(pluginId, key, "", null);
    }

    public void load() throws BackingStoreException, RpcException {
        pltPaths = helper.getString(DialyzerPreferencesConstants.PLT_PATHS, "");
        pltPathsFromPrefs = getPLTPathsFromPreferences();
        enabledPltPaths = helper.getString(
                DialyzerPreferencesConstants.ENABLED_PLT_PATHS, pltPaths);
        setFromSource(helper.getBoolean(
                DialyzerPreferencesConstants.FROM_SOURCE, true));
        setDialyzeOnCompile(helper.getBoolean(
                DialyzerPreferencesConstants.DIALYZE_ON_COMPILE, false));
        noCheckPLT = helper.getBoolean(
                DialyzerPreferencesConstants.NO_CHECK_PLT, true);
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

    public void setPltPaths(final Collection<String> pltPaths) {
        final List<String> paths = Lists.newArrayList(pltPaths);
        if (pltPathsFromPrefs != null) {
            for (final String path : pltPathsFromPrefs) {
                paths.remove(path);
            }
        }
        this.pltPaths = PreferencesUtils.packList(paths);
    }

    public Collection<String> getPltPaths() {
        final List<String> result = Lists.newArrayList();
        result.addAll(PreferencesUtils.unpackList(pltPaths));
        if (pltPathsFromPrefs != null) {
            result.addAll(pltPathsFromPrefs);
        }
        return result;
    }

    public void setDialyzeOnCompile(final boolean dialyzeOnCompile) {
        this.dialyzeOnCompile = dialyzeOnCompile;
    }

    public boolean getDialyzeOnCompile() {
        return dialyzeOnCompile;
    }

    public boolean getNoCheckPLT() {
        return noCheckPLT;
    }

    public void setEnabledPltPaths(final Collection<String> enabledPltPaths) {
        this.enabledPltPaths = PreferencesUtils.packList(enabledPltPaths);
    }

    public Collection<String> getEnabledPltPaths() {
        return PreferencesUtils.unpackList(enabledPltPaths);
    }

    public void setNoCheckPLT(final boolean noCheckPLT) {
        this.noCheckPLT = noCheckPLT;
    }
}
