package org.erlide.dialyzer.builder;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.erlide.core.ErlangCore;
import org.erlide.engine.util.PreferencesHelper;
import org.erlide.util.PreferencesUtils;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.base.Charsets;
import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.google.common.io.Files;

public final class DialyzerPreferences {

    private static final String QUALIFIER = ErlangCore.PLUGIN_ID + "/dialyzer";

    private final PreferencesHelper helper;

    private String pltPaths;
    private Collection<String> pltPathsFromPrefs;
    private String enabledPltPaths;
    private boolean fromSource;
    private boolean dialyzeOnCompile;
    private boolean noCheckPLT;
    private boolean removeWarningsOnClean;

    public static DialyzerPreferences get(final IProject project) {
        final DialyzerPreferences prefs = new DialyzerPreferences();
        prefs.load();
        if (project != null) {
            final DialyzerPreferences projectPrefs = new DialyzerPreferences(project);
            projectPrefs.load();
            projectPrefs.pltPaths = prefs.pltPaths;
            return projectPrefs;
        }
        return prefs;
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
        helper.putString(DialyzerPreferencesConstants.ENABLED_PLT_PATHS, enabledPltPaths);
        helper.putBoolean(DialyzerPreferencesConstants.FROM_SOURCE, getFromSource());
        helper.putBoolean(DialyzerPreferencesConstants.DIALYZE_ON_COMPILE,
                getDialyzeOnCompile());
        helper.putBoolean(DialyzerPreferencesConstants.NO_CHECK_PLT, noCheckPLT);
        helper.putBoolean(DialyzerPreferencesConstants.REMOVE_WARNINGS_ON_CLEAN,
                removeWarningsOnClean);
        helper.flush();
    }

    private static Collection<String> getPLTPathsFromPreferences() {
        final IPreferencesService service = Platform.getPreferencesService();
        final String key = "default_plt_files";
        final String pluginId = "org.erlide.ui";
        final String pltFilesString = service.getString(pluginId, key, "", null);
        return getPltFiles(pltFilesString);
    }

    public static String getAlternatePLTFileDirectoryFromPreferences() {
        final IPreferencesService service = Platform.getPreferencesService();
        final String key = "alternate_plt_file_directory";
        final String pluginId = "org.erlide.ui";
        return service.getString(pluginId, key, "", null);
    }

    public void load() {
        pltPaths = helper.getString(DialyzerPreferencesConstants.PLT_PATHS, "");
        pltPathsFromPrefs = getPLTPathsFromPreferences();
        enabledPltPaths = helper.getString(
                DialyzerPreferencesConstants.ENABLED_PLT_PATHS, pltPaths);
        setFromSource(helper.getBoolean(DialyzerPreferencesConstants.FROM_SOURCE, true));
        setDialyzeOnCompile(helper.getBoolean(
                DialyzerPreferencesConstants.DIALYZE_ON_COMPILE, false));
        noCheckPLT = helper.getBoolean(DialyzerPreferencesConstants.NO_CHECK_PLT, true);
        removeWarningsOnClean = helper.getBoolean(
                DialyzerPreferencesConstants.REMOVE_WARNINGS_ON_CLEAN, true);
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
        final Set<String> result = Sets.newHashSet();
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

    public void setRemoveWarningsOnClean(final boolean removeWarningsOnClean) {
        this.removeWarningsOnClean = removeWarningsOnClean;
    }

    public boolean getRemoveWarningsOnClean() {
        return removeWarningsOnClean;
    }

    private static Collection<String> getPltFiles(final String pltFilesString) {
        final Iterable<String> files = Splitter.on(",").split(pltFilesString);
        return getPltFiles(files);
    }

    private static Collection<String> getPltFiles(final Iterable<String> files) {
        final List<String> result = Lists.newArrayList();
        for (final String fileName : files) {
            final File file = new File(fileName);
            final IPath p = new Path(fileName);
            if ("plt".equals(p.getFileExtension())) {
                if (file.exists()) {
                    result.add(fileName);
                }
            } else {
                try {
                    final List<String> lines = Files.readLines(file, Charsets.UTF_8);
                    result.addAll(getPltFiles(lines));
                } catch (final IOException e) {
                    // ignore
                }
            }
        }
        return result;
    }
}
