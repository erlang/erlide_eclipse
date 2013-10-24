package org.erlide.core.internal.builder;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.PathSerializer;
import org.erlide.engine.model.root.ProjectConfigurator;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.runtime.api.RuntimeCore;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;

public class InternalConfigurator implements ProjectConfigurator {

    @Override
    public String encodeConfig(final IProject project, final ErlangProjectProperties info) {
        // nothing to do here
        return null;
    }

    @Override
    public ErlangProjectProperties decodeConfig(final String config) {
        // nothing to do here
        return null;
    }

    @Override
    public String getConfigFile() {
        // none
        return null;
    }

    private void load(final IEclipsePreferences node) {
        final ErlangProjectProperties result = new ErlangProjectProperties();
        if (node == null) {
            return;
        }

        final String sourceDirsStr = node.get(ProjectPreferencesConstants.SOURCE_DIRS,
                ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS);
        result.setSourceDirs(PathSerializer.unpackList(sourceDirsStr));
        final String includeDirsStr = node.get(ProjectPreferencesConstants.INCLUDE_DIRS,
                ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS);
        result.setIncludeDirs(PathSerializer.unpackList(includeDirsStr));
        final String outputDirsStr = node.get(ProjectPreferencesConstants.OUTPUT_DIR,
                ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR);
        result.setOutputDir(new Path(outputDirsStr));
        result.setRuntimeVersion(new RuntimeVersion(node.get(
                ProjectPreferencesConstants.RUNTIME_VERSION, null)));
        result.setRuntimeName(node.get(ProjectPreferencesConstants.RUNTIME_NAME, null));
        if (!result.getRuntimeVersion().isDefined()) {
            if (result.getRuntimeName() == null) {
                result.setRuntimeVersion(new RuntimeVersion(
                        ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION));
            } else {
                final RuntimeInfo info = RuntimeCore.getRuntimeInfoCatalog().getRuntime(
                        result.getRuntimeName());
                if (info != null) {
                    result.setRuntimeVersion(new RuntimeVersion(info.getVersion()));
                } else {
                    result.setRuntimeVersion(new RuntimeVersion(
                            ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION));
                }
            }
        }
        result.setExternalModulesFile(node.get(
                ProjectPreferencesConstants.PROJECT_EXTERNAL_MODULES,
                ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES));
        result.setExternalIncludesFile(node.get(
                ProjectPreferencesConstants.EXTERNAL_INCLUDES,
                ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES));
        result.setNukeOutputOnClean(node.getBoolean(
                ProjectPreferencesConstants.NUKE_OUTPUT_ON_CLEAN, false));
    }

    public void store(final IEclipsePreferences node, final ErlangProjectProperties result) {
        if (node == null) {
            return;
        }

        node.put(ProjectPreferencesConstants.SOURCE_DIRS,
                PathSerializer.packList(result.getSourceDirs()));
        node.put(ProjectPreferencesConstants.INCLUDE_DIRS,
                PathSerializer.packList(result.getIncludeDirs()));
        node.put(ProjectPreferencesConstants.OUTPUT_DIR, result.getOutputDir()
                .toPortableString());
        node.put(ProjectPreferencesConstants.EXTERNAL_INCLUDES,
                result.getExternalIncludesFile());
        if (result.getRuntimeVersion().isDefined()) {
            node.put(ProjectPreferencesConstants.RUNTIME_VERSION, result
                    .getRuntimeVersion().asMinor().toString());
        } else {
            node.remove(ProjectPreferencesConstants.RUNTIME_VERSION);
        }
        if (result.getRuntimeName() != null) {
            node.put(ProjectPreferencesConstants.RUNTIME_NAME, result.getRuntimeName());
        } else {
            node.remove(ProjectPreferencesConstants.RUNTIME_NAME);
        }
        node.put(ProjectPreferencesConstants.PROJECT_EXTERNAL_MODULES,
                result.getExternalModulesFile());
        node.putBoolean(ProjectPreferencesConstants.NUKE_OUTPUT_ON_CLEAN,
                result.isNukeOutputOnClean());

        try {
            node.flush();
        } catch (final BackingStoreException e) {
            ErlLogger.warn(e);
        }
    }

}
