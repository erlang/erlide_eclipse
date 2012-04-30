/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/

package org.erlide.core.internal.model.root;

import java.net.URISyntaxException;
import java.util.Collection;
import java.util.Collections;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.erlide.backend.BackendCore;
import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.ErlangCore;
import org.erlide.core.internal.model.erlang.PropertiesUtils;
import org.erlide.core.model.root.IOldErlangProjectProperties;
import org.erlide.utils.SystemUtils;
import org.osgi.service.prefs.BackingStoreException;

import com.ericsson.otp.erlang.RuntimeVersion;
import com.google.common.collect.Lists;

public final class OldErlangProjectProperties implements
        IPreferenceChangeListener, IOldErlangProjectProperties {

    private IProject project;

    private Collection<IPath> sourceDirs = PathSerializer
            .unpackList(ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS);
    private Collection<IPath> outputDirs = PathSerializer
            .unpackList(ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR);
    private Collection<IPath> includeDirs = PathSerializer
            .unpackList(ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS);
    private String externalIncludesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES;
    private String externalModulesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES;
    private RuntimeVersion runtimeVersion = new RuntimeVersion(
            ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
    private String runtimeName = null;
    private boolean nukeOutputOnClean = false;

    public OldErlangProjectProperties() {
    }

    public OldErlangProjectProperties(final IProject prj) {
        super();
        project = prj;
        final IEclipsePreferences root = new ProjectScope(project)
                .getNode(ErlangCore.PLUGIN_ID);
        // TODO load() should not be in constructor!
        load(root);
    }

    private void load(final IEclipsePreferences node) {
        if (project == null) {
            return;
        }

        if (SystemUtils.hasFeatureEnabled("erlide.newprops")) {
            try {
                final ErlProjectInfoBuilder builder = new ErlProjectInfoBuilder();
                final ErlProjectInfo npp = builder
                        .loadFromPreferences((IEclipsePreferences) node
                                .node("test"));
                builder.storeToPreferences(npp,
                        (IEclipsePreferences) node.node("new_test"));
            } catch (final BackingStoreException e) {
                e.printStackTrace();
            }
        }

        final String sourceDirsStr = node.get(
                ProjectPreferencesConstants.SOURCE_DIRS,
                ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS);
        sourceDirs = PathSerializer.unpackList(sourceDirsStr);
        final String includeDirsStr = node.get(
                ProjectPreferencesConstants.INCLUDE_DIRS,
                ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS);
        includeDirs = PathSerializer.unpackList(includeDirsStr);
        final String outputDirsStr = node.get(
                ProjectPreferencesConstants.OUTPUT_DIR,
                ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR);
        outputDirs = PathSerializer.unpackList(outputDirsStr);
        runtimeVersion = new RuntimeVersion(node.get(
                ProjectPreferencesConstants.RUNTIME_VERSION, null));
        runtimeName = node.get(ProjectPreferencesConstants.RUNTIME_NAME, null);
        if (!runtimeVersion.isDefined()) {
            if (runtimeName == null) {
                runtimeVersion = new RuntimeVersion(
                        ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
            } else {
                final RuntimeInfo ri = BackendCore.getRuntimeInfoManager()
                        .getRuntime(runtimeName);
                if (ri != null) {
                    runtimeVersion = new RuntimeVersion(ri.getVersion());
                } else {
                    runtimeVersion = new RuntimeVersion(
                            ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
                }
            }
        }
        externalModulesFile = node.get(
                ProjectPreferencesConstants.PROJECT_EXTERNAL_MODULES,
                ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES);
        externalIncludesFile = node.get(
                ProjectPreferencesConstants.EXTERNAL_INCLUDES,
                ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES);
        setNukeOutputOnClean(node.getBoolean(
                ProjectPreferencesConstants.NUKE_OUTPUT_ON_CLEAN, false));
    }

    @Override
    public void store() throws BackingStoreException {
        if (project == null) {
            return;
        }
        final IEclipsePreferences node = new ProjectScope(project)
                .getNode(ErlangCore.PLUGIN_ID);
        if (SystemUtils.hasFeatureEnabled("erlide.newprops")) {
            try {
                final ErlProjectInfo npp = PropertiesUtils.convertOld(this);
                final ErlProjectInfoBuilder builder = new ErlProjectInfoBuilder();
                builder.storeToPreferences(npp,
                        (IEclipsePreferences) node.node("test"));
            } catch (final BackingStoreException e) {
                e.printStackTrace();
            } catch (final URISyntaxException e) {
                e.printStackTrace();
            }
        }

        node.removePreferenceChangeListener(this);

        try {
            node.put(ProjectPreferencesConstants.SOURCE_DIRS,
                    PathSerializer.packList(sourceDirs));
            node.put(ProjectPreferencesConstants.INCLUDE_DIRS,
                    PathSerializer.packList(includeDirs));
            node.put(ProjectPreferencesConstants.OUTPUT_DIR, getOutputDir()
                    .toString());
            node.put(ProjectPreferencesConstants.EXTERNAL_INCLUDES,
                    externalIncludesFile);
            if (runtimeVersion.isDefined()) {
                node.put(ProjectPreferencesConstants.RUNTIME_VERSION,
                        runtimeVersion.asMinor().toString());
            } else {
                node.remove(ProjectPreferencesConstants.RUNTIME_VERSION);
            }
            if (runtimeName != null) {
                node.put(ProjectPreferencesConstants.RUNTIME_NAME, runtimeName);
            } else {
                node.remove(ProjectPreferencesConstants.RUNTIME_NAME);
            }
            node.put(ProjectPreferencesConstants.PROJECT_EXTERNAL_MODULES,
                    externalModulesFile);
            node.putBoolean(ProjectPreferencesConstants.NUKE_OUTPUT_ON_CLEAN,
                    isNukeOutputOnClean());

            node.flush();
        } finally {
            node.addPreferenceChangeListener(this);
        }
    }

    @Override
    public Collection<IPath> getIncludeDirs() {
        return Collections.unmodifiableCollection(includeDirs);
    }

    @Override
    public void setIncludeDirs(final Collection<IPath> includeDirs2) {
        includeDirs = Lists.newArrayList(includeDirs2);
    }

    @Override
    @Deprecated
    public IPath getOutputDir() {
        try {
            return outputDirs.iterator().next();
        } catch (final Exception e) {
            return null;
        }
    }

    @Override
    public Collection<IPath> getOutputDirs() {
        return outputDirs;
    }

    @Override
    @Deprecated
    public void setOutputDir(final IPath dir) {
        setOutputDirs(Lists.newArrayList(dir));
    }

    public void setOutputDirs(final Collection<IPath> dirs) {
        outputDirs = Lists.newArrayList(dirs);
    }

    @Override
    public Collection<IPath> getSourceDirs() {
        return Collections.unmodifiableCollection(sourceDirs);
    }

    @Override
    public void setSourceDirs(final Collection<IPath> sourceDirs2) {
        sourceDirs = Lists.newArrayList(sourceDirs2);
    }

    @Override
    public void copyFrom(
            final IOldErlangProjectProperties erlangProjectProperties) {
        final OldErlangProjectProperties bprefs = (OldErlangProjectProperties) erlangProjectProperties;
        includeDirs = bprefs.includeDirs;
        sourceDirs = bprefs.sourceDirs;
        outputDirs = bprefs.outputDirs;
        runtimeName = bprefs.runtimeName;
        runtimeVersion = bprefs.runtimeVersion;
    }

    @Override
    public String getExternalIncludesFile() {
        return externalIncludesFile;
    }

    @Override
    public void setExternalIncludesFile(final String file) {
        externalIncludesFile = file;
    }

    @Override
    public void setExternalModulesFile(final String externalModules) {
        externalModulesFile = externalModules;
    }

    @Override
    public String getExternalModulesFile() {
        return externalModulesFile;
    }

    @Override
    public RuntimeInfo getRuntimeInfo() {
        final RuntimeInfo runtime = BackendCore.getRuntimeInfoManager()
                .getRuntime(runtimeVersion, runtimeName);
        RuntimeInfo rt = null;
        if (runtime != null) {
            rt = RuntimeInfo.copy(runtime, false);
        }
        return rt;
    }

    @Override
    public RuntimeVersion getRuntimeVersion() {
        return runtimeVersion;
    }

    @Override
    public void preferenceChange(final PreferenceChangeEvent event) {
        final IEclipsePreferences root = new ProjectScope(project)
                .getNode(ErlangCore.PLUGIN_ID);
        load(root);
    }

    @Override
    public void setRuntimeVersion(final RuntimeVersion runtimeVersion) {
        this.runtimeVersion = runtimeVersion;
    }

    public boolean isNukeOutputOnClean() {
        return nukeOutputOnClean;
    }

    public void setNukeOutputOnClean(final boolean nukeOutputOnClean) {
        this.nukeOutputOnClean = nukeOutputOnClean;
    }

}
