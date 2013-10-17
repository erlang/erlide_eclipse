/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/

package org.erlide.engine.model.root;

import java.util.Collection;
import java.util.Collections;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.erlide.runtime.api.RuntimeCore;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.collect.Lists;

public final class ErlangProjectProperties implements
        IPreferenceChangeListener, IErlangProjectProperties {

    private IProject project;

    private Collection<IPath> sourceDirs = PathSerializer
            .unpackList(ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS);
    private IPath outputDir = new Path(
            ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR);
    private Collection<IPath> includeDirs = PathSerializer
            .unpackList(ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS);
    private String externalIncludesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES;
    private String externalModulesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES;
    private RuntimeVersion runtimeVersion = new RuntimeVersion(
            ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
    private String runtimeName = null;

    private boolean nukeOutputOnClean = false;
    private String builderName = "internal";
    private final String builderCompileTarget = "compile";
    private final String builderCleanTarget = "clean";

    private String name;

    private IPath location;

    public ErlangProjectProperties() {
    }

    public ErlangProjectProperties(final IProject prj) {
        super();
        project = prj;
        // TODO load() should not be in constructor!
        load();
    }

    @Override
    public void preferenceChange(final PreferenceChangeEvent event) {
        load();
    }

    private void load() {
        if (project == null) {
            return;
        }
        final IEclipsePreferences node = new ProjectScope(project)
                .getNode("org.erlide.core");

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
        outputDir = new Path(outputDirsStr);
        runtimeVersion = new RuntimeVersion(node.get(
                ProjectPreferencesConstants.RUNTIME_VERSION, null));
        runtimeName = node.get(ProjectPreferencesConstants.RUNTIME_NAME, null);
        if (!runtimeVersion.isDefined()) {
            if (runtimeName == null) {
                runtimeVersion = new RuntimeVersion(
                        ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
            } else {
                final RuntimeInfo info = RuntimeCore.getRuntimeInfoCatalog()
                        .getRuntime(runtimeName);
                if (info != null) {
                    runtimeVersion = new RuntimeVersion(info.getVersion());
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
        setBuilderName(node
                .get(ProjectPreferencesConstants.BUILDER, "internal"));
    }

    @Override
    public void store() throws BackingStoreException {
        if (project == null) {
            return;
        }
        final IEclipsePreferences node = new ProjectScope(project)
                .getNode("org.erlide.core");
        node.removePreferenceChangeListener(this);

        try {
            node.put(ProjectPreferencesConstants.SOURCE_DIRS,
                    PathSerializer.packList(sourceDirs));
            node.put(ProjectPreferencesConstants.INCLUDE_DIRS,
                    PathSerializer.packList(includeDirs));
            node.put(ProjectPreferencesConstants.OUTPUT_DIR,
                    outputDir.toPortableString());
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

            node.put(ProjectPreferencesConstants.BUILDER, getBuilderName()
                    .toString());

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
    public IPath getOutputDir() {
        return outputDir;
    }

    @Override
    public void setOutputDir(final IPath dir) {
        outputDir = dir;
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
    public void copyFrom(final IErlangProjectProperties erlangProjectProperties) {
        final IErlangProjectProperties bprefs = erlangProjectProperties;
        includeDirs = bprefs.getIncludeDirs();
        sourceDirs = bprefs.getSourceDirs();
        outputDir = bprefs.getOutputDir();
        runtimeName = bprefs.getRuntimeName();
        runtimeVersion = bprefs.getRequiredRuntimeVersion();
        builderName = bprefs.getBuilderName();
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
        final RuntimeInfo runtime = RuntimeCore.getRuntimeInfoCatalog()
                .getRuntime(runtimeVersion, runtimeName);
        return runtime;
    }

    @Override
    public RuntimeVersion getRuntimeVersion() {
        final RuntimeInfo runtimeInfo = getRuntimeInfo();
        return runtimeInfo != null ? runtimeInfo.getVersion() : runtimeVersion;
    }

    @Override
    public void setRuntimeVersion(final RuntimeVersion runtimeVersion) {
        this.runtimeVersion = runtimeVersion;
    }

    @Override
    public boolean isNukeOutputOnClean() {
        return nukeOutputOnClean;
    }

    @Override
    public void setNukeOutputOnClean(final boolean nukeOutputOnClean) {
        this.nukeOutputOnClean = nukeOutputOnClean;
    }

    @Override
    public RuntimeVersion getRequiredRuntimeVersion() {
        return runtimeVersion;
    }

    @Deprecated
    @Override
    public String getRuntimeName() {
        return runtimeName;
    }

    @Override
    public String getBuilderName() {
        return builderName;
    }

    @Override
    public void setBuilderName(final String builder) {
        builderName = builder;
    }

    @Override
    public void setName(final String projectName) {
        name = projectName;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public IPath getLocation() {
        return location;
    }

    @Override
    public void setLocation(final IPath location) {
        this.location = location;
    }
}
