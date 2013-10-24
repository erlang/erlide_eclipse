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

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.runtime.api.RuntimeCore;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;

import com.google.common.collect.Lists;

public class ErlangProjectProperties {

    private Collection<IPath> sourceDirs = PathSerializer
            .unpackList(ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS);
    private IPath outputDir = new Path(ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR);
    private Collection<IPath> includeDirs = PathSerializer
            .unpackList(ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS);
    private String externalIncludesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES;
    private String externalModulesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES;
    private RuntimeVersion runtimeVersion = new RuntimeVersion(
            ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
    private String runtimeName = null;

    private boolean nukeOutputOnClean = false;

    public ErlangProjectProperties() {
    }

    public Collection<IPath> getIncludeDirs() {
        return Collections.unmodifiableCollection(includeDirs);
    }

    public void setIncludeDirs(final Collection<IPath> includeDirs2) {
        includeDirs = Lists.newArrayList(includeDirs2);
    }

    public IPath getOutputDir() {
        return outputDir;
    }

    public void setOutputDir(final IPath dir) {
        outputDir = dir;
    }

    public Collection<IPath> getSourceDirs() {
        return Collections.unmodifiableCollection(sourceDirs);
    }

    public void setSourceDirs(final Collection<IPath> sourceDirs2) {
        sourceDirs = Lists.newArrayList(sourceDirs2);
    }

    public void copyFrom(final ErlangProjectProperties erlangProjectProperties) {
        final ErlangProjectProperties bprefs = erlangProjectProperties;
        includeDirs = bprefs.getIncludeDirs();
        sourceDirs = bprefs.getSourceDirs();
        outputDir = bprefs.getOutputDir();
        runtimeName = bprefs.getRuntimeName();
        runtimeVersion = bprefs.getRequiredRuntimeVersion();
    }

    public String getExternalIncludesFile() {
        return externalIncludesFile;
    }

    public void setExternalIncludesFile(final String file) {
        externalIncludesFile = file;
    }

    public void setExternalModulesFile(final String externalModules) {
        externalModulesFile = externalModules;
    }

    public String getExternalModulesFile() {
        return externalModulesFile;
    }

    public RuntimeInfo getRuntimeInfo() {
        final RuntimeInfo runtime = RuntimeCore.getRuntimeInfoCatalog().getRuntime(
                runtimeVersion, runtimeName);
        return runtime;
    }

    public RuntimeVersion getRuntimeVersion() {
        final RuntimeInfo runtimeInfo = getRuntimeInfo();
        return runtimeInfo != null ? runtimeInfo.getVersion() : runtimeVersion;
    }

    public void setRuntimeVersion(final RuntimeVersion runtimeVersion) {
        this.runtimeVersion = runtimeVersion;
    }

    public boolean isNukeOutputOnClean() {
        return nukeOutputOnClean;
    }

    public void setNukeOutputOnClean(final boolean nukeOutputOnClean) {
        this.nukeOutputOnClean = nukeOutputOnClean;
    }

    public RuntimeVersion getRequiredRuntimeVersion() {
        return runtimeVersion;
    }

    @Deprecated
    public String getRuntimeName() {
        return runtimeName;
    }

    @Deprecated
    public void setRuntimeName(final String runtimeName) {
        this.runtimeName = runtimeName;
    }

}
