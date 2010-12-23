/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/

package org.erlide.core.preferences;

import java.net.URISyntaxException;
import java.util.Collection;
import java.util.Collections;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IOldErlangProjectProperties;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.backend.RuntimeVersion;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.collect.Lists;

public final class OldErlangProjectProperties implements
        IPreferenceChangeListener, IOldErlangProjectProperties {

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

    public OldErlangProjectProperties() {
    }

    public OldErlangProjectProperties(final IProject prj) {
        super();
        project = prj;
        final IEclipsePreferences root = new ProjectScope(project)
                .getNode(ErlangPlugin.PLUGIN_ID);
        // TODO load() should not be in constructor!
        load(root);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.erlide.core.preferences.IOldErlangProjectProperties#load(org.eclipse
     * .core.runtime.preferences.IEclipsePreferences)
     */
    public void load(final IEclipsePreferences node) {
        if (project == null) {
            return;
        }

        if ("true".equals(System.getProperty("erlide.newprops"))) {
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
        outputDir = new Path(node.get(ProjectPreferencesConstants.OUTPUT_DIR,
                ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR));
        runtimeVersion = new RuntimeVersion(node.get(
                ProjectPreferencesConstants.RUNTIME_VERSION, null));
        runtimeName = node.get(ProjectPreferencesConstants.RUNTIME_NAME, null);
        if (!runtimeVersion.isDefined()) {
            if (runtimeName == null) {
                runtimeVersion = new RuntimeVersion(
                        ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
            } else {
                final RuntimeInfo ri = ErlangCore.getRuntimeInfoManager()
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
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.erlide.core.preferences.IOldErlangProjectProperties#store(org.eclipse
     * .core.runtime.preferences.IEclipsePreferences)
     */
    public void store(final IEclipsePreferences node) {
        if (project == null) {
            return;
        }

        if ("true".equals(System.getProperty("erlide.newprops"))) {
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
            node.put(ProjectPreferencesConstants.OUTPUT_DIR,
                    outputDir.toString());
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

            try {
                node.flush();
            } catch (final BackingStoreException e1) {
            }
        } finally {
            node.addPreferenceChangeListener(this);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.erlide.core.preferences.IOldErlangProjectProperties#getIncludeDirs()
     */
    public Collection<IPath> getIncludeDirs() {
        return Collections.unmodifiableCollection(includeDirs);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.erlide.core.preferences.IOldErlangProjectProperties#setIncludeDirs
     * (java.util.Collection)
     */
    public void setIncludeDirs(final Collection<IPath> includeDirs2) {
        includeDirs = Lists.newArrayList(includeDirs2);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.erlide.core.preferences.IOldErlangProjectProperties#getOutputDir()
     */
    public IPath getOutputDir() {
        return outputDir;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.erlide.core.preferences.IOldErlangProjectProperties#setOutputDir(
     * org.eclipse.core.runtime.IPath)
     */
    public void setOutputDir(final IPath dir) {
        if (!outputDir.equals(dir)) {
            // try {
            // final Backend b = ErlangCore.getBackendManager()
            // .getBuildBackend(project);
            // String p =
            // project.getLocation().append(outputDir).toString();
            // b.removePath(getUsePathZ(), p);
            //
            // p = project.getLocation().append(dir).toString();
            // b.addPath(getUsePathZ(), p);

            outputDir = dir;
            // } catch (final BackendException e) {
            // ErlLogger.warn(e);
            // }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.erlide.core.preferences.IOldErlangProjectProperties#getSourceDirs()
     */
    public Collection<IPath> getSourceDirs() {
        return Collections.unmodifiableCollection(sourceDirs);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.erlide.core.preferences.IOldErlangProjectProperties#setSourceDirs
     * (java.util.Collection)
     */
    public void setSourceDirs(final Collection<IPath> sourceDirs2) {
        sourceDirs = Lists.newArrayList(sourceDirs2);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.erlide.core.preferences.IOldErlangProjectProperties#copyFrom(org.
     * erlide.core.preferences.OldErlangProjectProperties)
     */
    public void copyFrom(
            final IOldErlangProjectProperties erlangProjectProperties) {
        final OldErlangProjectProperties bprefs = (OldErlangProjectProperties) erlangProjectProperties;
        includeDirs = bprefs.includeDirs;
        sourceDirs = bprefs.sourceDirs;
        outputDir = bprefs.outputDir;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.erlide.core.preferences.IOldErlangProjectProperties#
     * getExternalIncludesFile()
     */
    public String getExternalIncludesFile() {
        return externalIncludesFile;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.erlide.core.preferences.IOldErlangProjectProperties#
     * setExternalIncludesFile(java.lang.String)
     */
    public void setExternalIncludesFile(final String file) {
        externalIncludesFile = file;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.erlide.core.preferences.IOldErlangProjectProperties#
     * setExternalModulesFile(java.lang.String)
     */
    public void setExternalModulesFile(final String externalModules) {
        externalModulesFile = externalModules;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.erlide.core.preferences.IOldErlangProjectProperties#
     * getExternalModulesFile()
     */
    public String getExternalModulesFile() {
        return externalModulesFile;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.erlide.core.preferences.IOldErlangProjectProperties#getRuntimeInfo()
     */
    public RuntimeInfo getRuntimeInfo() {
        final RuntimeInfo runtime = ErlangCore.getRuntimeInfoManager()
                .getRuntime(runtimeVersion, runtimeName);
        RuntimeInfo rt = null;
        if (runtime != null) {
            rt = RuntimeInfo.copy(runtime, false);
        }
        return rt;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.erlide.core.preferences.IOldErlangProjectProperties#hasSourceDir(
     * org.eclipse.core.runtime.IPath)
     */
    public boolean hasSourceDir(final IPath fullPath) {
        final IPath f = fullPath.removeFirstSegments(1);
        for (final IPath s : getSourceDirs()) {
            if (s.equals(f)) {
                return true;
            }
            if (fullPath.segmentCount() == 1 && s.toString().equals(".")) {
                return true;
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.erlide.core.preferences.IOldErlangProjectProperties#getRuntimeVersion
     * ()
     */
    public RuntimeVersion getRuntimeVersion() {
        return runtimeVersion;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.erlide.core.preferences.IOldErlangProjectProperties#preferenceChange
     * (org
     * .eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent
     * )
     */
    public void preferenceChange(final PreferenceChangeEvent event) {

        System.out.println("PROP CHANGE DETECTED IN OLDPREFS " + event);

        final IEclipsePreferences root = new ProjectScope(project)
                .getNode(ErlangPlugin.PLUGIN_ID);
        load(root);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.erlide.core.preferences.IOldErlangProjectProperties#setRuntimeVersion
     * (org.erlide.jinterface.backend.RuntimeVersion)
     */
    public void setRuntimeVersion(final RuntimeVersion runtimeVersion) {
        this.runtimeVersion = runtimeVersion;
    }

}
