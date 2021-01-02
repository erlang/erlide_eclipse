package org.erlide.engine.model.root;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.PreferencesUtils;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;

@SuppressWarnings("all")
public class ErlangProjectProperties {
    private IPath outputDir;

    private Collection<IPath> sourceDirs;

    private Collection<IPath> includeDirs;

    private Collection<IPath> testDirs;

    private RuntimeVersion requiredRuntimeVersion;

    private String externalIncludesFile;

    private String externalModulesFile;

    public static final ErlangProjectProperties DEFAULT = ObjectExtensions
            .<ErlangProjectProperties> operator_doubleArrow(new ErlangProjectProperties(),
                    (Procedure1<ErlangProjectProperties>) (
                            final ErlangProjectProperties it) -> {
                        it.sourceDirs = PathSerializer.unpackList(
                                ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS);
                        final Path _path = new Path(
                                ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR);
                        it.outputDir = _path;
                        it.includeDirs = PathSerializer.unpackList(
                                ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS);
                        it.testDirs = PathSerializer.unpackList(
                                ProjectPreferencesConstants.DEFAULT_TEST_DIRS);
                        it.externalIncludesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES;
                        it.externalModulesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES;
                        it.requiredRuntimeVersion = ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION;
                    });

    public ErlangProjectProperties() {
        sourceDirs = CollectionLiterals.<IPath> newArrayList();
        final Path _path = new Path("");
        outputDir = _path;
        includeDirs = CollectionLiterals.<IPath> newArrayList();
        testDirs = CollectionLiterals.<IPath> newArrayList();
        externalIncludesFile = "";
        externalModulesFile = "";
        requiredRuntimeVersion = ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION;
    }

    public void setSourceDirs(final Collection<IPath> dirs) {
        sourceDirs = Lists.<IPath> newArrayList(dirs);
    }

    public void setSourceDirs(final IPath... dirs) {
        sourceDirs = Lists.<IPath> newArrayList(dirs);
    }

    public void setIncludeDirs(final Collection<IPath> dirs) {
        includeDirs = Lists.<IPath> newArrayList(dirs);
    }

    public void setIncludeDirs(final IPath... dirs) {
        includeDirs = Lists.<IPath> newArrayList(dirs);
    }

    public void setTestDirs(final Collection<IPath> dirs) {
        testDirs = Lists.<IPath> newArrayList(dirs);
    }

    public void setTestDirs(final IPath... dirs) {
        testDirs = Lists.<IPath> newArrayList(dirs);
    }

    public void copyFrom(final ErlangProjectProperties props) {
        includeDirs = props.includeDirs;
        testDirs = props.testDirs;
        sourceDirs = props.sourceDirs;
        outputDir = props.outputDir;
        requiredRuntimeVersion = props.requiredRuntimeVersion;
        externalIncludesFile = props.externalIncludesFile;
        externalModulesFile = props.externalModulesFile;
    }

    public String getExternalIncludes() {
        final String externalIncludesString = this
                .getExternal(ExternalKind.EXTERNAL_INCLUDES);
        return externalIncludesString;
    }

    public String getExternalModules() {
        final String externalModulesString = this
                .getExternal(ExternalKind.EXTERNAL_MODULES);
        return externalModulesString;
    }

    private String getExternal(final ExternalKind external) {
        final IPreferencesService service = Platform.getPreferencesService();
        String _xifexpression = null;
        final boolean _equals = Objects.equal(external, ExternalKind.EXTERNAL_INCLUDES);
        if (_equals) {
            _xifexpression = "default_external_includes";
        } else {
            _xifexpression = "default_external_modules";
        }
        final String key = _xifexpression;
        String result = this.getExternal(external, service, key, "org.erlide.ui");
        final boolean _isNullOrEmpty = Strings.isNullOrEmpty(result);
        if (_isNullOrEmpty) {
            result = this.getExternal(external, service, key, "org.erlide.core");
        }
        return result;
    }

    private String getExternal(final ExternalKind external,
            final IPreferencesService service, final String key, final String pluginId) {
        final String global = service.getString(pluginId, key, "", null);
        String _xifexpression = null;
        final boolean _equals = Objects.equal(external, ExternalKind.EXTERNAL_INCLUDES);
        if (_equals) {
            _xifexpression = externalIncludesFile;
        } else {
            _xifexpression = externalModulesFile;
        }
        final String projprefs = _xifexpression;
        return PreferencesUtils.packArray(new String[] { projprefs, global });
    }

    public IPath getOutputDir() {
        return outputDir;
    }

    public void setOutputDir(final IPath outputDir) {
        this.outputDir = outputDir;
    }

    public Collection<IPath> getSourceDirs() {
        return sourceDirs;
    }

    public Collection<IPath> getIncludeDirs() {
        return includeDirs;
    }

    public Collection<IPath> getTestDirs() {
        return testDirs;
    }

    public RuntimeVersion getRequiredRuntimeVersion() {
        return requiredRuntimeVersion;
    }

    public void setRequiredRuntimeVersion(final RuntimeVersion requiredRuntimeVersion) {
        this.requiredRuntimeVersion = requiredRuntimeVersion;
    }

    public String getExternalIncludesFile() {
        return externalIncludesFile;
    }

    public void setExternalIncludesFile(final String externalIncludesFile) {
        this.externalIncludesFile = externalIncludesFile;
    }

    public String getExternalModulesFile() {
        return externalModulesFile;
    }

    public void setExternalModulesFile(final String externalModulesFile) {
        this.externalModulesFile = externalModulesFile;
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final ErlangProjectProperties other = (ErlangProjectProperties) obj;
        if (!java.util.Objects.equals(outputDir, other.outputDir)) {
            return false;
        }
        if (!java.util.Objects.equals(sourceDirs, other.sourceDirs)) {
            return false;
        }
        if (!java.util.Objects.equals(includeDirs, other.includeDirs)) {
            return false;
        }
        if (!java.util.Objects.equals(testDirs, other.testDirs)) {
            return false;
        }
        if (!java.util.Objects.equals(requiredRuntimeVersion,
                other.requiredRuntimeVersion)) {
            return false;
        }
        if (!java.util.Objects.equals(externalIncludesFile, other.externalIncludesFile)) {
            return false;
        }
        if (!java.util.Objects.equals(externalModulesFile, other.externalModulesFile)) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        return java.util.Objects.hash(outputDir, sourceDirs, includeDirs, testDirs, requiredRuntimeVersion,
                externalIncludesFile, externalModulesFile);
    }

    @Override
    public String toString() {
        final ToStringBuilder b = new ToStringBuilder(this);
        b.add("outputDir", outputDir);
        b.add("sourceDirs", sourceDirs);
        b.add("includeDirs", includeDirs);
        b.add("testDirs", testDirs);
        b.add("requiredRuntimeVersion", requiredRuntimeVersion);
        b.add("externalIncludesFile", externalIncludesFile);
        b.add("externalModulesFile", externalModulesFile);
        return b.toString();
    }
}
