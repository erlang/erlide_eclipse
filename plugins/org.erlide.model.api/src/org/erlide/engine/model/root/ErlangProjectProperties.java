package org.erlide.engine.model.root;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtend.lib.annotations.EqualsHashCode;
import org.eclipse.xtend.lib.annotations.ToString;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.PreferencesUtils;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;

@Accessors
@EqualsHashCode
@ToString
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

    @Pure
    public IPath getOutputDir() {
        return outputDir;
    }

    public void setOutputDir(final IPath outputDir) {
        this.outputDir = outputDir;
    }

    @Pure
    public Collection<IPath> getSourceDirs() {
        return sourceDirs;
    }

    @Pure
    public Collection<IPath> getIncludeDirs() {
        return includeDirs;
    }

    @Pure
    public Collection<IPath> getTestDirs() {
        return testDirs;
    }

    @Pure
    public RuntimeVersion getRequiredRuntimeVersion() {
        return requiredRuntimeVersion;
    }

    public void setRequiredRuntimeVersion(final RuntimeVersion requiredRuntimeVersion) {
        this.requiredRuntimeVersion = requiredRuntimeVersion;
    }

    @Pure
    public String getExternalIncludesFile() {
        return externalIncludesFile;
    }

    public void setExternalIncludesFile(final String externalIncludesFile) {
        this.externalIncludesFile = externalIncludesFile;
    }

    @Pure
    public String getExternalModulesFile() {
        return externalModulesFile;
    }

    public void setExternalModulesFile(final String externalModulesFile) {
        this.externalModulesFile = externalModulesFile;
    }

    @Override
    @Pure
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
        if (outputDir == null) {
            if (other.outputDir != null) {
                return false;
            }
        } else if (!outputDir.equals(other.outputDir)) {
            return false;
        }
        if (sourceDirs == null) {
            if (other.sourceDirs != null) {
                return false;
            }
        } else if (!sourceDirs.equals(other.sourceDirs)) {
            return false;
        }
        if (includeDirs == null) {
            if (other.includeDirs != null) {
                return false;
            }
        } else if (!includeDirs.equals(other.includeDirs)) {
            return false;
        }
        if (testDirs == null) {
            if (other.testDirs != null) {
                return false;
            }
        } else if (!testDirs.equals(other.testDirs)) {
            return false;
        }
        if (requiredRuntimeVersion == null) {
            if (other.requiredRuntimeVersion != null) {
                return false;
            }
        } else if (!requiredRuntimeVersion.equals(other.requiredRuntimeVersion)) {
            return false;
        }
        if (externalIncludesFile == null) {
            if (other.externalIncludesFile != null) {
                return false;
            }
        } else if (!externalIncludesFile.equals(other.externalIncludesFile)) {
            return false;
        }
        if (externalModulesFile == null) {
            if (other.externalModulesFile != null) {
                return false;
            }
        } else if (!externalModulesFile.equals(other.externalModulesFile)) {
            return false;
        }
        return true;
    }

    @Override
    @Pure
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + (outputDir == null ? 0 : outputDir.hashCode());
        result = prime * result + (sourceDirs == null ? 0 : sourceDirs.hashCode());
        result = prime * result + (includeDirs == null ? 0 : includeDirs.hashCode());
        result = prime * result + (testDirs == null ? 0 : testDirs.hashCode());
        result = prime * result + (requiredRuntimeVersion == null ? 0
                : requiredRuntimeVersion.hashCode());
        result = prime * result
                + (externalIncludesFile == null ? 0 : externalIncludesFile.hashCode());
        return prime * result
                + (externalModulesFile == null ? 0 : externalModulesFile.hashCode());
    }

    @Override
    @Pure
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
