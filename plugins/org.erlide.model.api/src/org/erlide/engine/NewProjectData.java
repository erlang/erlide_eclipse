package org.erlide.engine;

import java.io.File;
import java.util.Map;

import org.eclipse.core.runtime.IPath;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IProjectConfigurator;
import org.erlide.engine.model.root.IProjectConfiguratorFactory;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.engine.model.root.ProjectConfiguratorFactory;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.runtime.api.RuntimeCore;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;

import com.google.common.base.MoreObjects;

@SuppressWarnings("all")
public class NewProjectData extends ErlangProjectProperties {
    private String name = null;

    private IPath location = null;

    private boolean existingProject = false;

    private BuilderTool builder = BuilderTool.INTERNAL;

    private ProjectConfigType configType = ProjectConfigType.INTERNAL;

    private Map<String, String> builderData = CollectionLiterals
            .<String, String> newHashMap();

    private final IProjectConfiguratorFactory factory;

    public NewProjectData() {
        factory = ProjectConfiguratorFactory.getDefault();
    }

    @Override
    public String toString() {
        String _xblockexpression = null;
        {
            final MoreObjects.ToStringHelper _stringHelper = MoreObjects
                    .toStringHelper(this);
            final Procedure1<MoreObjects.ToStringHelper> _function = (
                    final MoreObjects.ToStringHelper it) -> {
                it.add("name", name);
                it.add("location", location);
                it.add("existingProject", existingProject);
                it.add("configType", configType);
                it.add("builder", builder);
                it.add("builderData", builderData);
                it.add("super", super.toString());
            };
            final MoreObjects.ToStringHelper helper = ObjectExtensions.<MoreObjects.ToStringHelper> operator_doubleArrow(
                    _stringHelper, _function);
            _xblockexpression = helper.toString();
        }
        return _xblockexpression;
    }

    public void loadFromFile() {
        final String _portableString = getLocation()
                .append(getConfigType().getConfigName()).toPortableString();
        final File f = new File(_portableString);
        final boolean _exists = f.exists();
        if (_exists) {
            final String _absolutePath = f.getAbsolutePath();
            final String _plus = "» LOAD " + _absolutePath;
            System.out.println(_plus);
            final String _portableString_1 = getLocation().toPortableString();
            final File _file = new File(_portableString_1);
            final IProjectConfigurator config = factory.getConfig(getConfigType(), _file);
            final ErlangProjectProperties props = config.getConfiguration();
            setOutputDir(props.getOutputDir());
            this.setSourceDirs(props.getSourceDirs());
            this.setIncludeDirs(props.getIncludeDirs());
            this.setTestDirs(props.getTestDirs());
        }
    }

    public String detectProjectConfig() {
        String _xblockexpression = null;
        {
            System.out.println("» DETECT builder config");
            String _xifexpression = null;
            if (location != null) {
                String _xblockexpression_1 = null;
                {
                    System.out.println("DETECT builder config");
                    final String _portableString = location.toPortableString();
                    final File directory = new File(_portableString);
                    String _xifexpression_1 = null;
                    if (directory.isDirectory() && directory.exists()) {
                        String _xblockexpression_2 = null;
                        {
                            final IProjectConfigurator persister = factory
                                    .getConfig(configType, directory);
                            System.out.println("PERSISTER " + persister);
                            String _xifexpression_2 = null;
                            if (persister != null) {
                                String _xblockexpression_3 = null;
                                {
                                    final ErlangProjectProperties props = persister
                                            .getConfiguration();
                                    _xblockexpression_3 = "detected PROPS: " + props;
                                    System.out.println(_xblockexpression_3);
                                }
                                _xifexpression_2 = _xblockexpression_3;
                            }
                            _xblockexpression_2 = _xifexpression_2;
                        }
                        _xifexpression_1 = _xblockexpression_2;
                    }
                    _xblockexpression_1 = _xifexpression_1;
                }
                _xifexpression = _xblockexpression_1;
            }
            _xblockexpression = _xifexpression;
        }
        return _xblockexpression;
    }

    public RuntimeInfo bestRuntime() {
        return RuntimeCore.getRuntimeInfoCatalog()
                .getRuntime(ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION, null);
    }

    public String getName() {
        return name;
    }

    public void setName(final String name) {
        this.name = name;
    }

    public IPath getLocation() {
        return location;
    }

    public void setLocation(final IPath location) {
        this.location = location;
    }

    public boolean isExistingProject() {
        return existingProject;
    }

    public void setExistingProject(final boolean existingProject) {
        this.existingProject = existingProject;
    }

    public BuilderTool getBuilder() {
        return builder;
    }

    public void setBuilder(final BuilderTool builder) {
        this.builder = builder;
    }

    public ProjectConfigType getConfigType() {
        return configType;
    }

    public void setConfigType(final ProjectConfigType configType) {
        this.configType = configType;
    }

    public Map<String, String> getBuilderData() {
        return builderData;
    }

    public void setBuilderData(final Map<String, String> builderData) {
        this.builderData = builderData;
    }

    public IProjectConfiguratorFactory getFactory() {
        return factory;
    }
}
