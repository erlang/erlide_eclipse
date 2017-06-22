package org.erlide.engine.model.erlang.configuration;

import static com.google.common.truth.Truth.assertThat;

import java.util.Collection;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IProjectConfigurator;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.engine.model.root.ProjectConfiguratorFactory;
import org.junit.Test;

public class EmakeProjectConfigurationTest extends AbstractProjectConfigurationTest {

    @Test
    public void configuratorExists() {
        project.setConfigType(ProjectConfigType.EMAKE);

        final IProjectConfigurator config = ProjectConfiguratorFactory.getDefault()
                .getConfig(project.getConfigType(), project);
        assertThat(config).isNotNull();

        // final ProjectConfigurator configurator = config.getConfigurator();
        // assertThat(configurator).isEqualTo(notNullValue()));
    }

    @Override
    @Test
    public void configCanBeParsed() throws CoreException {
        project.setConfigType(ProjectConfigType.EMAKE);
        final IProjectConfigurator config = ProjectConfiguratorFactory.getDefault()
                .getConfig(project.getConfigType(), project);

        final ErlangProjectProperties expected = ErlangProjectProperties.DEFAULT;
        final ErlangProjectProperties actual = config.getConfiguration();

        assertThat(actual).isEqualTo(expected);
    }

    @Test
    public void propertiesShouldFollowConfigFileChange() throws CoreException {
        project.setConfigType(ProjectConfigType.EMAKE);
        final String cfgFile = ProjectConfigType.EMAKE.getConfigName();

        final String config1 = "{'src/*',[debug_info,{i,\"myinclude\"}]}. "
                + "{'src2/*',[debug_info,{i,\"myinclude\"}]}.";
        setFileContent(cfgFile, config1);
        project.getWorkspaceProject().refreshLocal(IResource.DEPTH_ONE, null);

        final Collection<IPath> actualSources = project.getProperties().getSourceDirs();
        assertThat(actualSources).hasSize(2);
        assertThat(actualSources).contains(new Path("src2"));

        final Collection<IPath> actualIncludes = project.getProperties().getIncludeDirs();
        assertThat(actualIncludes).hasSize(1);
        assertThat(actualIncludes).contains(new Path("myinclude"));
    }

}
