package org.erlide.engine.model.erlang.configuration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.core.IsNull.notNullValue;

import java.util.Collection;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.internal.model.root.ProjectConfiguratorFactory;
import org.erlide.engine.model.erlang.ErlangProjectPropertiesMatcher;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.engine.model.root.ProjectConfigurator;
import org.junit.Test;

public class EmakeProjectConfigurationTests extends AbstractProjectConfigurationTests {

    @Test
    public void configuratorExists() {
        project.setBuilderConfigType(ProjectConfigType.EMAKE);

        final ProjectConfigurator config = ProjectConfiguratorFactory.getDefault()
                .getConfig(project.getBuilderConfigType(), project);
        assertThat(config, is(notNullValue()));

        // final ProjectConfigurator configurator = config.getConfigurator();
        // assertThat(configurator, is(notNullValue()));
    }

    @Override
    @Test
    public void configCanBeParsed() throws CoreException {
        project.setBuilderConfigType(ProjectConfigType.EMAKE);
        final ProjectConfigurator config = ProjectConfiguratorFactory.getDefault()
                .getConfig(project.getBuilderConfigType(), project);

        final ErlangProjectProperties expected = ErlangProjectProperties.DEFAULT;
        final ErlangProjectProperties actual = config.getConfiguration();

        assertThat(actual, is(ErlangProjectPropertiesMatcher.sameAs(expected)));
    }

    @Test
    public void propertiesShouldFollowConfigFileChange() throws CoreException {
        project.setBuilderConfigType(ProjectConfigType.EMAKE);
        final String cfgFile = ProjectConfigType.EMAKE.getConfigName();

        final String config1 = "{'src/*',[debug_info,{i,\"myinclude\"}]}. "
                + "{'src2/*',[debug_info,{i,\"myinclude\"}]}.";
        setFileContent(cfgFile, config1);
        project.getWorkspaceProject().refreshLocal(IResource.DEPTH_ONE, null);

        final ErlangProjectProperties p2 = project.getProperties();

        final Collection<IPath> actualSources = p2.getSourceDirs();
        assertThat(actualSources, hasSize(2));
        assertThat(actualSources, hasItem(new Path("src2")));

        final Collection<IPath> actualIncludes = p2.getIncludeDirs();
        assertThat(actualIncludes, hasSize(1));
        assertThat(actualIncludes, hasItem(new Path("myinclude")));
    }

}
