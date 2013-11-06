package org.erlide.engine.model.erlang.configuration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;

import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.internal.model.root.ErlProject;
import org.erlide.engine.model.builder.BuilderConfig;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.builder.ErlangBuilder;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.ProjectConfigurator;
import org.junit.Test;

public class RebarProjectConfigurationTests extends AbstractProjectConfigurationTests {

    @Override
    @Test
    public void configCanBeParsed() throws CoreException {
        project.setBuilderTool(BuilderTool.REBAR);
        final ProjectConfigurator configurator = ErlangBuilder.getFactory()
                .getConfigurationPersister(project.getBuilderConfig()).getConfigurator();

        final ErlangProjectProperties expected = new ErlangProjectProperties();
        final ErlangProjectProperties actual = configurator.decodeConfig("");

        assertThat(actual, is(expected));
    }

    @Test
    public void propertiesShouldFollowConfigFileChange() throws CoreException {
        project.setBuilderTool(BuilderTool.REBAR);
        final String cfgFile = BuilderConfig.REBAR.getConfigName();
        final String config = getFileContent(cfgFile);

        final IPath expected = new Path("src2");
        final String config1 = config + "{erl_opts, [{i, \"myinclude\"}, "
                + "{src_dirs, [\"src\", \"" + expected.toPortableString() + "\"]}]}.";
        setFileContent(cfgFile, config1);

        ((ErlProject) project).loadProperties();
        final Collection<IPath> actual = project.getProperties().getSourceDirs();

        assertThat(actual, hasSize(2));
        assertThat(actual, contains(expected));
    }
}
