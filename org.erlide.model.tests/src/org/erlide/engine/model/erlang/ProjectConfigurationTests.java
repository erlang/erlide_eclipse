package org.erlide.engine.model.erlang;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.internal.model.root.ErlProject;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.builder.BuilderConfig;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.util.ErlideTestUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ProjectConfigurationTests {

    private IProject prj;

    @Before
    public void initialClean() throws CoreException {
        final IErlProject p2 = ErlideTestUtils.getExistingProject("builders");
        prj = p2.getResource().getProject();

        final IResource ebin = prj.findMember("ebin");
        if (ebin != null) {
            ebin.delete(true, null);
        } else {
            prj.getFolder("ebin").create(true, true, null);
        }
    }

    @After
    public void restore() {
        prj = null;
    }

    @Test
    public void defaultProjectBuilderShouldBeInternal() throws ErlModelException {
        final IErlangEngine engine = ErlangEngine.getInstance();
        final IErlProject project = engine.getModel().newProject("zux", "/tmp/zux");

        final ErlProject p = (ErlProject) project;
        p.loadCoreProperties();

        assertThat(project.getBuilderTool(), is(BuilderTool.INTERNAL));
        assertThat(project.getBuilderConfig(), is(BuilderConfig.INTERNAL));
    }
}
