package org.erlide.core.services.builder;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.erlide.core.ErlangCore;
import org.erlide.core.internal.builder.ErlangNature;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class InternalBuilderTest {

    private IProject prj;

    @Before
    public void initialClean() throws CoreException {
        final IErlProject p2 = ErlideTestUtils.getExistingProject("p2");
        prj = p2.getResource().getProject();

        final IResource beam = prj.findMember("ebin/m21.beam");
        if (beam != null) {
            beam.delete(true, null);
        }
        ErlangNature.setProjectBuilder(prj, "internal");
    }

    @After
    public void restore() {
        prj = null;
    }

    @Test
    public void projectShouldBuild() throws CoreException {
        prj.build(IncrementalProjectBuilder.FULL_BUILD, ErlangCore.BUILDER_ID,
                null, new NullProgressMonitor());
        prj.refreshLocal(IResource.DEPTH_INFINITE, null);

        final IResource beam = prj.findMember("ebin/m21.beam");
        assertThat(beam, notNullValue());
    }

    @Test
    public void projectShouldClean() throws CoreException {
        prj.build(IncrementalProjectBuilder.FULL_BUILD, ErlangCore.BUILDER_ID,
                null, new NullProgressMonitor());
        prj.build(IncrementalProjectBuilder.CLEAN_BUILD, ErlangCore.BUILDER_ID,
                null, new NullProgressMonitor());
        prj.refreshLocal(IResource.DEPTH_INFINITE, null);

        final IResource beam = prj.findMember("ebin/m21.beam");
        assertThat(beam, nullValue());
    }

}
