package org.erlide.core.services.builder;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.jobs.IJobManager;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.core.internal.builder.ErlangNature;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class BuildersTest {

    private IProject prj;

    @Before
    public void initialClean() throws CoreException {
        final IErlProject p2 = ErlideTestUtils.getExistingProject("p2");
        prj = p2.getResource().getProject();

        final IResource beam = prj.findMember("ebin/m21.beam");
        if (beam != null) {
            beam.delete(true, null);
        }
        final IResource ebin = prj.findMember("ebin");
        if (ebin != null) {
            ebin.refreshLocal(IResource.DEPTH_ONE, null);
        }
    }

    @After
    public void restore() {
        prj = null;
    }

    @AfterClass
    public static void finish() throws CoreException {
        final IErlProject p2 = ErlideTestUtils.getExistingProject("p2");
        final IProject prj = p2.getResource().getProject();
        ErlangNature.setErlangProjectBuilder(prj, "internal");
    }

    @Test
    public void internalBuilderShouldWork() throws CoreException {
        testBuilder("internal");
    }

    @Ignore
    @Test
    public void makeBuilderShouldWork() throws CoreException {
        // TODO how to make sure make is installed on test server?
        testBuilder("make");
    }

    @Test
    public void emakeBuilderShouldWork() throws CoreException {
        testBuilder("emake");
    }

    @Ignore
    @Test
    public void rebarBuilderShouldWork() throws CoreException {
        // TODO how to make sure rebar is installed on test server?
        testBuilder("rebar");
    }

    private void testBuilder(final String builder) throws CoreException {
        ErlangNature.setErlangProjectBuilder(prj, builder);
        final String builderId = ErlangNature.BUILDER_ID_MAP.get(builder);
        final String targetBeamPath = "ebin/m21.beam";

        final IResource beam0 = prj.findMember(targetBeamPath);
        assertThat("beam must not exist before test", beam0, nullValue());

        prj.build(IncrementalProjectBuilder.FULL_BUILD, builderId, null, null);
        // waitBuildToFinish();
        prj.refreshLocal(IResource.DEPTH_INFINITE, null);
        // waitRefreshToFinish();

        final IResource beam = prj.findMember(targetBeamPath);
        assertThat("beam was not created", beam, notNullValue());

        prj.build(IncrementalProjectBuilder.CLEAN_BUILD, builderId, null, null);
        // waitBuildToFinish();
        prj.refreshLocal(IResource.DEPTH_INFINITE, null);
        // waitRefreshToFinish();

        final IResource beam2 = prj.findMember(targetBeamPath);
        assertThat("beam was not removed", beam2, nullValue());
    }

    private void waitRefreshToFinish() {
        final IJobManager jobMan = Job.getJobManager();
        final Job[] build = jobMan.find(ResourcesPlugin.FAMILY_MANUAL_REFRESH);
        if (build.length == 1) {
            try {
                build[0].join();
            } catch (final InterruptedException e) {
            }
        }
    }

    private void waitBuildToFinish() {
        final IJobManager jobMan = Job.getJobManager();
        final Job[] build = jobMan.find(ResourcesPlugin.FAMILY_MANUAL_BUILD);
        if (build.length == 1) {
            try {
                build[0].join();
            } catch (final InterruptedException e) {
            }
        }
    }

}
