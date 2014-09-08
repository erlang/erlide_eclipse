package org.erlide.core.services.builder;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import java.io.StringBufferInputStream;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.jobs.IJobManager;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.core.internal.builder.BuildNotifier;
import org.erlide.core.internal.builder.ErlangBuilder;
import org.erlide.core.internal.builder.ErlangBuilder.BuildKind;
import org.erlide.core.internal.builder.ErlangBuilderFactory;
import org.erlide.core.internal.builder.ErlangNature;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.util.ErlideTestUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings("deprecation")
public class BuildersTest {

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

    @AfterClass
    public static void finish() throws CoreException {
        final IErlProject p2 = ErlideTestUtils.getExistingProject("p2");
        final IProject prj = p2.getResource().getProject();
        ErlangNature.setErlangProjectBuilder(prj, BuilderTool.INTERNAL);
    }

    @Test
    public void internalBuilderShouldWork() throws CoreException {
        testBuilder(BuilderTool.INTERNAL);
    }

    @Test
    public void makeBuilderShouldWork() throws CoreException {
        final IFolder folder = (IFolder) prj.findMember("src");
        final IFile app = folder.getFile("z.app.src");
        app.create(new StringBufferInputStream(
                "{application, builders,[{description, \"\"},{vsn, \"1\"},"
                        + "{registered, []},{applications, [kernel,stdlib]},"
                        + "{mod, { mod, []}},{env, []}]}."), true, null);
        try {
            testBuilder(BuilderTool.MAKE);
        } finally {
            app.delete(true, null);
        }
    }

    @Test
    public void emakeBuilderShouldWork() throws CoreException {
        testBuilder(BuilderTool.EMAKE);
    }

    @Test
    public void rebarBuilderShouldWork() throws CoreException {
        final IFolder folder = (IFolder) prj.findMember("src");
        final IFile app = folder.getFile("z.app.src");
        app.create(new StringBufferInputStream(
                "{application, builders,[{description, \"\"},{vsn, \"1\"},"
                        + "{registered, []},{applications, [kernel,stdlib]},"
                        + "{mod, { mod, []}},{env, []}]}."), true, null);
        try {
            testBuilder(BuilderTool.REBAR);
        } finally {
            app.delete(true, null);
        }
    }

    @Test(expected = AssertionError.class)
    public void rebarBuilderShouldNotWorkWithoutAppFile() throws CoreException {
        testBuilder(BuilderTool.REBAR);
    }

    private void testBuilder(final BuilderTool builderTool) throws CoreException {
        ErlangNature.setErlangProjectBuilder(prj, builderTool);
        final String targetBeamPath = "ebin/mod.beam";

        final IResource beam0 = prj.findMember(targetBeamPath);
        assertThat("beam existed before test", beam0, nullValue());

        final ErlangBuilder builder = ErlangBuilderFactory.get(builderTool);
        final BuildNotifier notifier = new BuildNotifier(null, prj);
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(prj);

        builder.build(BuildKind.FULL, erlProject, notifier);
        prj.refreshLocal(IResource.DEPTH_INFINITE, null);
        waitJobsToFinish(ResourcesPlugin.FAMILY_MANUAL_REFRESH);

        final IResource beam = prj.findMember(targetBeamPath);
        assertThat("beam was not created", beam, notNullValue());

        builder.clean(erlProject, notifier);
        prj.refreshLocal(IResource.DEPTH_INFINITE, null);
        waitJobsToFinish(ResourcesPlugin.FAMILY_MANUAL_REFRESH);

        final IResource beam2 = prj.findMember(targetBeamPath);
        assertThat("beam was not removed", beam2, nullValue());
    }

    private void waitJobsToFinish(final Object family) {
        final IJobManager jobMan = Job.getJobManager();
        final Job[] build = jobMan.find(family);
        if (build.length == 1) {
            try {
                build[0].join();
            } catch (final InterruptedException e) {
            }
        }
    }

}
