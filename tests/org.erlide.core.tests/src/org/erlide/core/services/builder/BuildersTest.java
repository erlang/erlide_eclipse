package org.erlide.core.services.builder;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.jobs.IJobManager;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.erlide.core.builder.BuildNotifier;
import org.erlide.core.builder.ErlangBuilder;
import org.erlide.core.builder.ErlangBuilderFactory;
import org.erlide.core.builder.ErlangNature;
import org.erlide.core.builder.ExternalBuilder;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.util.ErlideTestUtils;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.google.common.truth.Truth;

@SuppressWarnings("all")
public class BuildersTest {
    private IProject prj;

    private static String MOD = new Function0<String>() {
        @Override
        public String apply() {
            final StringConcatenation _builder = new StringConcatenation();
            _builder.append("-module(mod).");
            _builder.newLine();
            _builder.append("-export([f/0]).");
            _builder.newLine();
            _builder.append("-include(\"hdr.hrl\").");
            _builder.newLine();
            _builder.append("f() ->    4+5,   ok.");
            _builder.newLine();
            return _builder.toString();
        }
    }.apply();

    private static String HDR = new Function0<String>() {
        @Override
        public String apply() {
            final StringConcatenation _builder = new StringConcatenation();
            _builder.append("-define(HDR, hrd).");
            _builder.newLine();
            return _builder.toString();
        }
    }.apply();

    private static String EMAKEFILE = new Function0<String>() {
        @Override
        public String apply() {
            final StringConcatenation _builder = new StringConcatenation();
            _builder.append("{\'src/*\', [{i, \"include\"}, {outdir, \"ebin\"}]}.");
            _builder.newLine();
            return _builder.toString();
        }
    }.apply();

    private static String MAKEFILE = new Function0<String>() {
        @Override
        public String apply() {
            final StringConcatenation _builder = new StringConcatenation();
            _builder.append("ERLFLAGS= -pa $(CURDIR)/ebin");
            _builder.newLine();
            _builder.append("REBAR=$(shell which rebar)");
            _builder.newLine();
            _builder.append("ifeq ($(REBAR),)");
            _builder.newLine();
            _builder.append("$(error \"Rebar not available on this system\")");
            _builder.newLine();
            _builder.append("endif");
            _builder.newLine();
            _builder.append(".PHONY: all");
            _builder.newLine();
            _builder.append("all: compile");
            _builder.newLine();
            _builder.append("compile: ; $(REBAR) skip_deps=true compile");
            _builder.newLine();
            _builder.append("clean: ; rm $(CURDIR)/ebin/*");
            _builder.newLine();
            return _builder.toString();
        }
    }.apply();

    private final String APP = new Function0<String>() {
        @Override
        public String apply() {
            final StringConcatenation _builder = new StringConcatenation();
            _builder.append("{application, builders,");
            _builder.newLine();
            _builder.append("    ");
            _builder.append(
                    "[{description, \"\"},{vsn, \"1\"},{registered, []},{applications, [kernel,stdlib]},{mod, { mod, []}},{env, []}]");
            _builder.newLine();
            _builder.append("}.");
            _builder.newLine();
            return _builder.toString();
        }
    }.apply();

    @Before
    public void setupProject() throws CoreException {
        ErlideTestUtils.initProjects();
        prj = ErlideTestUtils.createProject("builders");
        ErlideTestUtils.createFolder(prj, "src");
        ErlideTestUtils.createFile(prj, "src/mod.erl", BuildersTest.MOD);
        ErlideTestUtils.createFolder(prj, "include");
        ErlideTestUtils.createFile(prj, "include/hdr.hrl", BuildersTest.HDR);
        ErlideTestUtils.createFile(prj, "src/builders.app.src", APP);
        ErlideTestUtils.createFile(prj, "Emakefile", BuildersTest.EMAKEFILE);
        ErlideTestUtils.createFile(prj, "Makefile", BuildersTest.MAKEFILE);
        prj.refreshLocal(IResource.DEPTH_INFINITE, null);
        final IErlProject erlPrj = ErlideTestUtils.createErlProject(prj);
        final ErlangProjectProperties properties = erlPrj.getProperties();
        final RuntimeVersion _runtimeVersion = new RuntimeVersion(25);
        properties.setRequiredRuntimeVersion(_runtimeVersion);
        erlPrj.setProperties(properties);
        ExternalBuilder.DEBUG = true;
    }

    @After
    public void restore() {
        try {
            if (prj != null) {
                prj.delete(true, null);
            }
            prj = null;
            ExternalBuilder.DEBUG = false;
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    @Test
    public void internalBuilderShouldWork() throws CoreException {
        testBuilder(BuilderTool.INTERNAL);
    }

    @Test
    @Ignore
    public void makeBuilderShouldWork() throws CoreException {
        testBuilder(BuilderTool.MAKE);
    }

    @Test
    public void emakeBuilderShouldWork() throws CoreException {
        testBuilder(BuilderTool.EMAKE);
    }

    @Test
    @Ignore
    public void rebarBuilderShouldWork() throws CoreException {
        testBuilder(BuilderTool.REBAR);
    }

    private void testBuilder(final BuilderTool builderTool) throws CoreException {
        Truth.assertWithMessage("project exists").that(prj).isNotNull();
        ErlangNature.setErlangProjectBuilder(prj, builderTool);
        final String targetBeamPath = "ebin/mod.beam";
        final IResource beam0 = prj.findMember(targetBeamPath);
        Truth.assertWithMessage("beam existed before test").that(beam0).isNull();
        final ErlangBuilder builder = ErlangBuilderFactory.get(builderTool);
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(prj);
        final BuildNotifier notifier = new BuildNotifier(null, prj);
        builder.build(ErlangBuilder.BuildKind.FULL, erlProject, notifier);
        prj.refreshLocal(IResource.DEPTH_INFINITE, null);
        waitJobsToFinish(ResourcesPlugin.FAMILY_MANUAL_REFRESH);
        final IResource beam = prj.findMember(targetBeamPath);
        Truth.assertWithMessage("beam was not created").that(beam).isNotNull();
        builder.clean(erlProject, notifier);
        prj.refreshLocal(IResource.DEPTH_INFINITE, null);
        waitJobsToFinish(ResourcesPlugin.FAMILY_MANUAL_REFRESH);
        final IResource beam2 = prj.findMember(targetBeamPath);
        Truth.assertWithMessage("beam was not removed").that(beam2).isNull();
    }

    private void waitJobsToFinish(final Object family) {
        final IJobManager jobMan = Job.getJobManager();
        final Job[] build = jobMan.find(family);
        final int _length = build.length;
        final boolean _tripleEquals = _length == 1;
        if (_tripleEquals) {
            try {
                build[0].join();
            } catch (final Throwable _t) {
                if (_t instanceof InterruptedException) {
                    Thread.currentThread().interrupt();
                } else {
                    throw Exceptions.sneakyThrow(_t);
                }
            }
        }
    }
}
