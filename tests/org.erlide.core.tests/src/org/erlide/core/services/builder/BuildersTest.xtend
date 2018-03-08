package org.erlide.core.services.builder

import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.jobs.IJobManager
import org.eclipse.core.runtime.jobs.Job
import org.erlide.core.builder.BuildNotifier
import org.erlide.core.builder.ErlangBuilder
import org.erlide.core.builder.ErlangBuilder.BuildKind
import org.erlide.core.builder.ErlangBuilderFactory
import org.erlide.core.builder.ErlangNature
import org.erlide.core.builder.ExternalBuilder
import org.erlide.engine.ErlangEngine
import org.erlide.engine.model.builder.BuilderTool
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.model.root.IErlProject
import org.erlide.engine.util.ErlideTestUtils
import org.erlide.runtime.runtimeinfo.RuntimeVersion
import org.junit.After
import org.junit.Before
import org.junit.Test

import static com.google.common.truth.Truth.assertWithMessage
import static org.erlide.core.builder.ExternalBuilder.*

@SuppressWarnings("all") class BuildersTest {
	IProject prj

	static String MOD = '''
		-module(mod).
		-export([f/0]).
		-include("hdr.hrl").
		f() ->    4+5,   ok.
	'''
	static String HDR = '''
		-define(HDR, hrd).
	'''
	static String EMAKEFILE = '''
		{'src/*', [{i, "include"}, {outdir, "ebin"}]}.
	'''
	static String MAKEFILE = '''
		ERLFLAGS= -pa $(CURDIR)/ebin
		REBAR=$(shell which rebar)
		ifeq ($(REBAR),)
		$(error "Rebar not available on this system")
		endif
		.PHONY: all
		all: compile
		compile: ; $(REBAR) skip_deps=true compile
		clean: ; rm $(CURDIR)/ebin/*
	'''

	String APP = '''
		{application, builders,
		    [{description, ""},{vsn, "1"},{registered, []},{applications, [kernel,stdlib]},{mod, { mod, []}},{env, []}]
		}.
	'''

	@Before def void setupProject() throws CoreException {
		ErlideTestUtils.initProjects()
		prj = ErlideTestUtils.createProject("builders")

		ErlideTestUtils.createFolder(prj, "src")
		ErlideTestUtils.createFile(prj, "src/mod.erl", MOD)

		ErlideTestUtils.createFolder(prj, "include")
		ErlideTestUtils.createFile(prj, "include/hdr.hrl", HDR)

		ErlideTestUtils.createFile(prj, "src/builders.app.src", APP)
		ErlideTestUtils.createFile(prj, "Emakefile", EMAKEFILE)
		ErlideTestUtils.createFile(prj, "Makefile", MAKEFILE)

		prj.refreshLocal(IResource.DEPTH_INFINITE, null)

		val IErlProject erlPrj = ErlideTestUtils.createErlProject(prj)
		val ErlangProjectProperties properties = erlPrj.getProperties()
		properties.setRequiredRuntimeVersion(new RuntimeVersion(18))
		erlPrj.setProperties(properties)

		ExternalBuilder.DEBUG = true;
	}

	@After def void restore() {
		prj?.delete(true, null)
		prj = null

		ExternalBuilder.DEBUG = false;
	}

	@Test def void internalBuilderShouldWork() throws CoreException {
		testBuilder(BuilderTool.INTERNAL)
	}

	@Test def void makeBuilderShouldWork() throws CoreException {
		testBuilder(BuilderTool.MAKE)
	}

	@Test def void emakeBuilderShouldWork() throws CoreException {
		testBuilder(BuilderTool.EMAKE)
	}

	@Test def void rebarBuilderShouldWork() throws CoreException {
		testBuilder(BuilderTool.REBAR)
	}

// TODO	@Test(expected=AssertionError) def void rebarBuilderShouldNotWorkWithoutAppFile() throws CoreException {
//		testBuilder(BuilderTool.REBAR)
//	}
	def private void testBuilder(BuilderTool builderTool) throws CoreException {
		assertWithMessage("project exists").that(prj).isNotNull()
		ErlangNature.setErlangProjectBuilder(prj, builderTool)

		val String targetBeamPath = "ebin/mod.beam"
		val IResource beam0 = prj.findMember(targetBeamPath)
		assertWithMessage("beam existed before test").that(beam0).isNull()

		val ErlangBuilder builder = ErlangBuilderFactory.get(builderTool)
		val IErlProject erlProject = ErlangEngine.getInstance().getModel().getErlangProject(prj)
		val BuildNotifier notifier = new BuildNotifier(null, prj)
		builder.build(BuildKind.FULL, erlProject, notifier)
		prj.refreshLocal(IResource.DEPTH_INFINITE, null)
		waitJobsToFinish(ResourcesPlugin.FAMILY_MANUAL_REFRESH)
		val IResource beam = prj.findMember(targetBeamPath)
		assertWithMessage("beam was not created").that(beam).isNotNull()

		builder.clean(erlProject, notifier)
		prj.refreshLocal(IResource.DEPTH_INFINITE, null)
		waitJobsToFinish(ResourcesPlugin.FAMILY_MANUAL_REFRESH)
		val IResource beam2 = prj.findMember(targetBeamPath)
		assertWithMessage("beam was not removed").that(beam2).isNull()
	}

	def private void waitJobsToFinish(Object family) {
		val IJobManager jobMan = Job.getJobManager()
		val Job[] build = jobMan.find(family)
		if (build.length === 1) {
			try {
				build.get(0).join()
			} catch (InterruptedException e) {
				Thread.currentThread.interrupt
			}

		}
	}
}
