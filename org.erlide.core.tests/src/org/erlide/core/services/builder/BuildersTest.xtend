package org.erlide.core.services.builder

import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.jobs.IJobManager
import org.eclipse.core.runtime.jobs.Job
import org.erlide.core.builder.BuildNotifier
import org.erlide.core.internal.builder.ErlangBuilder
import org.erlide.core.internal.builder.ErlangBuilder.BuildKind
import org.erlide.core.internal.builder.ErlangBuilderFactory
import org.erlide.core.internal.builder.ErlangNature
import org.erlide.engine.ErlangEngine
import org.erlide.engine.model.builder.BuilderTool
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.model.root.IErlProject
import org.erlide.engine.util.ErlideTestUtils
import org.erlide.runtime.runtimeinfo.RuntimeVersion
import org.junit.After
import org.junit.Before
import org.junit.Test

import static com.google.common.truth.Truth.assertThat
import static com.google.common.truth.Truth.assertWithMessage

@SuppressWarnings("deprecation") class BuildersTest {
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
        {'src/*', [{i, \"include\"}, {outdir, \"ebin\"}]}.
    '''
    static String MAKEFILE = '''
        # Copyright 2012 Erlware, LLC. All Rights Reserved.
        #
        # This file is provided to you under the Apache License,
        # Version 2.0 (the "License"); you may not use this file
        # except in compliance with the License.  You may obtain
        # a copy of the License at
        #
        #   http://www.apache.org/licenses/LICENSE-2.0
        #
        # Unless required by applicable law or agreed to in writing,
        # software distributed under the License is distributed on an
        # "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
        # KIND, either express or implied.  See the License for the
        # specific language governing permissions and limitations
        # under the License.
        #

        ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

        DEPS_PLT=$(CURDIR)/.deps_plt
        DEPS=erts kernel stdlib

        # =============================================================================
        # Verify that the programs we need to run are installed on this system
        # =============================================================================
        ERL = $(shell which erl)

        ifeq ($(ERL),)
        $(error "Erlang not available on this system")
        endif

        REBAR=$(shell which rebar)

        ifeq ($(REBAR),)
        $(error "Rebar not available on this system")
        endif

        .PHONY: all compile doc clean test dialyzer typer shell distclean pdf \
            update-deps clean-common-test-data rebuild

        all: deps compile dialyzer test

        # =============================================================================
        # Rules to build the system
        # =============================================================================

        deps:
                $(REBAR) get-deps
                $(REBAR) compile

        update-deps:
                $(REBAR) update-deps
                $(REBAR) compile

        compile:
                $(REBAR) skip_deps=true compile

        doc:
                $(REBAR) skip_deps=true doc

        eunit: compile clean-common-test-data
                $(REBAR) skip_deps=true eunit

        test: compile eunit

        $(DEPS_PLT):
                @echo Building local plt at $(DEPS_PLT)
                @echo
                dialyzer --output_plt $(DEPS_PLT) --build_plt \n             --apps $(DEPS) -r deps

        dialyzer: $(DEPS_PLT)
                dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin

        typer:
                typer --plt $(DEPS_PLT) -r ./src

        shell: deps compile
        # You often want *rebuilt* rebar tests to be available to the
        # shell you have to call eunit (to get the tests
        # rebuilt). However, eunit runs the tests, which probably
        # fails (thats probably why You want them in the shell). This
        # runs eunit but tells make to ignore the result.
                - @$(REBAR) skip_deps=true eunit
                @$(ERL) $(ERLFLAGS)

        pdf:
                pandoc README.md -o README.pdf

        clean:
                - rm -rf $(CURDIR)/test/*.beam
                - rm -rf $(CURDIR)/logs
                - rm -rf $(CURDIR)/ebin
                $(REBAR) skip_deps=true clean

        distclean: clean
                - rm -rf $(DEPS_PLT)
                - rm -rvf $(CURDIR)/deps

        rebuild: distclean deps compile escript dialyzer test
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
        ErlideTestUtils.createFile(prj, "include/hdr.erl", HDR)

        ErlideTestUtils.createFile(prj, "src/builders.app.src", APP)
        ErlideTestUtils.createFile(prj, "Emakefile", EMAKEFILE)
        ErlideTestUtils.createFile(prj, "Makefile", MAKEFILE)

        prj.refreshLocal(IResource.DEPTH_INFINITE, null)

        val IErlProject erlPrj = ErlideTestUtils.createErlProject(prj)
        val ErlangProjectProperties properties = erlPrj.getProperties()
        properties.setRequiredRuntimeVersion(new RuntimeVersion(18))
        erlPrj.setProperties(properties)
    }

    @After def void restore() {
        prj?.delete(true, null)
        prj = null
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

    @Test(expected=AssertionError) def void rebarBuilderShouldNotWorkWithoutAppFile() throws CoreException {
        testBuilder(BuilderTool.REBAR)
    }

    def private void testBuilder(BuilderTool builderTool) throws CoreException {
        assertThat(prj).isNotNull()
        ErlangNature.setErlangProjectBuilder(prj, builderTool)

        val String targetBeamPath = "ebin/mod.beam"
        val IResource beam0 = prj.findMember(targetBeamPath)
        assertWithMessage("beam existed before test").that(beam0).isNull()

        val ErlangBuilder builder = ErlangBuilderFactory.get(builderTool)
        val BuildNotifier notifier = new BuildNotifier(null, prj)
        val IErlProject erlProject = ErlangEngine.getInstance().getModel().getErlangProject(prj)
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
