package org.erlide.engine.util

import java.util.Collection
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.Path
import org.erlide.engine.model.ErlModelException
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.model.root.IErlModule
import org.erlide.engine.model.root.IErlProject
import org.junit.Before
import org.junit.Test

import static com.google.common.truth.Truth.assertThat

class ModuleLocatorTest {
    IErlProject p1
    IErlProject p2
    static package String M11 = '''
        -module(m11).

        -export([f/0, g/0]).

        -include("h11.hrl").
        -include("h12.hrl").
        -include("h21.hrl").

        f() ->
            %% in project
            m12:f(),
            ?H11,
            ?H12,

            %% in workspace
            m21:f(),
            ?H21,
            ?H22, % not to be found!

            ok.

        g() ->
            %% in externals
            m31:f(),
            ?H31,
            ?H32, % not to be found!

            %% not found
            m41:f(),
            ?H41,

            ok.
    '''
    static package String H11 = '''
        -define(H11, h11).
    '''
    static package String H12 = '''
        -define(H12, h12).
    '''
    static package String M21 = '''
        -module(m21).
        -export([f/0]).
        f() ->
            ok.
    '''
    static package String H21 = '''
        -define(H21, h21).
    '''
    static package String H22 = '''
        -define(H22, h22).
    '''

    @Before def void setup() throws CoreException {
        val IProject pp1 = ErlideTestUtils.createProject("p1")
        ErlideTestUtils.createFolder(pp1, "src")
        ErlideTestUtils.createFile(pp1, "src/m11.erl", M11)
        ErlideTestUtils.createFile(pp1, "src/h12.erl", H12)
        ErlideTestUtils.createFolder(pp1, "include")
        ErlideTestUtils.createFile(pp1, "include/h11.erl", H11)

        val IProject pp2 = ErlideTestUtils.createProject("p2")
        ErlideTestUtils.createFolder(pp2, "src")
        ErlideTestUtils.createFile(pp2, "src/m21.erl", M21)
        ErlideTestUtils.createFile(pp2, "src/h22.erl", H22)
        ErlideTestUtils.createFolder(pp2, "include")
        ErlideTestUtils.createFile(pp2, "include/h21.erl", H21)

        p1 = ErlideTestUtils.createErlProject(pp1)
        p2 = ErlideTestUtils.createErlProject(pp2)
        if (p1 !== null) {
            p1.makeConsistent(null)
        }
        if (p1 !== null) {
            p2.makeConsistent(null)
        }
    }

    @Test def void demoProjectsShouldBeInWorkspace() {
        assertThat(p1).isNotNull()
        assertThat(p2).isNotNull()
    }

    @Test def void demoProjectsShouldBeConfiguredProperly() {
        checkProjectDirectories(p1, #[new Path("src")], #[new Path("include")])
        //TODO , new Path("T_ROOT/p3/include")])
        checkProjectDirectories(p2, #[new Path("src")], #[new Path("include")])
    }

    def private void checkProjectDirectories(IErlProject project, Object[] expectedSources, Object[] expectedIncludes) {
        val ErlangProjectProperties properties = project.getProperties()
        assertThat(properties.getSourceDirs().toArray()).isEqualTo(expectedSources)
        assertThat(properties.getIncludeDirs().toArray()).isEqualTo(expectedIncludes)
        // TODO assertThat(properties.getExternalModules()).isEqualTo("../external_modules")
        // TODO assertThat(properties.getExternalIncludes()).isEqualTo("../external_includes")
    }

    // @Test
    def void demoProjectsShouldHaveRightExternalModules() throws ErlModelException {
        checkExternalModules(p1, #["m11.erl"])
        checkExternalModules(p2, #["m11.erl"])
        checkExternalIncludes(p1, #[])
        checkExternalIncludes(p2, #[])
    }

    def private void checkExternalModules(IErlProject project, String[] extmods) throws ErlModelException {
        checkModuleNamesInList(extmods, project.getExternalModules())
    }

    def private void checkExternalIncludes(IErlProject project,
        String[] extincs) throws ErlModelException {
        checkModuleNamesInList(extincs, project.getExternalIncludes())
    }

    def private void checkModuleNamesInList(String[] mods, Collection<IErlModule> list) {
        for (String name : mods) {
            assertThat(hasModWithName(list, name)).isTrue()
        }
    }

    def private boolean hasModWithName(Collection<IErlModule> list, String name) {
        for (IErlModule m : list) {
            if (m.getName().equals(name)) {
                return true
            }
        }
        return false
    }
}
