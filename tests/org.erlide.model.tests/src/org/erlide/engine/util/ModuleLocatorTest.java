package org.erlide.engine.util;

import java.util.Collection;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.junit.Before;
import org.junit.Test;

import com.google.common.truth.Truth;

@SuppressWarnings("all")
public class ModuleLocatorTest {
    private IErlProject p1;

    private IErlProject p2;

    static String M11 = new Function0<String>() {
        @Override
        public String apply() {
            final StringConcatenation _builder = new StringConcatenation();
            _builder.append("-module(m11).");
            _builder.newLine();
            _builder.newLine();
            _builder.append("-export([f/0, g/0]).");
            _builder.newLine();
            _builder.newLine();
            _builder.append("-include(\"h11.hrl\").");
            _builder.newLine();
            _builder.append("-include(\"h12.hrl\").");
            _builder.newLine();
            _builder.append("-include(\"h21.hrl\").");
            _builder.newLine();
            _builder.newLine();
            _builder.append("f() ->");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("%% in project");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("m12:f(),");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("?H11,");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("?H12,");
            _builder.newLine();
            _builder.newLine();
            _builder.append("    ");
            _builder.append("%% in workspace");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("m21:f(),");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("?H21,");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("?H22, % not to be found!");
            _builder.newLine();
            _builder.newLine();
            _builder.append("    ");
            _builder.append("ok.");
            _builder.newLine();
            _builder.newLine();
            _builder.append("g() ->");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("%% in externals");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("m31:f(),");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("?H31,");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("?H32, % not to be found!");
            _builder.newLine();
            _builder.newLine();
            _builder.append("    ");
            _builder.append("%% not found");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("m41:f(),");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("?H41,");
            _builder.newLine();
            _builder.newLine();
            _builder.append("    ");
            _builder.append("ok.");
            _builder.newLine();
            return _builder.toString();
        }
    }.apply();

    static String H11 = new Function0<String>() {
        @Override
        public String apply() {
            final StringConcatenation _builder = new StringConcatenation();
            _builder.append("-define(H11, h11).");
            _builder.newLine();
            return _builder.toString();
        }
    }.apply();

    static String H12 = new Function0<String>() {
        @Override
        public String apply() {
            final StringConcatenation _builder = new StringConcatenation();
            _builder.append("-define(H12, h12).");
            _builder.newLine();
            return _builder.toString();
        }
    }.apply();

    static String M21 = new Function0<String>() {
        @Override
        public String apply() {
            final StringConcatenation _builder = new StringConcatenation();
            _builder.append("-module(m21).");
            _builder.newLine();
            _builder.append("-export([f/0]).");
            _builder.newLine();
            _builder.append("f() ->");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("ok.");
            _builder.newLine();
            return _builder.toString();
        }
    }.apply();

    static String H21 = new Function0<String>() {
        @Override
        public String apply() {
            final StringConcatenation _builder = new StringConcatenation();
            _builder.append("-define(H21, h21).");
            _builder.newLine();
            return _builder.toString();
        }
    }.apply();

    static String H22 = new Function0<String>() {
        @Override
        public String apply() {
            final StringConcatenation _builder = new StringConcatenation();
            _builder.append("-define(H22, h22).");
            _builder.newLine();
            return _builder.toString();
        }
    }.apply();

    @Before
    public void setup() throws CoreException {
        final IProject pp1 = ErlideTestUtils.createProject("p1");
        ErlideTestUtils.createFolder(pp1, "src");
        ErlideTestUtils.createFile(pp1, "src/m11.erl", ModuleLocatorTest.M11);
        ErlideTestUtils.createFile(pp1, "src/h12.erl", ModuleLocatorTest.H12);
        ErlideTestUtils.createFolder(pp1, "include");
        ErlideTestUtils.createFile(pp1, "include/h11.erl", ModuleLocatorTest.H11);
        final IProject pp2 = ErlideTestUtils.createProject("p2");
        ErlideTestUtils.createFolder(pp2, "src");
        ErlideTestUtils.createFile(pp2, "src/m21.erl", ModuleLocatorTest.M21);
        ErlideTestUtils.createFile(pp2, "src/h22.erl", ModuleLocatorTest.H22);
        ErlideTestUtils.createFolder(pp2, "include");
        ErlideTestUtils.createFile(pp2, "include/h21.erl", ModuleLocatorTest.H21);
        p1 = ErlideTestUtils.createErlProject(pp1);
        p2 = ErlideTestUtils.createErlProject(pp2);
        if (p1 != null) {
            p1.makeConsistent(null);
        }
        if (p1 != null) {
            p2.makeConsistent(null);
        }
    }

    @Test
    public void demoProjectsShouldBeInWorkspace() {
        Truth.assertThat(p1).isNotNull();
        Truth.assertThat(p2).isNotNull();
    }

    @Test
    public void demoProjectsShouldBeConfiguredProperly() {
        final Path _path = new Path("src");
        final Path _path_1 = new Path("include");
        checkProjectDirectories(p1, new Object[] { _path }, new Object[] { _path_1 });
        final Path _path_2 = new Path("src");
        final Path _path_3 = new Path("include");
        checkProjectDirectories(p2, new Object[] { _path_2 }, new Object[] { _path_3 });
    }

    private void checkProjectDirectories(final IErlProject project,
            final Object[] expectedSources, final Object[] expectedIncludes) {
        final ErlangProjectProperties properties = project.getProperties();
        Truth.<Object> assertThat(properties.getSourceDirs().toArray())
                .isEqualTo(expectedSources);
        Truth.<Object> assertThat(properties.getIncludeDirs().toArray())
                .isEqualTo(expectedIncludes);
    }

    public void demoProjectsShouldHaveRightExternalModules() throws ErlModelException {
        checkExternalModules(p1, new String[] { "m11.erl" });
        checkExternalModules(p2, new String[] { "m11.erl" });
        checkExternalIncludes(p1, new String[] {});
        checkExternalIncludes(p2, new String[] {});
    }

    private void checkExternalModules(final IErlProject project, final String[] extmods)
            throws ErlModelException {
        checkModuleNamesInList(extmods, project.getExternalModules());
    }

    private void checkExternalIncludes(final IErlProject project, final String[] extincs)
            throws ErlModelException {
        checkModuleNamesInList(extincs, project.getExternalIncludes());
    }

    private void checkModuleNamesInList(final String[] mods,
            final Collection<IErlModule> list) {
        for (final String name : mods) {
            Truth.assertThat(Boolean.valueOf(hasModWithName(list, name))).isTrue();
        }
    }

    private boolean hasModWithName(final Collection<IErlModule> list, final String name) {
        for (final IErlModule m : list) {
            final boolean _equals = m.getName().equals(name);
            if (_equals) {
                return true;
            }
        }
        return false;
    }
}
