package org.erlide.core.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.backend.util.StringUtils;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlElement.Kind;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlRecordDef;
import org.erlide.core.erlang.IErlTypespec;
import org.erlide.core.erlang.IOldErlangProjectProperties;
import org.erlide.core.erlang.util.BackendUtils;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.erlang.util.ModelUtils;
import org.erlide.core.text.ErlangToolkit;
import org.erlide.jinterface.backend.Backend;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideOpen;
import erlang.OpenResult;

public class ModelUtilsTests {

	private static IErlProject projects[] = null;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		ErlideTestUtils.initProjects();
		// We set up projects here, it's quite costly
		final String name1 = "testproject1";
		final IErlProject erlProject1 = ErlideTestUtils.createProject(
				ErlideTestUtils.getTmpPath(name1), name1);
		final String name2 = "testproject2";
		final IErlProject erlProject2 = ErlideTestUtils.createProject(
				ErlideTestUtils.getTmpPath(name2), name2);
		projects = new IErlProject[] { erlProject1, erlProject2 };
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		ErlideTestUtils.deleteProjects();
	}

	@Before
	public void setUp() throws Exception {
		ErlideTestUtils.initModules();
	}

	@After
	public void tearDown() throws Exception {
		ErlideTestUtils.deleteModules();
	}

	@Test
	public void getImportsAsListTest() throws Exception {
		// given
		// an Erlang module with imports
		final IErlModule moduleA = ErlideTestUtils.createModule(projects[0],
				"ax.erl",
				"-module(ax).\n-import(lists, [reverse/1, foldl/3].\n");
		moduleA.open(null);
		// when
		// fetching imports as list of OtpErlangTuple
		final Collection<IErlElement> children = moduleA.getChildren();
		final Collection<IErlImport> imports2 = moduleA.getImports();
		final List<OtpErlangObject> imports = ModelUtils
				.getImportsAsList(moduleA);
		// then
		// they should be returned
		if (children.size() != 2) {
			System.out.println(moduleA);
			System.out.println(children);
		}
		assertEquals(2, children.size());
		assertEquals(1, imports2.size());
		assertEquals(1, imports.size());
		final OtpErlangAtom listAtom = new OtpErlangAtom("lists");
		assertEquals(
				new OtpErlangTuple(new OtpErlangObject[] {
						listAtom,
						new OtpErlangList(new OtpErlangObject[] {
								makeTuple2("reverse", 1),
								makeTuple2("foldl", 3) }) }), imports.get(0));
	}

	@Test
	public void findExternalTypeTest() throws Exception {
		// given
		// an Erlang module with typedef
		final IErlModule moduleB = ErlideTestUtils
				.createModule(
						projects[0],
						"bx.erl",
						"-module(bx).\n-type concat_thing() :: atom() | integer() | float() | string().\n");
		// final IErlModule moduleC =
		// ErlideTestUtils.createErlModule(projects[1],
		// "c.erl", "-module(c).\n-type cc() :: b:concat_thing().\n");
		moduleB.open(null);
		// moduleC.open(null);
		// when
		// looking for it
		// within project
		final IErlElement element1 = ModelUtils.findExternalType(moduleB, "bx",
				"concat_thing", moduleB.getResource().getLocation()
						.toPortableString(), projects[0], false);
		// in other project but path given
		final IErlElement element2 = ModelUtils.findExternalType(moduleB, "bx",
				"concat_thing", moduleB.getResource().getLocation()
						.toPortableString(), projects[1], false);
		// in other project no path given, search all projects true
		final IErlElement element3 = ModelUtils.findExternalType(moduleB, "bx",
				"concat_thing", null, projects[1], true);
		// in other project no path given, search all projects false, -> null
		final IErlElement element4 = ModelUtils.findExternalType(moduleB, "bx",
				"concat_thing", null, projects[1], false);
		// then
		// it should be returned if found
		assertTrue(element1 instanceof IErlTypespec);
		assertTrue(element2 instanceof IErlTypespec);
		assertTrue(element3 instanceof IErlTypespec);
		assertNull(element4);
	}

	@Test
	public void findExternalFunctionModuleTest() throws Exception {
		// given
		// a module with functions and functions
		final IErlModule moduleD = ErlideTestUtils
				.createModule(projects[0], "d.erl",
						"-module(d).\n-export([f/0]).\nf() ->\n    ok.\ng() ->\n    ?MODULE:f().\n");
		moduleD.open(null);
		// when
		// looking for it with ?MODULE
		final IErlElement element1 = ModelUtils.findExternalFunction("?MODULE",
				new ErlangFunction("f", 0), null, projects[0], false, moduleD);
		// then
		// it should be found
		assertTrue(element1 instanceof IErlFunction);
	}

	@Test
	public void findFunctionInExternalFilesTest() throws Exception {
		// given
		// a module with calls to the lists module
		final IErlProject project = projects[0];
		final IErlModule moduleE = ErlideTestUtils
				.createModule(
						project,
						"e.erl",
						"-module(e).\n-export([f/0]).\nf() ->\n    lists:reverse([1, 0]),\n    lists:reverse([1, 0], [2]).\n");
		moduleE.open(null);
		// when
		// looking for lists:reverse/2 and lists:reverse/1
		final Backend backend = ErlangCore.getBackendManager().getIdeBackend();
		final IErlModel model = ErlangCore.getModel();
		final OpenResult res = ErlideOpen.open(backend,
				ErlangToolkit.createScannerModuleName(moduleE), 49,
				ModelUtils.getImportsAsList(moduleE),
				model.getExternalModules(project), model.getPathVars());
		final IErlElement function = ModelUtils.findExternalFunction(
				res.getName(), res.getFunction(), res.getPath(), project,
				false, moduleE);
		// final OpenResult res2 = ErlideOpen.open(backend,
		// ErlangToolkit.createScannerModuleName(moduleE), 81,
		// ModelUtils.getImportsAsList(moduleE),
		// model.getExternalModules(project), model.getPathVars());
		final IErlElement module = ModelUtils.findExternalModule(
				function.getModuleName(), res.getPath(), project, false);
		// then
		// the function should be returned and the module, in External Files
		assertNotNull(function);
		assertTrue(function instanceof IErlFunction);
		assertNotNull(module);
		assertEquals(function.getParent(), module);
		assertEquals(function.getModule().getProject(), project);
	}

	@Test
	public void findPreprocessorDefTest() throws Exception {
		// given
		// a module with includes and record
		final IErlProject project = projects[0];
		final IErlModule header = ErlideTestUtils
				.createModule(project, "a.hrl",
						"-record(rec1, {field, another=def}).\n-define(MACRO(A), lists:reverse(A)).\n");
		final IErlModule module = ErlideTestUtils
				.createModule(
						project,
						"f.erl",
						"-module(f).\n-include(\"a.hrl\").\n-export([f/0]).\n-record(rec2, {a, b}).\n"
								+ "f() ->\n    lists:reverse([1, 0]),\n    lists:reverse([1, 0], [2]).\n");
		module.open(null);
		final IErlPreprocessorDef preprocessorDef1 = ModelUtils
				.findPreprocessorDef(module, "rec1", Kind.RECORD_DEF, "");
		final IErlPreprocessorDef preprocessorDef2 = ModelUtils
				.findPreprocessorDef(header, "rec1", Kind.RECORD_DEF, "");
		final IErlPreprocessorDef preprocessorDef3 = ModelUtils
				.findPreprocessorDef(Arrays.asList(projects), "f.erl", "rec2",
						Kind.RECORD_DEF, "");
		// then
		// the record should be returned
		assertNotNull(module);
		assertNotNull(preprocessorDef1);
		assertTrue(preprocessorDef1 instanceof IErlRecordDef);
		assertEquals(preprocessorDef1, preprocessorDef2);
		assertEquals(preprocessorDef1.getParent(), header);
		assertNotNull(preprocessorDef3);
		assertEquals(preprocessorDef3.getParent(), module);
	}

	@Test
	public void findPreprocessorDefExternalHeaderTest() throws Exception {
		// given
		// a module with includes and record
		final IErlProject project = projects[0];
		final IErlModule module = ErlideTestUtils
				.createModule(
						project,
						"g.erl",
						"-module(g).\n-include_lib(\"kernel/include/file.hrl\").\n-export([f/0]).\n-record(rec2, {a, b}).\n"
								+ "f() ->\n    lists:reverse([1, 0]),\n    lists:reverse([1, 0], [2]).\n");
		module.open(null);
		// when
		// looking for the record
		final IErlPreprocessorDef preprocessorDef = ModelUtils
				.findPreprocessorDef(module, "file_info", Kind.RECORD_DEF, "");
		// then
		// the record should be returned
		assertNotNull(module);
		assertNotNull(preprocessorDef);
		assertTrue(preprocessorDef instanceof IErlRecordDef);
		assertEquals(preprocessorDef.getModule().getProject(), project);
	}

	private OtpErlangTuple makeTuple2(final String functionName, final int arity) {
		return new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom(functionName), new OtpErlangLong(arity) });
	}

	@Test
	public void getPreprocessorDefs() throws Exception {
		final IErlProject project = projects[0];
		final IErlModule module = ErlideTestUtils
				.createModule(
						project,
						"a.erl",
						"-module(g).\n-include_lib(\"kernel/include/file.hrl\").\n-export([f/0]).\n-define(A(B), '++B++').\n-record(rec2, {a, b}).\n"
								+ "f() ->\n    lists:reverse([1, 0]),\n    lists:reverse([1, 0], [2]).\n");
		module.open(null);
		final List<IErlPreprocessorDef> macrodDefs = ModelUtils
				.getPreprocessorDefs(module, Kind.MACRO_DEF, "");
		final List<IErlPreprocessorDef> recordDefs = ModelUtils
				.getPreprocessorDefs(module, Kind.RECORD_DEF, "");
		assertEquals(2, macrodDefs.size());
		assertEquals(3, recordDefs.size());
	}

	@Test
	public void getExternalModule() throws Exception {
		File externalFile = null;
		IErlProject erlProject = null;
		try {
			// given
			// an erlang project and an external file not in any project
			final String projectName = "testproject";
			erlProject = ErlideTestUtils.createTmpErlProject(projectName);
			final String externalFileName = "external.erl";
			externalFile = ErlideTestUtils
					.createTmpFile(externalFileName,
							"-module(external).\nf([_ | _]=L ->\n    atom_to_list(L).\n");
			final String absolutePath = externalFile.getAbsolutePath();
			final String externalsFileName = "x.erlidex";
			final File externalsFile = ErlideTestUtils.createTmpFile(
					externalsFileName, absolutePath);
			final IProject project = erlProject.getProject();
			final IOldErlangProjectProperties properties = erlProject
					.getProperties();
			final IEclipsePreferences root = new ProjectScope(project)
					.getNode(ErlangPlugin.PLUGIN_ID);
			properties.setExternalModulesFile(externalsFile.getAbsolutePath());
			properties.store(root);
			erlProject.open(null);
			// when
			// looking for it
			final IErlModule externalModule = ModelUtils.getExternalModule(
					ErlideUtil.withoutExtension(externalFileName), erlProject);
			// then
			// we should find it
			assertNotNull(externalModule);
			assertTrue(StringUtils.equalFilePaths(absolutePath,
					externalModule.getFilePath()));
		} finally {
			if (externalFile != null && externalFile.exists()) {
				externalFile.delete();
			}
			if (erlProject != null) {
				ErlideTestUtils.deleteProject(erlProject);
			}
		}

	}

	@Test
	public void getExternalModuleWithPrefix() throws Exception {
		File externalFile = null;
		IErlProject erlProject = null;
		try {
			// given
			// an erlang project and an external file not in any project
			final String projectName = "testproject";
			erlProject = ErlideTestUtils.createTmpErlProject(projectName);
			final String externalFileName = "external.erl";
			externalFile = ErlideTestUtils
					.createTmpFile(externalFileName,
							"-module(external).\nf([_ | _]=L ->\n    atom_to_list(L).\n");
			final String absolutePath = externalFile.getAbsolutePath();
			final String externalsFileName = "x.erlidex";
			final File externalsFile = ErlideTestUtils.createTmpFile(
					externalsFileName, absolutePath);
			final IProject project = erlProject.getProject();
			final IOldErlangProjectProperties properties = erlProject
					.getProperties();
			final IEclipsePreferences root = new ProjectScope(project)
					.getNode(ErlangPlugin.PLUGIN_ID);
			properties.setExternalModulesFile(externalsFile.getAbsolutePath());
			properties.store(root);
			erlProject.open(null);
			// when
			// looking for it
			final Backend backend = BackendUtils
					.getBuildOrIdeBackend(erlProject.getProject());
			final List<String> modules = ModelUtils
					.getExternalModulesWithPrefix(backend, "ex", erlProject);
			// then
			// we should find it
			assertEquals(modules.size(), 1);
			assertEquals(ErlideUtil.withoutExtension(externalFileName),
					modules.get(0));
		} finally {
			if (externalFile != null && externalFile.exists()) {
				externalFile.delete();
			}
			if (erlProject != null) {
				ErlideTestUtils.deleteProject(erlProject);
			}
		}
	}

	@Test
	public void findExternalModuleFromPath() throws Exception {
		File externalFile = null;
		IErlProject erlProject = null;
		try {
			// given
			// an erlang project and an external file not in any project
			final String projectName = "testproject";
			erlProject = ErlideTestUtils.createTmpErlProject(projectName);
			final String externalFileName = "external.erl";
			externalFile = ErlideTestUtils
					.createTmpFile(externalFileName,
							"-module(external).\nf([_ | _]=L ->\n    atom_to_list(L).\n");
			final String absolutePath = externalFile.getAbsolutePath();
			final String externalsFileName = "x.erlidex";
			final File externalsFile = ErlideTestUtils.createTmpFile(
					externalsFileName, absolutePath);
			final IProject project = erlProject.getProject();
			final IOldErlangProjectProperties properties = erlProject
					.getProperties();
			final IEclipsePreferences root = new ProjectScope(project)
					.getNode(ErlangPlugin.PLUGIN_ID);
			properties.setExternalModulesFile(externalsFile.getAbsolutePath());
			properties.store(root);
			erlProject.open(null);
			// when
			// looking for it
			final IErlModule module = ModelUtils
					.findExternalModuleFromPath(absolutePath);
			// then
			// we should find it
			assertNotNull(module);
			assertEquals(externalFileName, module.getName());
		} finally {
			if (externalFile != null && externalFile.exists()) {
				externalFile.delete();
			}
			if (erlProject != null) {
				ErlideTestUtils.deleteProject(erlProject);
			}
		}
	}

	@Test
	public void getModulesWithReferencedProjectsWithPrefix() throws Exception {
		// given
		// two erlang projects, the first references the second, second has
		// an erlang module
		final IProject project = projects[0].getProject();
		final IProjectDescription description = project.getDescription();
		final IProject[] refs = new IProject[] { projects[1].getProject() };
		description.setReferencedProjects(refs);
		project.setDescription(description, null);
		final IErlModule module = ErlideTestUtils.createModule(projects[1],
				"abc.erl",
				"-module(abc).\n-export(f/0)\nf() ->\n   {abc, ok}.\n");
		ErlideTestUtils.createModule(projects[0], "bbc.erl",
				"-module(bbc).\n-export(f/0)\nf() ->\n   {abc, ok}.\n");
		// when
		// looking for module with prefix, it should be found
		final List<IErlModule> modules = ModelUtils
				.getModulesWithReferencedProjectsWithPrefix(projects[0], "a");
		// then
		// we should find it
		assertNotNull(modules);
		assertEquals(1, modules.size());
		assertEquals(module, modules.get(0));
	}

	@Test
	public void getModuleFromExternalModulePath() throws Exception {
		File externalFile = null;
		IErlProject erlProject = null;
		try {
			// given
			// an erlang project and an external file not in any project
			final String projectName = "testproject";
			erlProject = ErlideTestUtils.createTmpErlProject(projectName);
			final String externalFileName = "external.erl";
			externalFile = ErlideTestUtils
					.createTmpFile(externalFileName,
							"-module(external).\nf([_ | _]=L ->\n    atom_to_list(L).\n");
			final String absolutePath = externalFile.getAbsolutePath();
			final String externalsFileName = "x.erlidex";
			final File externalsFile = ErlideTestUtils.createTmpFile(
					externalsFileName, absolutePath);
			final IProject project = erlProject.getProject();
			final IOldErlangProjectProperties properties = erlProject
					.getProperties();
			final IEclipsePreferences root = new ProjectScope(project)
					.getNode(ErlangPlugin.PLUGIN_ID);
			properties.setExternalModulesFile(externalsFile.getAbsolutePath());
			properties.store(root);
			erlProject.open(null);
			// when
			// looking for it with its external module path
			final IErlModule module = ModelUtils
					.findExternalModuleFromPath(absolutePath);
			assertNotNull(module);
			final String externalModulePath = ModelUtils
					.getExternalModulePath(module);
			final IErlModule module2 = ModelUtils
					.getModuleFromExternalModulePath(externalModulePath);
			// then
			// we should find it
			assertEquals(externalFileName, module.getName());
			assertEquals(module, module2);
		} finally {
			if (externalFile != null && externalFile.exists()) {
				externalFile.delete();
			}
			if (erlProject != null) {
				ErlideTestUtils.deleteProject(erlProject);
			}
		}
	}

	@Test
	public void findIncludeFile() throws Exception {
		// given
		// a project with a module and an include including file.hrl
		final IErlProject project = projects[0];
		final String headerName = "a.hrl";
		final IErlModule header = ErlideTestUtils
				.createModule(
						project,
						headerName,
						"-include_lib(\"kernel/include/file.hrl\").\n-record(rec1, {field, another=def}).\n-define(MACRO(A), lists:reverse(A)).\n");
		header.open(null);
		final IErlModule module = ErlideTestUtils
				.createModule(
						project,
						"f.erl",
						"-module(f).\n-include(\"a.hrl\").\n-export([f/0]).\n-record(rec2, {a, b}).\n"
								+ "f() ->\n    lists:reverse([1, 0]),\n    lists:reverse([1, 0], [2]).\n");
		module.open(null);
		// when
		// looking for the include
		final String s = ModelUtils.findIncludeFile(project, headerName, "");
		final String filePath = ModelUtils.findIncludeFile(project, "file.hrl",
				"");
		// then
		// it should be found
		assertEquals(header.getFilePath(), s);
		assertNotNull(filePath);
		assertNotSame("file.hrl", filePath);
	}

	@Test
	public void findTypespec() throws Exception {
		// given
		// a project with a module and an include with a typespec
		final IErlProject project = projects[0];
		final String headerName = "a.hrl";
		final IErlModule header = ErlideTestUtils
				.createModule(project, headerName,
						"-type date() :: {pos_integer(), pos_integer(), pos_integer()}.\n");
		header.open(null);
		final IErlModule module = ErlideTestUtils
				.createModule(
						project,
						"f.erl",
						"-module(f).\n-include(\"a.hrl\").\n-export([f/0]).\n-record(rec2, {a, b}).\n"
								+ "f() ->\n    lists:reverse([1, 0]),\n    lists:reverse([1, 0], [2]).\n");
		module.open(null);
		// when
		// looking for the typespec
		final IErlTypespec typespec = ModelUtils.findTypespec(module, "date",
				"");
		// then
		// it should be found
		assertNotNull(typespec);
		assertEquals(typespec.getParent(), header);
	}

	@Test
	public void findFunction() throws Exception {
		// given
		// a project with a module with a function
		final IErlProject project = projects[0];
		final IErlModule module = ErlideTestUtils
				.createModule(
						project,
						"f.erl",
						"-module(f).\n-export([f/0]).\n-record(rec2, {a, b}).\n"
								+ "f() ->\n    lists:reverse([1, 0]),\n    lists:reverse([1, 0], [2]).\n");
		module.open(null);
		// when
		// looking for the function
		final IErlFunction function = ModelUtils.findFunction(module,
				new ErlangFunction("f", 0));
		// then
		// it should be found
		assertNotNull(function);
		assertEquals(function.getParent(), module);
	}

}
