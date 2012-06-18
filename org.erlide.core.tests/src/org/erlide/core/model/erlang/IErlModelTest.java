package org.erlide.core.model.erlang;

import static org.junit.Assert.*;

import java.io.File;
import java.net.URI;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElementLocator;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IErlModelChangeListener;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.model.util.ErlangFunction;
import org.erlide.test.support.ErlideTestUtils;
import org.erlide.utils.Util;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class IErlModelTest extends ErlModelTestBase {

    private static final String XX_ERLIDEX = "xx.erlidex";
    private static final String PV = "XXYYZZ";
    private IErlModel model;

    @Before
    @Override
    public void setUp() throws Exception {
        super.setUp();
        model = project.getModel();
    }

    @After
    @Override
    public void tearDown() throws Exception {
        ResourcesPlugin.getWorkspace().getPathVariableManager()
                .setURIValue(PV, (URI) null);
        super.tearDown();
    }

    // void addModelChangeListener(IErlModelChangeListener listener);
    // void removeModelChangeListener(IErlModelChangeListener listener);
    @Test
    public void addModelChangeListener() throws Exception {
        final List<IErlElement> changed = Lists.newArrayList();
        final IErlModelChangeListener listener = new IErlModelChangeListener() {

            @Override
            public void elementChanged(final IErlElement element) {
                changed.add(element);
            }
        };
        final IErlModule module2 = ErlideTestUtils.createModule(project,
                "zz.erl", "-module(zz).\n");
        final IErlModule module3 = ErlideTestUtils.createModule(project,
                "tt.erl", "-module(tt).\n");
        model.addModelChangeListener(listener);
        module2.open(null);
        model.removeModelChangeListener(listener);
        module3.open(null);
        final List<IErlElement> changed2 = Lists.newArrayList(changed);
        changed.clear();
        final List<IErlElement> changed3 = Lists.newArrayList(changed);
        changed.clear();
        assertEquals(1, changed2.size());
        assertEquals(0, changed3.size());
    }

    // IErlElement findElement(IResource resource);
    @Test
    public void findElement() throws Exception {
        final IProject workspaceProject = project.getWorkspaceProject();
        final IErlElement findElement = model.findElement(workspaceProject);
        final IErlElement findElement2 = model.findElement(module
                .getCorrespondingResource());
        final IErlElement findElement3 = model.findElement(workspaceProject
                .getFolder("no_way"));
        final IErlElement findElement4 = model.findElement(workspaceProject
                .getParent());
        final IErlElement parent = (IErlElement) module.getParent();
        final IFolder folder = (IFolder) parent.getResource();
        final IErlElement findElement5 = model.findElement(folder);
        project.open(null);
        final IErlElement srcFolder = project.getChildNamed("src");
        assertEquals(project, findElement);
        assertEquals(module, findElement2);
        assertNull(findElement3);
        assertNull(findElement4);
        assertEquals(srcFolder, findElement5);
    }

    // IErlElement findElement(IResource resource, boolean openElements);

    // IErlProject findProject(IProject project);
    @Test
    public void findProject() throws Exception {
        final IProject workspaceProject = project.getWorkspaceProject();
        final IErlProject project2 = projects[1];
        final IProject workspaceProject2 = project2.getWorkspaceProject();
        final IErlProject findProject = model.findProject(workspaceProject);
        final IErlProject findProject2 = model.findProject(workspaceProject2);
        assertEquals(project, findProject);
        assertEquals(project2, findProject2);
    }

    // IErlModule findModule(IFile file);
    @Test
    public void findModule() throws Exception {
        final IErlModule module2 = ErlideTestUtils.createModule(project,
                "zz.erl", "-module(zz).\n");
        final IFile file = (IFile) module2.getResource();
        final IErlModule findModule = model.findModule(file);
        final IErlElement parent = (IErlElement) module2.getParent();
        final IFolder folder = (IFolder) parent.getResource();
        IFile createFile = null;
        try {
            createFile = ErlideTestUtils.createFile("tt.erl", "-module(tt).\n",
                    folder);
            model.findModule(createFile);
            assertEquals(module2, findModule);
        } finally {
            if (createFile != null && createFile.exists()) {
                createFile.delete(true, null);
            }
        }
    }

    // IErlModule findModule(String name) throws ErlModelException;
    @Test
    public void findModule_name() throws Exception {
        ErlideTestUtils
                .createModule(projects[1], "zz.hrl", "-define(X, zz).\n");
        final IErlModule findModule = model.findModule("xx");
        final IErlModule findModule2 = model.findModule("xx.erl");
        final IErlModule findModule3 = model.findModule("yy.hrl");
        final IErlModule findModule4 = model.findModule("zz");
        final IErlModule findModule5 = model.findModule("zz.hrl");
        final IErlModule findModule6 = model.findModule("XX");
        assertEquals(module, findModule);
        assertEquals(module, findModule2);
        assertNull(findModule3);
        assertNull(findModule4);
        assertNull(findModule5);
        assertNull(findModule6);
    }

    // IErlModule findModuleIgnoreCase(String name) throws ErlModelException;
    @Test
    public void findModuleIgnoreCase() throws Exception {
        final IErlModule findModule = model.findModuleIgnoreCase("XX");
        final IErlModule findModule2 = model.findModuleIgnoreCase("XX.ERL");
        assertEquals(module, findModule);
        assertEquals(module, findModule2);
    }

    // IErlModule findModule(String moduleName, String modulePath)
    // throws ErlModelException;
    @Test
    public void findModule_namePath() throws Exception {
        final IErlModule findModule = model.findModule("xx", null);
        final IErlModule findModule2 = model.findModule("xx.erl", null);
        final String filePath = module.getFilePath();
        final IErlModule findModule3 = model.findModule(null, filePath);
        final IErlModule findModule4 = model.findModule("apa", filePath);
        assertEquals(module, findModule);
        assertEquals(module, findModule2);
        assertEquals(module, findModule3);
        assertEquals(module, findModule4);
    }

    // IErlModule findInclude(final String includeName, final String
    // includePath)
    @Test
    public void findInclude() throws Exception {
        final IErlModule module2 = ErlideTestUtils.createModule(projects[1],
                "zz.hrl", "-define(X, zz).\n");
        final IErlModule include = ErlideTestUtils.createInclude(projects[1],
                "xx.hrl", "-define(X, xx).\n");
        final IErlModule findInclude = model.findInclude("zz", null);
        final IErlModule findInclude2 = model.findInclude("xx", null);
        final String filePath = include.getFilePath();
        final IErlModule findInclude3 = model.findInclude(null, filePath);
        final IErlModule findInclude4 = model.findInclude("apa", filePath);
        final IErlModule findInclude5 = model.findInclude(null,
                module2.getFilePath());
        assertNull(findInclude);
        assertEquals(include, findInclude2);
        assertEquals(include, findInclude3);
        assertEquals(include, findInclude4);
        assertNull(findInclude5);
    }

    // IErlElement innermostThat(final IErlElement el,
    // final IErlangFirstThat firstThat);

    // OtpErlangList getPathVars();
    @Test
    public void getPathVars() throws Exception {
        final OtpErlangList pathVars = model.getPathVars();
        final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
                .getPathVariableManager();
        final String[] pathVariableNames = pvm.getPathVariableNames();
        final int n = pathVariableNames.length;
        assertEquals(n, pathVars.arity());

        final URI path = ErlideTestUtils.getTmpURIPath("");
        pvm.setURIValue(PV, path);
        final OtpErlangList pathVars2 = model.getPathVars();
        assertEquals(n + 1, pathVars2.arity());

        final OtpErlangTuple t = (OtpErlangTuple) pathVars2.elementAt(0);
        final String name = Util.stringValue(t.elementAt(0));
        final String value = pvm.getURIValue(name).getPath();
        final String value2 = Util.stringValue(t.elementAt(1));
        assertEquals(value, value2);
    }

    // IErlFunction findFunction(FunctionRef r) throws ErlModelException;
    @Test
    public void findFunction() throws Exception {
        final FunctionRef functionRef = new FunctionRef("xx", "f", 1);
        final FunctionRef functionRef2 = new FunctionRef("xx", "f",
                ErlangFunction.ANY_ARITY);
        final FunctionRef functionRef3 = new FunctionRef("xx", "f", 0);
        final IErlFunction findFunction = model.findFunction(functionRef);
        final IErlFunction findFunction2 = model.findFunction(functionRef2);
        final IErlFunction findFunction3 = model.findFunction(functionRef3);
        assertNotNull(findFunction);
        assertNotNull(findFunction2);
        assertNull(findFunction3);
    }

    // IErlModule findIncludeFromModule(IErlModule module, String includeName,
    // String includePath, Scope scope) throws ErlModelException;
    @Test
    public void findIncludeFromModule() throws Exception {
        File externalIncludeFile = null;
        final IErlProject myProject = projects[0];
        final IProject workspaceProject = myProject.getWorkspaceProject();
        final IProject[] referencedProjects = workspaceProject
                .getReferencedProjects();
        final Collection<IPath> includeDirs = myProject.getIncludeDirs();
        // given
        // a project with an external include and an internal include and a
        // referenced project with an include and an include in the same
        // directory as the module
        try {
            final String xxHrl = "xx.hrl";
            externalIncludeFile = ErlideTestUtils.createTmpFile(xxHrl,
                    "-record(rec2, {field, another=def}.");
            final String externalIncludePath = externalIncludeFile
                    .getAbsolutePath();
            final IPath p = new Path(externalIncludePath).removeLastSegments(1);
            final List<IPath> newIncludeDirs = Lists.newArrayList(includeDirs);
            newIncludeDirs.add(p);
            myProject.setIncludeDirs(newIncludeDirs);
            final IErlModule include = ErlideTestUtils.createInclude(myProject,
                    "yy.hrl", "-define(Y, include).\n");
            final IErlProject project1 = projects[1];
            final IErlModule referencedInclude = ErlideTestUtils.createInclude(
                    project1, "zz.hrl", "-define(Z, referenced).\n");
            final IErlModule includeInModuleDir = ErlideTestUtils.createModule(
                    myProject, "ww.hrl", "-define(WW, x).\n");
            myProject.open(null);
            // when
            // looking for includes
            final String xx = "xx";
            final IErlModule x1 = model.findIncludeFromModule(module, xx, null,
                    IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule x2 = model.findIncludeFromModule(module, xx, null,
                    IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule x3 = model.findIncludeFromModule(module, xx, null,
                    IErlElementLocator.Scope.REFERENCED_PROJECTS);
            final String yy = "yy";
            final IErlModule y1 = model.findIncludeFromModule(module, yy, null,
                    IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule y2 = model.findIncludeFromModule(module, yy, null,
                    IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule y3 = model.findIncludeFromModule(module, yy, null,
                    IErlElementLocator.Scope.REFERENCED_PROJECTS);
            final String zz = "zz";
            final IErlModule z1 = model.findIncludeFromModule(module, zz, null,
                    IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule z2 = model.findIncludeFromModule(module, zz, null,
                    IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule z3 = model.findIncludeFromModule(module, zz, null,
                    IErlElementLocator.Scope.REFERENCED_PROJECTS);
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(new IProject[] { project1
                    .getWorkspaceProject() });
            workspaceProject.setDescription(description, null);
            myProject.open(null);
            final IErlModule z4 = model.findIncludeFromModule(module, zz, null,
                    IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule z5 = model.findIncludeFromModule(module, zz, null,
                    IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule z6 = model.findIncludeFromModule(module, zz, null,
                    IErlElementLocator.Scope.REFERENCED_PROJECTS);
            final String ww = "ww";
            final IErlModule w1 = model.findIncludeFromModule(module, ww, null,
                    IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlElementLocator mymodel = myProject.getModel();
            final IErlModule w2 = mymodel.findIncludeFromProject(myProject, ww,
                    null, IErlElementLocator.Scope.PROJECT_ONLY);
            // then
            // scope should be respected
            assertNotNull(x1);
            assertEquals(xxHrl, x1.getName());
            assertNotNull(x2);
            assertEquals(xxHrl, x2.getName());
            assertNotNull(x3);
            assertEquals(xxHrl, x3.getName());
            assertEquals(include, y1);
            assertEquals(include, y2);
            assertEquals(include, y3);
            assertNull(z1);
            assertEquals(referencedInclude, z2);
            assertNull(z3);
            assertNull(z4);
            assertEquals(referencedInclude, z5);
            assertEquals(referencedInclude, z6);
            assertEquals(includeInModuleDir, w1);
            assertNull(w2);
        } finally {
            if (externalIncludeFile != null && externalIncludeFile.exists()) {
                externalIncludeFile.delete();
            }
            myProject.setIncludeDirs(includeDirs);
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(referencedProjects);
            workspaceProject.setDescription(description, null);
        }
    }

    // enum Scope {
    // PROJECT_ONLY, REFERENCED_PROJECTS, ALL_PROJECTS
    // }
    // IErlModule findModuleFromProject(IErlProject project, String moduleName,
    // String modulePath, Scope scope) throws ErlModelException;
    @Test
    public void findModuleFromProject() throws Exception {
        File externalModuleFile = null;
        File externalsFile = null;
        final IErlProject aProject = projects[0];
        final IProject workspaceProject = aProject.getWorkspaceProject();
        final IProject[] referencedProjects = workspaceProject
                .getReferencedProjects();
        final String externalModulesString = aProject
                .getExternalModulesString();
        // given
        // a project with an external module and an internal module and a
        // referenced project with a module
        try {
            final String xxErl = "xx.erl";
            externalModuleFile = ErlideTestUtils.createTmpFile(xxErl,
                    "-module(xx).\n");
            final String externalModulePath = externalModuleFile
                    .getAbsolutePath();
            externalsFile = ErlideTestUtils.createTmpFile(XX_ERLIDEX,
                    externalModulePath);
            aProject.setExternalModulesFile(externalsFile.getAbsolutePath());
            final IErlModule aModule = ErlideTestUtils.createModule(aProject,
                    "yy.erl", "-module(yy).\n");
            final IErlProject project1 = projects[1];
            final IErlModule referencedModule = ErlideTestUtils.createModule(
                    project1, "zz.erl", "-module(zz).\n");
            aProject.open(null);
            // when
            // looking for modules
            final String xx = "xx";
            final IErlModule x1 = model.findModuleFromProject(aProject, xx,
                    null, IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule x2 = model.findModuleFromProject(aProject, xx,
                    null, IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule x3 = model.findModuleFromProject(aProject, xx,
                    null, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            final String yy = "yy";
            final IErlModule y1 = model.findModuleFromProject(aProject, yy,
                    null, IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule y2 = model.findModuleFromProject(aProject, yy,
                    null, IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule y3 = model.findModuleFromProject(aProject, yy,
                    null, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            final IErlModule y4 = model.findModuleFromProject(project1, yy,
                    null, IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule y5 = model.findModuleFromProject(project1, yy,
                    null, IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule y6 = model.findModuleFromProject(project1, yy,
                    null, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            final String zz = "zz";
            final IErlModule z1 = model.findModuleFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule z2 = model.findModuleFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule z3 = model.findModuleFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(new IProject[] { project1
                    .getWorkspaceProject() });
            workspaceProject.setDescription(description, null);
            aProject.open(null);
            final IErlModule z4 = model.findModuleFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule z5 = model.findModuleFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule z6 = model.findModuleFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            // then
            // scope should be respected
            assertNotNull(x1);
            assertEquals(xxErl, x1.getName());
            assertNotNull(x2);
            assertEquals(xxErl, x2.getName());
            assertNotNull(x3);
            assertEquals(xxErl, x3.getName());
            assertEquals(aModule, y1);
            assertEquals(aModule, y2);
            assertEquals(aModule, y3);
            assertNull(y4);
            assertEquals(aModule, y5);
            assertNull(y6);
            assertNull(z1);
            assertEquals(referencedModule, z2);
            assertNull(z3);
            assertNull(z4);
            assertEquals(referencedModule, z5);
            assertEquals(referencedModule, z6);
        } finally {
            if (externalModuleFile != null && externalModuleFile.exists()) {
                externalModuleFile.delete();
            }
            if (externalsFile != null && externalsFile.exists()) {
                externalsFile.delete();
            }
            aProject.setExternalModulesFile(externalModulesString);
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(referencedProjects);
            workspaceProject.setDescription(description, null);
        }
    }

    @Test
    public void findModuleFromProject_preferProjectFile() throws Exception {
        File externalModuleFile = null;
        File externalsFile = null;
        final IErlProject aProject = projects[0];
        final IProject workspaceProject = aProject.getWorkspaceProject();
        final IProject[] referencedProjects = workspaceProject
                .getReferencedProjects();
        final String externalModulesString = aProject
                .getExternalModulesString();
        // given
        // a project with an external include and a
        // referenced project with an include, both have same name
        try {
            final String zzErl = "zz.erl";
            final String xxxContents = "-module(zz).\n";
            externalModuleFile = ErlideTestUtils.createTmpFile(zzErl,
                    xxxContents);
            final String externalModulePath = externalModuleFile
                    .getAbsolutePath();
            externalsFile = ErlideTestUtils.createTmpFile(XX_ERLIDEX,
                    externalModulePath);
            aProject.setExternalModulesFile(externalsFile.getAbsolutePath());
            final IErlProject project1 = projects[1];
            final IErlModule referencedModule = ErlideTestUtils.createModule(
                    project1, zzErl, xxxContents);
            aProject.open(null);
            // when
            // looking for module
            final String zz = "zz";
            final IErlModule zz1 = model.findModuleFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule zz2 = model.findModuleFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule zz3 = model.findModuleFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(new IProject[] { project1
                    .getWorkspaceProject() });
            workspaceProject.setDescription(description, null);
            aProject.open(null);
            final IErlModule zz4 = model.findModuleFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule zz5 = model.findModuleFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule zz6 = model.findModuleFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            // then
            // the non-external should be preferred
            assertNotNull(zz1);
            assertEquals(zzErl, zz1.getName());
            assertNotSame(referencedModule, zz1);
            assertEquals(referencedModule, zz2);
            assertNotNull(zz3);
            assertEquals(zzErl, zz3.getName());
            assertNotSame(referencedModule, zz3);
            assertNotNull(zz4);
            assertNotSame(referencedModule, zz4);
            assertEquals(referencedModule, zz5);
            assertEquals(referencedModule, zz6);
        } finally {
            if (externalModuleFile != null && externalModuleFile.exists()) {
                externalModuleFile.delete();
            }
            if (externalsFile != null && externalsFile.exists()) {
                externalsFile.delete();
            }
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(referencedProjects);
            workspaceProject.setDescription(description, null);
            aProject.setExternalModulesFile(externalModulesString);
        }
    }

    // IErlModule findIncludeFromProject(IErlProject project, String
    // includeName, String includePath, Scope scope) throws ErlModelException;
    @Test
    public void findIncludeFromProject() throws Exception {
        File externalIncludeFile = null;
        final IErlProject aProject = projects[0];
        final IProject workspaceProject = aProject.getWorkspaceProject();
        final IProject[] referencedProjects = workspaceProject
                .getReferencedProjects();
        final Collection<IPath> includeDirs = aProject.getIncludeDirs();
        // given
        // a project with an external include and an internal include and a
        // referenced project with an include
        try {
            final String xxHrl = "xx.hrl";
            externalIncludeFile = ErlideTestUtils.createTmpFile(xxHrl,
                    "-record(rec2, {field, another=def}.");
            final String externalIncludePath = externalIncludeFile
                    .getAbsolutePath();
            final IPath p = new Path(externalIncludePath).removeLastSegments(1);
            final List<IPath> newIncludeDirs = Lists.newArrayList(includeDirs);
            newIncludeDirs.add(p);
            aProject.setIncludeDirs(newIncludeDirs);
            final IErlModule include = ErlideTestUtils.createInclude(aProject,
                    "yy.hrl", "-define(Y, include).\n");
            final IErlProject project1 = projects[1];
            final IErlModule referencedInclude = ErlideTestUtils.createInclude(
                    project1, "zz.hrl", "-define(Z, referenced).\n");
            aProject.open(null);
            // when
            // looking for includes
            final String xx = "xx";
            final IErlModule x1 = model.findIncludeFromProject(aProject, xx,
                    null, IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule x2 = model.findIncludeFromProject(aProject, xx,
                    null, IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule x3 = model.findIncludeFromProject(aProject, xx,
                    null, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            final String yy = "yy";
            final IErlModule y1 = model.findIncludeFromProject(aProject, yy,
                    null, IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule y2 = model.findIncludeFromProject(aProject, yy,
                    null, IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule y3 = model.findIncludeFromProject(aProject, yy,
                    null, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            final IErlModule y4 = model.findIncludeFromProject(project1, yy,
                    null, IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule y5 = model.findIncludeFromProject(project1, yy,
                    null, IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule y6 = model.findIncludeFromProject(project1, yy,
                    null, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            final String zz = "zz";
            final IErlModule z1 = model.findIncludeFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule z2 = model.findIncludeFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule z3 = model.findIncludeFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(new IProject[] { project1
                    .getWorkspaceProject() });
            workspaceProject.setDescription(description, null);
            aProject.open(null);
            final IErlModule z4 = model.findIncludeFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule z5 = model.findIncludeFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule z6 = model.findIncludeFromProject(aProject, zz,
                    null, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            // then
            // scope should be respected
            assertNotNull(x1);
            assertEquals(xxHrl, x1.getName());
            assertNotNull(x2);
            assertEquals(xxHrl, x2.getName());
            assertNotNull(x3);
            assertEquals(xxHrl, x3.getName());
            assertEquals(include, y1);
            assertEquals(include, y2);
            assertEquals(include, y3);
            assertNull(y4);
            assertEquals(include, y5);
            assertNull(y6);
            assertNull(z1);
            assertEquals(referencedInclude, z2);
            assertNull(z3);
            assertNull(z4);
            assertEquals(referencedInclude, z5);
            assertEquals(referencedInclude, z6);
        } finally {
            if (externalIncludeFile != null && externalIncludeFile.exists()) {
                externalIncludeFile.delete();
            }
            aProject.setIncludeDirs(includeDirs);
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(referencedProjects);
            workspaceProject.setDescription(description, null);
        }
    }

    @Test
    public void findInclude_preferProjectFile() throws Exception {
        File externalIncludeFile = null;
        final IErlProject aProject = projects[0];
        final IProject workspaceProject = aProject.getWorkspaceProject();
        final IProject[] referencedProjects = workspaceProject
                .getReferencedProjects();
        final Collection<IPath> includeDirs = aProject.getIncludeDirs();
        // given
        // a project with an external include and a
        // referenced project with an include, both have same name
        try {
            final String xxHrl = "xx.hrl";
            externalIncludeFile = ErlideTestUtils.createTmpFile(xxHrl,
                    "-record(rec2, {field, another=def}.");
            final String externalIncludePath = externalIncludeFile
                    .getAbsolutePath();
            final IPath p = new Path(externalIncludePath).removeLastSegments(1);
            final List<IPath> newIncludeDirs = Lists.newArrayList(includeDirs);
            newIncludeDirs.add(p);
            aProject.setIncludeDirs(newIncludeDirs);
            final IErlProject project1 = projects[1];
            final IErlModule referencedInclude = ErlideTestUtils.createInclude(
                    project1, xxHrl, "-define(Z, referenced).\n");
            aProject.open(null);
            // when
            // looking for includes
            final String xx = "xx";
            final IErlModule x1 = model.findIncludeFromProject(aProject, xx,
                    null, IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule x2 = model.findIncludeFromProject(aProject, xx,
                    null, IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule x3 = model.findIncludeFromProject(aProject, xx,
                    null, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(new IProject[] { project1
                    .getWorkspaceProject() });
            workspaceProject.setDescription(description, null);
            aProject.open(null);
            final IErlModule x4 = model.findIncludeFromProject(aProject, xx,
                    null, IErlElementLocator.Scope.PROJECT_ONLY);
            final IErlModule x5 = model.findIncludeFromProject(aProject, xx,
                    null, IErlElementLocator.Scope.ALL_PROJECTS);
            final IErlModule x6 = model.findIncludeFromProject(aProject, xx,
                    null, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            // then
            // the non-external should be preferred
            assertNotNull(x1);
            assertEquals(xxHrl, x1.getName());
            assertNotSame(referencedInclude, x1);
            assertEquals(referencedInclude, x2);
            assertNotNull(x3);
            assertEquals(xxHrl, x3.getName());
            assertNotSame(referencedInclude, x3);
            assertNotNull(x4);
            assertNotSame(referencedInclude, x4);
            assertEquals(referencedInclude, x5);
            assertEquals(referencedInclude, x6);
        } finally {
            if (externalIncludeFile != null && externalIncludeFile.exists()) {
                externalIncludeFile.delete();
            }
            aProject.setIncludeDirs(includeDirs);
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(referencedProjects);
            workspaceProject.setDescription(description, null);
        }
    }

    // IErlProject newProject(final String name, final String path)
    // throws ErlModelException;

}
