package org.erlide.core.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.erlide.core.common.Util;
import org.erlide.core.model.root.api.IErlElement;
import org.erlide.core.model.root.api.IErlModel;
import org.erlide.core.model.root.api.IErlModelChangeListener;
import org.erlide.core.model.root.api.IErlProject;
import org.erlide.core.model.util.ErlangFunction;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class IErlModelTest extends ErlModelTestBase {

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
                .setValue(PV, null);
        super.tearDown();
    }

    // void copy(IErlElement[] elements, IErlElement[] containers,
    // IErlElement[] siblings, String[] renamings, boolean replace,
    // IProgressMonitor monitor) throws ErlModelException;
    // void delete(IErlElement[] elements, boolean force, IProgressMonitor
    // monitor)
    // throws ErlModelException;
    // IErlProject getErlangProject(IProject project);
    // Collection<IErlProject> getErlangProjects() throws ErlModelException;
    // IWorkspace getWorkspace();
    // void move(IErlElement[] elements, IErlElement[] containers,
    // IErlElement[] siblings, String[] renamings, boolean replace,
    // IProgressMonitor monitor) throws ErlModelException;
    // void rename(IErlElement[] elements, IErlElement[] destinations,
    // String[] names, boolean replace, IProgressMonitor monitor)
    // throws ErlModelException;

    // void addModelChangeListener(IErlModelChangeListener listener);
    // void removeModelChangeListener(IErlModelChangeListener listener);
    @Test
    public void addModelChangeListener() throws Exception {
        final List<IErlElement> changed = Lists.newArrayList();
        final IErlModelChangeListener listener = new IErlModelChangeListener() {

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

    // throws ErlModelException;
    // IErlElement innermostThat(final IErlElement el,
    // final IErlangFirstThat firstThat);

    // OtpErlangList getPathVars();
    @Test
    public void getPathVars() throws Exception {
        final OtpErlangList pathVars = model.getPathVars();
        final IPathVariableManager pathVariableManager = ResourcesPlugin
                .getWorkspace().getPathVariableManager();
        final String[] pathVariableNames = pathVariableManager
                .getPathVariableNames();
        final IPath path = ErlideTestUtils.getTmpPath("");
        pathVariableManager.setValue(PV, path);
        final OtpErlangList pathVars2 = model.getPathVars();
        final int n = pathVariableNames.length;
        final OtpErlangTuple t = (OtpErlangTuple) pathVars2.elementAt(0);
        final String name = Util.stringValue(t.elementAt(0));
        final String value = pathVariableManager.getValue(name)
                .toPortableString();
        final String value2 = Util.stringValue(t.elementAt(1));
        assertEquals(n, pathVars.arity());
        assertEquals(n + 1, pathVars2.arity());
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

    // IErlProject newProject(final String name, final String path)
    // throws ErlModelException;

}
