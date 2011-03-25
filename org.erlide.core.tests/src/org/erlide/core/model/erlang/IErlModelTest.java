package org.erlide.core.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.List;

import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.erlide.core.common.Util;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class IErlModelTest extends ErlModelTestBase {

    private IErlModel model;

    @Before
    @Override
    public void setUp() throws Exception {
        super.setUp();
        model = project.getModel();
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
    // void notifyChange(IErlElement element);

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
        final String name = module.getName();
        model.addModelChangeListener(listener);
        final IPath srcPath = project.getWorkspaceProject().getFullPath()
                .append("src");
        module.getResource().move(srcPath.append("yy.erl"), true, null);
        model.removeModelChangeListener(listener);
        final List<IErlElement> changed2 = Lists.newArrayList(changed);
        changed.clear();
        final IErlModule module2 = project.findModule("yy", null);
        module2.getResource().move(srcPath.append(name), true, null);
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
        final IErlElement findElement5 = model.findElement(parent
                .getCorrespondingResource());
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
    // IErlModule findModule(IFile file);
    // IErlModule findModule(String name) throws ErlModelException;
    // IErlModule findModuleIgnoreCase(String name) throws ErlModelException;
    // IErlModule findModule(String moduleName, String modulePath)
    // throws ErlModelException;
    // IErlModule findInclude(final String includeName, final String
    // includePath)
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
        final String PV = "XXYYZZ";
        try {
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
        } finally {
            pathVariableManager.setValue(PV, null);
        }
    }

    // IErlFunction findFunction(FunctionRef r) throws ErlModelException;
    // IErlProject newProject(final String name, final String path)
    // throws ErlModelException;

}
