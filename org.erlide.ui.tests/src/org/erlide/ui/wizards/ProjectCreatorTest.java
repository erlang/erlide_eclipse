package org.erlide.ui.wizards;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import java.net.URI;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.operation.IRunnableContext;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.IProjectConfiguratorFactory;
import org.erlide.engine.model.root.NewProjectData;
import org.erlide.ui.tests.util.DummyRunnableContext;
import org.erlide.ui.tests.util.WorkbenchHelper;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class ProjectCreatorTest {

    private ProjectCreator creator;
    private static URI location;
    private static IRunnableContext context;
    private static String name;
    private IProject prj = null;
    static IProjectConfiguratorFactory factory;

    @BeforeClass
    public static void init() {
        name = "demo111";
        location = null;
        context = new DummyRunnableContext();
        factory = ErlangEngine.getInstance().getProjectConfiguratorFactory();

        WorkbenchHelper.waitForWorkbench();
    }

    @Before
    public void setup() throws CoreException {
        if (prj != null) {
            if (prj.exists()) {
                prj.delete(true, null);
            }
            if (prj.exists()) {
                throw new IllegalArgumentException();
            }
            prj = null;
        }
    }

    @After
    public void teardown() throws CoreException {
        final IWorkspace w = ResourcesPlugin.getWorkspace();
        final IProject p = w.getRoot().getProject(name);
        if (p.exists()) {
            p.delete(true, null);
        }
    }

    @Test
    public void createSimpleProject() throws CoreException {
        final NewProjectData info = new NewProjectData(factory);
        info.copyFrom(ErlangProjectProperties.DEFAULT);
        creator = new ProjectCreator(name, location, new IProject[] {}, info, context,
                null);
        prj = creator.createProject();
        assertThat(prj, is(not(nullValue())));

        final IErlProject erlPrj = ErlangEngine.getInstance().getModel().findProject(prj);
        assertThat(erlPrj, is(not(nullValue())));

        final ErlangProjectProperties props = erlPrj.getProperties();
        assertThat(props.getOutputDir(), is((IPath) new Path("ebin")));

    }

    @Test(expected = CoreException.class)
    public void createExistingProjectShouldCrash() throws CoreException {
        final NewProjectData info = new NewProjectData(factory);
        info.copyFrom(ErlangProjectProperties.DEFAULT);
        creator = new ProjectCreator(name, location, new IProject[] {}, info, context,
                null);
        prj = creator.createProject();
        prj = creator.createProject();
    }
}
