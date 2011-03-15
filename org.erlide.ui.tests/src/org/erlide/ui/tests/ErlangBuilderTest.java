package org.erlide.ui.tests;

import static org.junit.Assert.assertNotNull;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.erlide.core.model.erlang.IErlProject;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.Before;
import org.junit.Test;

public class ErlangBuilderTest {
    private IErlProject p1;

    @Before
    public void setup() {
        p1 = ErlideTestUtils.getExistingProject("p1");
    }

    @Test
    public void projectShouldBuild() throws CoreException {
        assertNotNull(p1);
        final IProject prj = (IProject) p1.getResource();
        prj.build(IncrementalProjectBuilder.CLEAN_BUILD, null);
        prj.build(IncrementalProjectBuilder.FULL_BUILD, null);
        // TODO listen for resource changes and maybe follow the progress
        // monitor too to check the result
    }

}
