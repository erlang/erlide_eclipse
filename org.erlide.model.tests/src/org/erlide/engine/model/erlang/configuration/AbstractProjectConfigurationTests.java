package org.erlide.engine.model.erlang.configuration;

import org.erlide.engine.internal.model.root.ErlProject;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.util.ErlideTestUtils;
import org.junit.After;
import org.junit.Before;

public class AbstractProjectConfigurationTests {

    protected IErlProject project;

    @Before
    public void setUp() throws Exception {
        ErlideTestUtils.initProjects();
        final String name = "testproject3";
        project = ErlideTestUtils.createProject(ErlideTestUtils.getTmpPath(name), name);
        final ErlProject p = (ErlProject) project;
        p.loadCoreProperties();
    }

    @After
    public void tearDown() throws Exception {
        ErlideTestUtils.deleteProjects();
    }

}
