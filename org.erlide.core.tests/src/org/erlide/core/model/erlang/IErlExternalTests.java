package org.erlide.core.model.erlang;

import static org.junit.Assert.*;

import java.io.File;
import java.util.List;

import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElement.Kind;
import org.erlide.core.model.root.IErlExternal;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class IErlExternalTests extends ErlModelTestBase {

    private static final String XX_ERLIDEX = "xx.erlidex";
    private File externalFile;
    private File externalsFile;
    private String externalModulesString;

    @Before
    @Override
    public void setUp() throws Exception {
        super.setUp();
        externalModulesString = project.getExternalModulesString();
        final String externalFileName = "external.erl";
        externalFile = ErlideTestUtils.createTmpFile(externalFileName,
                "-module(external).\nf([_ | _]=L ->\n    atom_to_list(L).\n");
        final String absolutePath = externalFile.getAbsolutePath();
        externalsFile = ErlideTestUtils.createTmpFile(XX_ERLIDEX, absolutePath);
        project.setExternalModulesFile(externalsFile.getAbsolutePath());
        project.open(null);
    }

    @After
    @Override
    public void tearDown() throws Exception {
        if (externalFile != null && externalFile.exists()) {
            externalFile.delete();
        }
        if (externalsFile != null && externalsFile.exists()) {
            externalsFile.delete();
        }
        project.setExternalModulesFile(externalModulesString);
        super.tearDown();
    }

    // boolean isOTP();
    @Test
    public void isOTP() throws Exception {
        final List<IErlElement> externals = project
                .getChildrenOfKind(Kind.EXTERNAL);
        final IErlExternal external = (IErlExternal) externals.get(0);
        external.open(null);
        final IErlExternal external2 = (IErlExternal) externals.get(1);
        external2.open(null);
        final IErlExternal external3 = (IErlExternal) external.getChildren()
                .get(0);
        final IErlExternal external4 = (IErlExternal) external2.getChildren()
                .get(0);
        assertFalse(external.isOTP());
        assertTrue(external2.isOTP());
        assertFalse(external3.isOTP());
        assertTrue(external4.isOTP());
    }

    // boolean hasIncludes();
    @Test
    public void hasIncludes() throws Exception {
        final List<IErlElement> externals = project
                .getChildrenOfKind(Kind.EXTERNAL);
        final IErlExternal external = (IErlExternal) externals.get(0);
        external.open(null);
        final IErlExternal externalOTP = (IErlExternal) externals.get(1);
        externalOTP.open(null);
        final IErlExternal externalDialyzer = (IErlExternal) externalOTP
                .getChildNamed("dialyzer");
        assertTrue(external.hasIncludes());
        assertTrue(externalOTP.hasIncludes());
        assertFalse(externalDialyzer.hasIncludes());
    }

}
