package org.erlide.core.model.erlang;

import static org.junit.Assert.*;

import java.util.List;

import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElement.Kind;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.Test;

public class IErlImportTests extends ErlModelTestBase {

    // public String getImportModule();
    @Test
    public void getImportModule() throws Exception {
        final IErlModule module2 = ErlideTestUtils.createModule(project,
                "zz.erl", "-module(zz).\n"
                        + "-import(lists, [foldl/3, reverse/1, reverse/2]).\n");
        module2.open(null);
        final List<IErlElement> imports = module2
                .getChildrenOfKind(Kind.IMPORT);
        final IErlImport import1 = (IErlImport) imports.get(0);
        final String importModule = import1.getImportModule();
        assertEquals("lists", importModule);
    }

}
