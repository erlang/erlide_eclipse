package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.util.ErlideTestUtils;
import org.junit.Test;

public class IErlImportTests extends ErlModelTestBase {

    // public String getImportModule();
    @Test
    public void getImportModule() throws Exception {
        final IErlModule module2 = ErlideTestUtils.createModule(project, "zz.erl",
                "-module(zz).\n" + "-import(lists, [foldl/3, reverse/1, reverse/2]).\n");
        module2.open(null);
        final List<IErlElement> imports = module2
                .getChildrenOfKind(ErlElementKind.IMPORT);
        final IErlImport import1 = (IErlImport) imports.get(0);
        final String importModule = import1.getImportModule();
        assertEquals("lists", importModule);
    }

}
