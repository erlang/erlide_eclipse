package org.erlide.core.model.erlang;

import static org.junit.Assert.*;

import java.util.Collection;
import java.util.List;

import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElement.Kind;
import org.erlide.core.model.util.ErlangFunction;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.Before;
import org.junit.Test;

public class IErlImportExportTests extends ErlModelTestBase {

    private IErlModule module2;
    private List<IErlElement> imports;
    private List<IErlElement> exports;
    private IErlImport import1;
    private IErlExport export;

    @Before
    @Override
    public void setUp() throws Exception {
        super.setUp();
        module2 = ErlideTestUtils.createModule(project, "zz.erl",
                "-module(zz).\n-export([f/2, f/0]).\n"
                        + "-import(lists, [foldl/3, reverse/1, reverse/2]).\n");
        module2.open(null);
        imports = module2.getChildrenOfKind(Kind.IMPORT);
        exports = module2.getChildrenOfKind(Kind.EXPORT);
        import1 = (IErlImport) imports.get(0);
        export = (IErlExport) exports.get(0);
    }

    // public boolean hasFunction(final ErlangFunction f);
    @Test
    public void hasFunction() throws Exception {
        assertTrue(import1.hasFunction(new ErlangFunction("reverse", 1)));
        assertTrue(import1.hasFunction(new ErlangFunction("reverse", 2)));
        assertTrue(import1.hasFunction(new ErlangFunction("reverse",
                ErlangFunction.ANY_ARITY)));
        assertFalse(import1.hasFunction(new ErlangFunction("reverse", 3)));
        assertTrue(export.hasFunction(new ErlangFunction("f", 0)));
        assertTrue(export.hasFunction(new ErlangFunction("f",
                ErlangFunction.ANY_ARITY)));
        assertFalse(export.hasFunction(new ErlangFunction("f", 1)));
    }

    // public Collection<ErlangFunction> getFunctions();
    @Test
    public void getFunctions() throws Exception {
        final Collection<ErlangFunction> functions = import1.getFunctions();
        final ErlangFunction function = functions.iterator().next();
        final Collection<ErlangFunction> functions2 = export.getFunctions();
        final ErlangFunction function2 = functions2.iterator().next();
        assertEquals(3, functions.size());
        assertEquals(new ErlangFunction("foldl", 3), function);
        assertEquals(2, functions2.size());
        assertEquals(new ErlangFunction("f", 2), function2);
    }

}
