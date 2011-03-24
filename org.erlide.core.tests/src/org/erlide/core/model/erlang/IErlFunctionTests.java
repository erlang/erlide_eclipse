package org.erlide.core.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.erlide.core.model.erlang.IErlElement.Kind;
import org.erlide.core.model.erlang.util.ErlangFunction;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.Before;
import org.junit.Test;

public class IErlFunctionTests extends ErlModelTestBase {

    private static final String MAKE_A_TUPLE = "make a tuple";
    IErlModule module2;
    private IErlFunction functionA;
    private IErlFunction functionB;
    private IErlFunction functionC;

    @Override
    @Before
    public void setUp() throws Exception {
        super.setUp();
        module2 = ErlideTestUtils.createModule(project, "yy.erl",
                "-module(yy).\n-export([a/1, b/0]).\n"
                        + "a(L) when is_list(L)->\n    lists:reverse(L);\n"
                        + "a(A) when is_atom(A)->\n    atom_to_list(A).\n"
                        + "b() ->\n    b.\n% " + MAKE_A_TUPLE + "\n"
                        + "c(A, B, C) ->\n    {c, A, B, C}.\n");
        module2.open(null);
        final List<IErlElement> functions = module2
                .getChildrenOfKind(Kind.FUNCTION);
        functionA = (IErlFunction) functions.get(0);
        functionB = (IErlFunction) functions.get(1);
        functionC = (IErlFunction) functions.get(2);
    }

    // boolean isExported();
    @Test
    public void isExported() throws Exception {
        assertTrue(functionA.isExported());
        assertTrue(functionB.isExported());
        assertFalse(functionC.isExported());
    }

    // List<IErlFunctionClause> getClauses();
    @Test
    public void getClauses() throws Exception {
        final List<IErlFunctionClause> clauses = functionA.getClauses();
        final List<IErlFunctionClause> clauses2 = functionB.getClauses();
        assertEquals(2, clauses.size());
        assertEquals(0, clauses2.size());
    }

    // ErlangFunction getFunction();
    @Test
    public void getFunction() throws Exception {
        final ErlangFunction function = functionA.getFunction();
        final ErlangFunction function2 = functionB.getFunction();
        final ErlangFunction function3 = functionC.getFunction();
        assertEquals(new ErlangFunction("a", 1), function);
        assertEquals(new ErlangFunction("b", 0), function2);
        assertEquals(new ErlangFunction("c", 3), function3);
    }

    // String getNameWithArity();
    @Test
    public void getNameWithArity() throws Exception {
        final String nameWithArity = functionA.getNameWithArity();
        final String nameWithArity2 = functionB.getNameWithArity();
        final String nameWithArity3 = functionC.getNameWithArity();
        assertEquals("a/1", nameWithArity);
        assertEquals("b/0", nameWithArity2);
        assertEquals("c/3", nameWithArity3);
    }

    // String getNameWithParameters();
    @Test
    public void getNameWithParameters() throws Exception {
        final String nameWithParameters = functionA.getNameWithParameters();
        final String nameWithParameters2 = functionB.getNameWithParameters();
        final String nameWithParameters3 = functionC.getNameWithParameters();
        assertEquals("a(_)", nameWithParameters);
        assertEquals("b()", nameWithParameters2);
        assertEquals("c(_, _, _)", nameWithParameters3);
    }

    // String getComment();
    @Test
    public void getComment() throws Exception {
        final String comment = functionA.getComment();
        final String comment2 = functionB.getComment();
        final String comment3 = functionC.getComment();
        assertNull(comment);
        assertNull(comment2);
        assertEquals(MAKE_A_TUPLE, comment3);
    }

    // String getModuleName();
    @Test
    public void getModuleName() throws Exception {
        final String moduleName = functionA.getModuleName();
        final String moduleName2 = functionB.getModuleName();
        final String moduleName3 = functionC.getModuleName();
        final String name = module2.getName();
        assertEquals(name, moduleName);
        assertEquals(name, moduleName2);
        assertEquals(name, moduleName3);
    }
}
