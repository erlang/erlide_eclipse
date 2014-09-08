package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.List;

import org.junit.Test;

public class IErlFunctionTests extends ErlModelTestBase2 {

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
    public void getComments() throws Exception {
        final Collection<IErlComment> comments = functionA.getComments();
        final Collection<IErlComment> comments2 = functionB.getComments();
        final Collection<IErlComment> comments3 = functionC.getComments();
        assertTrue(comments.isEmpty());
        assertTrue(comments2.isEmpty());
        assertFalse(comments3.isEmpty());
        assertEquals(ErlModelTestBase2.MAKE_A_TUPLE, comments3.iterator().next()
                .getSource());
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
