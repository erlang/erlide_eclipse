package org.erlide.core.model.erlang;

import java.util.List;

import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElement.Kind;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.Before;

public class ErlModelTestBase2 extends ErlModelTestBase {

    protected static final String MAKE_A_TUPLE = "make a tuple";
    protected IErlModule module2;
    protected IErlFunction functionA;
    protected IErlFunction functionB;
    protected IErlFunction functionC;

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

}
