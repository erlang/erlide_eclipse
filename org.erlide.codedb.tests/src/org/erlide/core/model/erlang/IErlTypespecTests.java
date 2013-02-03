package org.erlide.core.model.erlang;

import static org.junit.Assert.*;

import java.util.List;

import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElement.Kind;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.Test;

public class IErlTypespecTests extends ErlModelTestBase {

    @Test
    public void getName() throws Exception {
        final IErlModule module2 = ErlideTestUtils
                .createModule(
                        project,
                        "yy.erl",
                        "-module(yy).\n"
                                + "-spec return_error(integer(), any()) -> no_return().\n"
                                + "return_error(Line, Message) ->\n"
                                + "    throw({error, {Line, ?MODULE, Message}}).");
        module2.open(null);
        final List<IErlElement> childrenOfKind = module2
                .getChildrenOfKind(Kind.TYPESPEC);
        final IErlElement element = childrenOfKind.get(0);
        final IErlTypespec typespec = (IErlTypespec) element;
        assertEquals("return_error", typespec.getName());
    }

}
