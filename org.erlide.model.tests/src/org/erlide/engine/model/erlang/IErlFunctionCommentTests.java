package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;

import org.erlide.engine.util.ErlideTestUtils;
import org.junit.Test;

public class IErlFunctionCommentTests extends ErlModelTestBase {

    /**
     * http://www.assembla.com/spaces/erlide/tickets/891-wrong-function-comment-
     * in-edoc-view-and-hover
     */
    @Test
    public void functionCommentsOnlyTopLevel() throws Exception {
        final String s = "f1()->\n    %some comment here \n    foo:bar().\n"
                + "f2() ->\n    ok.";
        final IErlModule myModule = ErlideTestUtils.createModule(project, "w.erl", s);
        myModule.open(null);
        final IErlFunction function = (IErlFunction) myModule.getChildNamed("f2");
        assertEquals(0, function.getComments().size());
    }
}
