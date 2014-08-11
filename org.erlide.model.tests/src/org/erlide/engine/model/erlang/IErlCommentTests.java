package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.util.ErlideTestUtils;
import org.junit.Before;
import org.junit.Test;

public class IErlCommentTests extends ErlModelTestBase {

    private IErlModule module2;
    private IErlModule module4;
    private IErlModule module5;

    @Before
    @Override
    public void setUp() throws Exception {
        super.setUp();
        module2 = ErlideTestUtils.createModule(project, "ww.erl",
                "% header\n-module(ww).\n% comment\n");
        module2.open(null);
        module4 = ErlideTestUtils.createModule(project, "ii.erl",
                "-module(ii).\n% fn\n-spec fn() ->ok.\nfn() ->\n    ok.\n");
        module4.open(null);
        module5 = ErlideTestUtils.createModule(project, "jj.erl",
                "-module(jj).\n% a\na()->\n    ok.\n-spec fn() ->ok."
                        + "\n% fn\nfn() ->\n    ok.\n");
        module5.open(null);
    }

    // boolean isHeader();
    @Test
    public void isHeader() throws Exception {
        final IErlModule module3 = ErlideTestUtils.createModule(project, "uu.erl",
                "-module(uu).\n% header too\n\n% comment\n");
        module3.open(null);
        final Collection<IErlComment> comments = module2.getComments();
        final Iterator<IErlComment> iterator = comments.iterator();
        final IErlComment comment = iterator.next();
        final IErlComment comment2 = iterator.next();
        final Collection<IErlComment> comments2 = module3.getComments();
        final Iterator<IErlComment> iterator2 = comments2.iterator();
        final IErlComment comment3 = iterator2.next();
        final IErlComment comment4 = iterator2.next();
        assertTrue(comment.isHeader());
        assertFalse(comment2.isHeader());
        assertTrue(comment3.isHeader());
        assertFalse(comment4.isHeader());
    }

    // Kind IErlElement#getKind()
    @Test
    public void getKind() throws Exception {
        final Collection<IErlComment> comments = module2.getComments();
        final Iterator<IErlComment> iterator = comments.iterator();
        final IErlComment comment = iterator.next();
        final IErlComment comment2 = iterator.next();
        assertEquals(ErlElementKind.COMMENT, comment.getKind());
        assertEquals(ErlElementKind.COMMENT, comment2.getKind());
    }

    @Test
    public void functionSpecIsComment() throws Exception {
        final List<IErlElement> functions = module4
                .getChildrenOfKind(ErlElementKind.FUNCTION);
        final IErlFunction fn = (IErlFunction) functions.get(0);
        final Iterator<IErlComment> iterator = fn.getComments().iterator();
        final IErlMember comment1 = iterator.next();
        assertNotNull(comment1);
        assertFalse(iterator.hasNext());
    }

    @Test
    public void functionSpecIsComment2() throws Exception {
        final List<IErlElement> functions = module5
                .getChildrenOfKind(ErlElementKind.FUNCTION);
        IErlFunction fn = (IErlFunction) functions.get(0);
        Iterator<IErlComment> iterator = fn.getComments().iterator();
        IErlMember comment1 = iterator.next();
        assertNotNull(comment1);
        assertFalse(iterator.hasNext());
        fn = (IErlFunction) functions.get(1);
        iterator = fn.getComments().iterator();
        comment1 = iterator.next();
        assertNotNull(comment1);
        assertFalse(iterator.hasNext());
    }

}
