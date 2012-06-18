package org.erlide.core.model.erlang;

import static org.junit.Assert.*;

import java.util.Collection;
import java.util.Iterator;

import org.erlide.core.model.root.IErlElement.Kind;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.Before;
import org.junit.Test;

public class IErlCommentTests extends ErlModelTestBase {

    private IErlModule module2;

    @Before
    @Override
    public void setUp() throws Exception {
        super.setUp();
        module2 = ErlideTestUtils.createModule(project, "ww.erl",
                "% header\n-module(ww).\n% comment\n");
        module2.open(null);
    }

    // boolean isHeader();
    @Test
    public void isHeader() throws Exception {
        final IErlModule module3 = ErlideTestUtils.createModule(project,
                "uu.erl", "-module(uu).\n% header too\n\n% comment\n");
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
        assertEquals(Kind.COMMENT, comment.getKind());
        assertEquals(Kind.COMMENT, comment2.getKind());
    }

}
