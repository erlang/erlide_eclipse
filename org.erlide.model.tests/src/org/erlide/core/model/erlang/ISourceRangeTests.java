package org.erlide.core.model.erlang;

import static org.junit.Assert.*;

import org.erlide.core.model.root.IErlElement;
import org.junit.Before;
import org.junit.Test;

public class ISourceRangeTests extends ErlModelTestBase {

    private ISourceRange sourceRange;
    private ISourceRange sourceRange2;

    @Override
    @Before
    public void setUp() throws Exception {
        super.setUp();
        module.open(null);
        final IErlElement element = module.getElementAtLine(0);
        final IErlAttribute attribute = (IErlAttribute) element;
        sourceRange = attribute.getSourceRange();
        final IErlElement element2 = module.getElementAtLine(2);
        final IErlFunction function = (IErlFunction) element2;
        sourceRange2 = function.getSourceRange();
    }

    // int getLength();
    @Test
    public void getLength() throws Exception {
        assertEquals(12, sourceRange.getLength());
        assertEquals(29, sourceRange2.getLength());
    }

    // int getOffset();
    @Test
    public void getOffset() throws Exception {
        assertEquals(0, sourceRange.getOffset());
        assertEquals(33, sourceRange2.getOffset());
    }

    // boolean hasPosition(int position);
    @Test
    public void hasPosition() throws Exception {
        assertTrue(sourceRange.hasPosition(0));
        final int offset = sourceRange.getOffset() + sourceRange.getLength()
                - 1;
        final int offset2 = sourceRange.getOffset() + sourceRange.getLength();
        final int offset3 = sourceRange.getOffset() + sourceRange.getLength()
                + 1;
        assertTrue(sourceRange.hasPosition(offset));
        assertTrue(sourceRange.hasPosition(offset2));
        assertFalse(sourceRange.hasPosition(offset3));
    }

}
