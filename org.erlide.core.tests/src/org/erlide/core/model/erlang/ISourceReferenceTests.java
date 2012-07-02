package org.erlide.core.model.erlang;

import static org.junit.Assert.*;

import org.erlide.core.model.root.IErlElement;
import org.junit.Test;

public class ISourceReferenceTests extends ErlModelTestBase {

    // ISourceRange getSourceRange() throws ErlModelException;
    @Test
    public void getSourceRange() throws Exception {
        module.open(null);
        final IErlElement element = module.getElementAtLine(0);
        final ISourceReference sourceReference = (ISourceReference) element;
        final ISourceRange sourceRange = sourceReference.getSourceRange();
        final IErlElement element2 = module.getElementAtLine(1);
        final ISourceReference sourceReference2 = (ISourceReference) element2;
        final ISourceRange sourceRange2 = sourceReference2.getSourceRange();
        assertEquals(0, sourceRange.getOffset());
        assertEquals(13, sourceRange2.getOffset());
    }

    // public int getLineStart();
    // public int getLineEnd();
    @Test
    public void getLineX() throws Exception {
        module.open(null);
        final IErlElement element = module.getElementAtLine(0);
        final ISourceReference sourceReference = (ISourceReference) element;
        final IErlElement element2 = module.getElementAtLine(1);
        final ISourceReference sourceReference2 = (ISourceReference) element2;
        assertEquals(0, sourceReference.getLineStart());
        assertEquals(0, sourceReference.getLineEnd());
        assertEquals(1, sourceReference2.getLineStart());
        assertEquals(1, sourceReference2.getLineEnd());
    }

}
