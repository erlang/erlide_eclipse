package org.erlide.core.model.erlang;

import static org.junit.Assert.*;

import org.erlide.core.model.root.IErlElement;
import org.junit.Test;

public class IErlMemberTests extends ErlModelTestBase {

    // void setNameRange(int offset, int length);
    @Test
    public void setNameRange() throws Exception {
        module.open(null);
        final IErlElement element = module.getElementAt(1);
        final IErlAttribute attribute = (IErlAttribute) element;
        final int offset = 1, length = 2;
        attribute.setNameRange(offset, length);
        final ISourceRange nameRange = attribute.getNameRange();
        assertEquals(offset, nameRange.getOffset());
        assertEquals(length, nameRange.getLength());
    }

    // ISourceRange getNameRange();
    @Test
    public void getNameRange() throws Exception {
        module.open(null);
        final IErlElement element = module.getElementAt(1);
        final IErlAttribute attribute = (IErlAttribute) element;
        final ISourceRange nameRange = attribute.getNameRange();
        assertEquals(0, nameRange.getOffset());
        assertEquals(12, nameRange.getLength());
    }
}
