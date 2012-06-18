package org.erlide.core.model.erlang;

import static org.junit.Assert.*;

import java.util.List;

import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElement.Kind;
import org.junit.Test;

public class IErlExportTests extends ErlModelTestBase2 {

    // public boolean hasFunction(final ErlangFunction f);
    @Test
    public void hasFunction() throws Exception {
        module.open(null);
        final List<IErlElement> childrenOfKind = module2
                .getChildrenOfKind(Kind.EXPORT);
        final IErlElement element = childrenOfKind.get(0);
        final IErlExport export = (IErlExport) element;
        assertTrue(export.hasFunction(functionA.getFunction()));
        assertTrue(export.hasFunction(functionB.getFunction()));
        assertFalse(export.hasFunction(functionC.getFunction()));
    }

}
