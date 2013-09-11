package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.junit.Test;

public class IErlExportTests extends ErlModelTestBase2 {

    // public boolean hasFunction(final ErlangFunction f);
    @Test
    public void hasFunction() throws Exception {
        module.open(null);
        final List<IErlElement> childrenOfKind = module2
                .getChildrenOfKind(ErlElementKind.EXPORT);
        final IErlElement element = childrenOfKind.get(0);
        final IErlExport export = (IErlExport) element;
        assertTrue(export.hasFunction(functionA.getFunction()));
        assertTrue(export.hasFunction(functionB.getFunction()));
        assertFalse(export.hasFunction(functionC.getFunction()));
    }

}
