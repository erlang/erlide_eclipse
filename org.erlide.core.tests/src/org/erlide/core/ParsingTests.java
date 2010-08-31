/**
 * 
 */
package org.erlide.core;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;

import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.internal.ErlParser;
import org.erlide.core.erlang.internal.TestingSupport;
import org.erlide.core.text.ErlangToolkit;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import erlang.ErlideScanner;

/**
 * @author jakob
 * 
 */
public class ParsingTests {

    IErlModule module;

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        module = ErlangCore.getModelManager().getModuleFromText("testing", "",
                null);
    }

    /**
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
        module.dispose();
        module = null;
    }

    private boolean parse(final String s) {
        final String scannerModuleName = ErlangToolkit
                .createScannerModuleName(module);
        ErlideScanner.initialScan(scannerModuleName, "", s, false);
        return ErlParser.parse(module, false, "", false);
    }

    @Test
    public void parseCompileDirective() {
        assertTrue(parse("-compile([inline,{hipe,[{regalloc,linear_scan}]}])."));
        try {
            final IErlElement attribute = TestingSupport.createErlAttribute(
                    module, "compile", null,
                    "[inline,{hipe,[{regalloc,linear_scan}]}]", 0, 50);
            final List<IErlElement> expected = new ArrayList<IErlElement>(1);
            expected.add(attribute);
            final List<IErlElement> actual = module.getChildren();
            assertEquals(expected, actual);
            assertEquals(expected.toString(), actual.toString());
        } catch (final ErlModelException e) {
            fail("ErlModelException" + e.getMessage());
        }
    }
}
