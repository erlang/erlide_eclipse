/**
 *
 */
package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.services.parsing.InternalScanner;
import org.erlide.engine.services.parsing.ParserService;
import org.erlide.engine.util.TestingSupport;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

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
        final IErlModel model = ErlangEngine.getInstance().getModel();
        module = model.getModuleFromText(model, "testing", "", null);
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
        final String scannerModuleName = module.getScannerName();
        // XXX implementation detail - how to do it better?
        final InternalScanner internalScanner = (InternalScanner) ErlangEngine
                .getInstance().getSimpleScannerService();
        internalScanner.create(scannerModuleName);
        boolean result = false;
        try {
            ErlangEngine.getInstance().getScannerProviderService().get(scannerModuleName)
                    .initialScan(s, "", false);
            final ParserService parser = ErlangEngine.getInstance().getParserService();
            result = parser.parse(module, scannerModuleName, false, "", s, false);
        } finally {
            ErlangEngine.getInstance().getScannerProviderService().get(scannerModuleName)
                    .dispose();
        }
        return result;
    }

    @Test
    public void parseCompileDirective() throws ErlModelException {
        final String sourceContent = "[inline,{hipe,[{regalloc,linear_scan}]}]";
        final String source = "-compile(" + sourceContent + ").";
        assertTrue(parse(source));
        final IErlElement attribute = TestingSupport.createErlAttribute(module,
                "compile", null, sourceContent, 0, 50);
        final List<IErlElement> expected = new ArrayList<IErlElement>(1);
        expected.add(attribute);
        final Collection<IErlElement> actual = module.getChildren();
        // assertEquals(expected, actual);
        assertEquals(expected.toString(), actual.toString());
    }

}
