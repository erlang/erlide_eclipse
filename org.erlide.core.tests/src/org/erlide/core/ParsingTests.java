/**
 * 
 */
package org.erlide.core;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.TestingSupport;
import org.erlide.core.erlang.internal.ErlParser;
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
		module = ErlangCore.getModelManager().getModuleFromText(null,
				"testing", "", null);
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
	public void parseCompileDirective() throws ErlModelException {
		final String sourceContent = "[inline,{hipe,[{regalloc,linear_scan}]}]";
		final String source = "-compile(" + sourceContent + ").";
		assertTrue(parse(source));
		final IErlElement attribute = TestingSupport.createErlAttribute(module,
				"compile", null, sourceContent, 0, 50);
		final List<IErlElement> expected = new ArrayList<IErlElement>(1);
		expected.add(attribute);
		final Collection<IErlElement> actual = module.getChildren();
		assertEquals(expected, actual);
		assertEquals(expected.toString(), actual.toString());
	}
}
