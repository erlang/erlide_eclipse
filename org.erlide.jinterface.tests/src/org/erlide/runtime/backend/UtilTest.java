package org.erlide.runtime.backend;

import static org.junit.Assert.assertEquals;

import org.erlide.jinterface.backend.util.Util;
import org.junit.Test;

public class UtilTest {
	@Test
	public void normalizeSpaces_shouldKeepSingleSpaces() {
		String input = "a b c";
		String value = Util.normalizeSpaces(input);
		String expected = "a b c";
		assertEquals(value, expected);
	}

	@Test
	public void normalizeSpaces_shouldCompressMultipleSpaces() {
		String input = "a   b     c";
		String value = Util.normalizeSpaces(input);
		String expected = "a b c";
		assertEquals(value, expected);
	}

	@Test
	public void normalizeSpaces_shouldCompressTabs() {
		String input = "a\t\tb\tc";
		String value = Util.normalizeSpaces(input);
		String expected = "a b c";
		assertEquals(value, expected);
	}

	@Test
	public void normalizeSpaces_shouldCompressNewlines() {
		String input = "a\r\nb\nc";
		String value = Util.normalizeSpaces(input);
		String expected = "a b c";
		assertEquals(value, expected);
	}

	@Test
	public void normalizeSpaces_shouldCompressAll() {
		String input = "a\r\n\t   b\n\t\t  \tc";
		String value = Util.normalizeSpaces(input);
		String expected = "a b c";
		assertEquals(value, expected);
	}
}
