package org.erlide.jinterface.backend;

import static org.junit.Assert.assertEquals;

import org.erlide.core.common.Util;
import org.junit.Test;

public class UtilTest {
    @Test
    public void normalizeSpaces_shouldKeepSingleSpaces() {
        final String input = "a b c";
        final String value = Util.normalizeSpaces(input);
        final String expected = "a b c";
        assertEquals(value, expected);
    }

    @Test
    public void normalizeSpaces_shouldCompressMultipleSpaces() {
        final String input = "a   b     c";
        final String value = Util.normalizeSpaces(input);
        final String expected = "a b c";
        assertEquals(value, expected);
    }

    @Test
    public void normalizeSpaces_shouldCompressTabs() {
        final String input = "a\t\tb\tc";
        final String value = Util.normalizeSpaces(input);
        final String expected = "a b c";
        assertEquals(value, expected);
    }

    @Test
    public void normalizeSpaces_shouldCompressNewlines() {
        final String input = "a\r\nb\nc";
        final String value = Util.normalizeSpaces(input);
        final String expected = "a b c";
        assertEquals(value, expected);
    }

    @Test
    public void normalizeSpaces_shouldCompressAll() {
        final String input = "a\r\n\t   b\n\t\t  \tc";
        final String value = Util.normalizeSpaces(input);
        final String expected = "a b c";
        assertEquals(value, expected);
    }
}
