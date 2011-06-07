package org.erlide.core.common;

import junit.framework.Assert;

import org.erlide.jinterface.util.TermParser;
import org.erlide.jinterface.util.TermParserException;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public class UtilTest {

    @Test
    public void testIoListToString_small() {
        final OtpErlangObject input = OtpErlang.mkList(new OtpErlangString(
                "hej"), new OtpErlangString("hoj"));
        final String result = Util.ioListToString(input, 10);
        final String expected = "hejhoj";
        Assert.assertEquals(expected, result);
    }

    @Test
    public void testIoListToString_large1() {
        final OtpErlangObject input = OtpErlang.mkList(new OtpErlangString(
                "hej"), new OtpErlangString("hoj"));
        final String result = Util.ioListToString(input, 4);
        final String expected = "hejh... <truncated>";
        Assert.assertEquals(expected, result);
    }

    @Test
    public void testIoListToString_large2() {
        final OtpErlangObject input = OtpErlang.mkList(new OtpErlangString(
                "hej"), new OtpErlangString("hoj"));
        final String result = Util.ioListToString(input, 3);
        final String expected = "hej... <truncated>";
        Assert.assertEquals(expected, result);
    }

    @Test
    public void testIsTag_number() throws TermParserException {
        final OtpErlangObject input = TermParser.getParser().parse("3");
        Assert.assertEquals(false, Util.isTag(input, "ok"));
    }

    @Test
    public void testIsTag_good_atom() throws TermParserException {
        final OtpErlangObject input = TermParser.getParser().parse("ok");
        Assert.assertEquals(true, Util.isTag(input, "ok"));
    }

    @Test
    public void testIsTag_wrong_atom() throws TermParserException {
        final OtpErlangObject input = TermParser.getParser().parse("okx");
        Assert.assertEquals(false, Util.isTag(input, "ok"));
    }

    @Test
    public void testIsTag_tuple_int() throws TermParserException {
        final OtpErlangObject input = TermParser.getParser().parse("{3,9}");
        Assert.assertEquals(false, Util.isTag(input, "ok"));
    }

    @Test
    public void testIsTag_tuple_good_atom() throws TermParserException {
        final OtpErlangObject input = TermParser.getParser().parse("{ok, 9}");
        Assert.assertEquals(true, Util.isTag(input, "ok"));
    }

    @Test
    public void testIsTag_tuple_wrong_atom() throws TermParserException {
        final OtpErlangObject input = TermParser.getParser().parse("{okx, 9}");
        Assert.assertEquals(false, Util.isTag(input, "ok"));
    }
}
