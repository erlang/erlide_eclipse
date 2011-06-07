package org.erlide.core.common;

import junit.framework.Assert;

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

}
