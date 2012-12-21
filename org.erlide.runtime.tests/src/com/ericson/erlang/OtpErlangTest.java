package com.ericson.erlang;

import static org.junit.Assert.*;

import java.util.Arrays;

import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangString;

public class OtpErlangTest {

    @Test
    public void checkOffsetByCodepointBug() {
        final String s = "abcdefg";
        final String ss = s.substring(3, 6);
        final int[] cps = OtpErlangString.stringToCodePoints(ss);
        final int[] expect = new int[] { 100, 101, 102 };
        assertEquals(Arrays.toString(expect), Arrays.toString(cps));
    }
}
