package org.erlide.jinterface.java;

import junit.framework.Assert;

import org.erlide.jinterface.util.ParserException;
import org.erlide.jinterface.util.TermParser;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpFormatPlaceholder;
import com.ericsson.otp.erlang.OtpOutputStream;
import com.ericsson.otp.erlang.OtpPatternVariable;

public class JInterfaceTest {

    @Test
    public void atom_1() throws ParserException {
        final OtpErlangAtom r = (OtpErlangAtom) TermParser.parse("hello");
        Assert.assertEquals(r.atomValue(), "hello");
    }

    @Test
    public void atom_2() throws ParserException {
        final OtpErlangAtom r = (OtpErlangAtom) TermParser.parse("hello   ");
        Assert.assertEquals(r.atomValue(), "hello");
    }

    @Test
    public void atom_3() throws ParserException {
        final OtpErlangAtom r = (OtpErlangAtom) TermParser.parse("   hello");
        Assert.assertEquals(r.atomValue(), "hello");
    }

    @Test
    public void number_1() throws ParserException {
        final OtpErlangLong r = (OtpErlangLong) TermParser.parse("321");
        Assert.assertEquals(r.longValue(), 321);
    }

    @Test
    public void var_1() throws ParserException {
        final OtpPatternVariable r = (OtpPatternVariable) TermParser
                .parse("Hello");
        Assert.assertEquals(r.getName(), "Hello");
    }

    @Test
    public void var_2() throws ParserException {
        final OtpPatternVariable r = (OtpPatternVariable) TermParser.parse("_");
        Assert.assertEquals(r.getName(), "_");
    }

    @Test
    public void string_1() throws ParserException {
        final OtpErlangString r = (OtpErlangString) TermParser
                .parse("\"Hello\"");
        Assert.assertEquals(r.stringValue(), "Hello");
    }

    @Test
    public void string_2() throws ParserException {
        final OtpErlangString r = (OtpErlangString) TermParser
                .parse("\"Hello world!\"");
        Assert.assertEquals(r.stringValue(), "Hello world!");
    }

    @Test
    public void placeholder_1() throws ParserException {
        final OtpFormatPlaceholder r = (OtpFormatPlaceholder) TermParser
                .parse("~hello");
        Assert.assertEquals(r.getName(), "hello");
    }

    @Test
    public void list_1() throws ParserException {
        final OtpErlangList r = (OtpErlangList) TermParser.parse("[]");
        Assert.assertEquals("[]", r.toString());
    }

    @Test
    public void list_2() throws ParserException {
        final OtpErlangList r = (OtpErlangList) TermParser.parse("[a,2,b,4]");
        Assert.assertEquals("[a,2,b,4]", r.toString());
    }

    @Test
    public void list_3() throws ParserException {
        final OtpErlangList r = (OtpErlangList) TermParser.parse("[a,2,b|4]");
        Assert.assertEquals("[a,2,b|4]", r.toString());
    }

    @Test
    public void list_4() throws ParserException {
        final OtpErlangList r = (OtpErlangList) TermParser
                .parse("[ax , [ 2  , b ], 4 ,5]");
        Assert.assertEquals("[ax,[2,b],4,5]", r.toString());
    }

    @Test
    public void sublist_1() throws ParserException {
        final OtpErlangList r = (OtpErlangList) TermParser.parse("[a,2,b,4]");
        final OtpErlangList s = (OtpErlangList) TermParser.parse("[2,b,4]");
        final OtpErlangObject ss = r.getTail();
        Assert.assertEquals(s, ss);
    }

    @Test
    public void sublist_2() throws ParserException {
        final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3|4]");
        final OtpErlangList s = (OtpErlangList) TermParser.parse("[2,3|4]");
        final OtpErlangObject ss = r.getTail();
        Assert.assertEquals(s, ss);
    }

    @Test
    public void sublist_4() throws ParserException {
        final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3,4]");
        final OtpErlangList s = (OtpErlangList) TermParser.parse("[3,4]");
        final OtpErlangObject ss = r.getNthTail(2);
        Assert.assertEquals(s, ss);
    }

    @Test
    public void sublist_3() throws ParserException {
        final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3|4]");
        final OtpErlangList s = (OtpErlangList) TermParser.parse("[3|4]");
        final OtpErlangObject ss = r.getNthTail(2);
        Assert.assertEquals(s, ss);
    }

    @Test
    public void sublist_4a() throws ParserException {
        final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3,4]");
        final OtpErlangObject ss = r.getNthTail(0);
        Assert.assertEquals(r, ss);
    }

    @Test
    public void sublist_4b() throws ParserException {
        final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3,4]");
        final OtpErlangObject ss = r.getNthTail(4);
        Assert.assertEquals(new OtpErlangList(), ss);
    }

    @Test
    public void sublist_4c() throws ParserException {
        final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3,4]");
        final OtpErlangObject ss = r.getNthTail(5);
        Assert.assertEquals(null, ss);
    }

    @Test
    public void sublist_4d() throws ParserException {
        final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3|4]");
        final OtpErlangObject s = TermParser.parse("4");
        final OtpErlangObject ss = r.getNthTail(3);
        Assert.assertEquals(s, ss);
    }

    @Test
    public void sublist_5() throws ParserException {
        final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3,4]");
        final OtpErlangObject ss = r.getNthTail(2);
        final OtpOutputStream out = new OtpOutputStream();
        ss.encode(out);
        final byte[] contents1 = out.toByteArray();
        out.reset();
        final OtpErlangList s = (OtpErlangList) TermParser.parse("[3,4]");
        s.encode(out);
        final byte[] contents2 = out.toByteArray();

        Assert.assertEquals(contents2.length, contents1.length);
        for (int i = 0; i < contents1.length; i++) {
            Assert.assertEquals(contents2[i], contents1[i]);
        }
    }

    @Test
    public void sublist_6() throws ParserException {
        final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3|4]");
        final OtpErlangObject ss = r.getNthTail(2);
        final OtpOutputStream out = new OtpOutputStream();
        ss.encode(out);
        final byte[] contents1 = out.toByteArray();
        out.reset();
        final OtpErlangList s = (OtpErlangList) TermParser.parse("[3|4]");
        s.encode(out);
        final byte[] contents2 = out.toByteArray();

        Assert.assertEquals(contents2.length, contents1.length);
        for (int i = 0; i < contents1.length; i++) {
            Assert.assertEquals(contents2[i], contents1[i]);
        }
    }
}
