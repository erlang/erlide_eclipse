/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface;

import static org.junit.Assert.*;

import java.math.BigInteger;
import java.util.Arrays;

import junit.framework.Assert;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangFloat;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.Signature;
import com.ericsson.otp.erlang.SignatureException;

public class TypeConverterTest {

    private void test(final Object o, final String sig,
            final OtpErlangObject expect) throws SignatureException {
        final OtpErlangObject result = TypeConverter.java2erlang(o, sig);
        assertTrue(expect.equals(result));
    }

    @Before
    public void setUp() {
        Signature.setUseCache(false);
    }

    @After
    public void tearDown() {
        Signature.setUseCache(true);
    }

    @SuppressWarnings("boxing")
    @Test
    public void cvtIntegerOk_1() throws SignatureException {
        test(397, "i", new OtpErlangLong(397));
    }

    @SuppressWarnings("boxing")
    @Test
    public void cvtIntegerOk_2() throws SignatureException {
        test(3, "x", new OtpErlangLong(3));
    }

    @Test
    public void cvtIntegerOk_3() throws SignatureException {
        final BigInteger bigInteger = new BigInteger("39799999999999999999999",
                10);
        test(bigInteger, "i", new OtpErlangLong(bigInteger));
    }

    @SuppressWarnings("boxing")
    @Test(expected = SignatureException.class)
    public void cvtIntegerFail_1() throws SignatureException {
        test(3, "s", new OtpErlangLong(3));
    }

    @Test
    public void cvtListOk_1() throws SignatureException {
        test(new String[] { "a" }, "ls", new OtpErlangList(
                new OtpErlangObject[] { new OtpErlangString("a") }));
    }

    @SuppressWarnings("boxing")
    @Test
    public void cvtListOk_2() throws SignatureException {
        test(new Object[] { "a", 35 }, "lx", new OtpErlangList(
                new OtpErlangObject[] { new OtpErlangString("a"),
                        new OtpErlangLong(35) }));
    }

    @Test(expected = SignatureException.class)
    public void cvtListFail_1() throws SignatureException {
        test(new String[] { "a" }, "li", new OtpErlangList(
                new OtpErlangObject[] { new OtpErlangString("a") }));
    }

    @SuppressWarnings("boxing")
    @Test(expected = SignatureException.class)
    public void cvtListFail_2() throws SignatureException {
        test(new Object[] { "a", 35 }, "ls", new OtpErlangList(
                new OtpErlangObject[] { new OtpErlangString("a"),
                        new OtpErlangLong(35) }));
    }

    @Test
    public void cvtStringOk_1() throws SignatureException {
        test("astring", "a", new OtpErlangAtom("astring"));
    }

    @Test
    public void cvtStringOk_2() throws SignatureException {
        test("astring", "s", new OtpErlangString("astring"));
    }

    @Test
    public void cvtStringOk_4() throws SignatureException {
        test("", "s", new OtpErlangString(""));
    }

    @Test
    public void cvtStringOk_3() throws SignatureException {
        test("astring", "b", new OtpErlangBinary("astring".getBytes()));
    }

    @Test(expected = SignatureException.class)
    public void cvtStringFail_1() throws SignatureException {
        test("astring", "p", new OtpErlangString("astring"));
    }

    @SuppressWarnings("boxing")
    @Test
    public void cvtFloatOk_1() throws SignatureException {
        test(3.14f, "d", new OtpErlangFloat(3.14f));
    }

    @SuppressWarnings("boxing")
    @Test
    public void cvtDoubleOk_1() throws SignatureException {
        test(3.14d, "d", new OtpErlangDouble(3.14d));
    }

    @Test
    public void parseSignature_0() throws SignatureException {
        final String sig = null;
        final Signature[] result = Signature.parse(sig);
        assertTrue(result == null);
    }

    @Test
    public void parseSignature_1() throws SignatureException {
        final String sig = "aslsilpfd";
        final Signature[] result = Signature.parse(sig);
        final String expect = "[a, s, l(s), i, l(p), f, d]";
        Assert.assertEquals(expect, Arrays.toString(result));
    }

    @Test
    public void parseSignature_2() throws SignatureException {
        final String sig = "llxi";
        final Signature[] result = Signature.parse(sig);
        final String expect = "[l(l(x)), i]";
        assertTrue(Arrays.toString(result).equals(expect));
    }

    @Test
    public void parseSignature_3() throws SignatureException {
        final String sig = "2axd";
        final Signature[] result = Signature.parse(sig);
        final String expect = "[t(a,x), d]";
        assertTrue(Arrays.toString(result).equals(expect));
    }

    @Test
    public void parseSignature_4() throws SignatureException {
        final String sig = "l3axl2sad";
        final Signature[] result = Signature.parse(sig);
        final String expect = "[l(t(a,x,l(t(s,a)))), d]";
        assertTrue(Arrays.toString(result).equals(expect));
    }

    @Test
    public void parseSignature_5() throws SignatureException {
        final String sig = "32sadax";
        final Signature[] result = Signature.parse(sig);
        final String expect = "[t(t(s,a),d,a), x]";
        assertTrue(Arrays.toString(result).equals(expect));
    }

    @SuppressWarnings("boxing")
    @Test
    public void cvtBoolOk_1() throws SignatureException {
        test(true, "o", new OtpErlangAtom("true"));
    }

    @SuppressWarnings("boxing")
    @Test
    public void cvtBoolOk_2() throws SignatureException {
        test(false, "o", new OtpErlangAtom("false"));
    }

    @SuppressWarnings("boxing")
    @Test(expected = SignatureException.class)
    public void cvtBoolFail_1() throws SignatureException {
        test(true, "i", new OtpErlangAtom("true"));
    }

    @Test
    public void cvtConvertible_1() throws SignatureException {
        final IConvertible x = new IConvertible() {
            @Override
            public OtpErlangObject toErlangObject() {
                return new OtpErlangAtom("__kalle__");
            }
        };
        test(x, "j", new OtpErlangAtom("__kalle__"));
    }

    static class Cvt {
        public static String fromErlangObject(final OtpErlangObject obj) {
            return "hej";
        }
    }

    @Test
    public void cvtConvertible_2() throws SignatureException {
        final Object x = "hej";
        final OtpErlangObject obj = new OtpErlangAtom("hej d�");
        final Object y = TypeConverter.erlang2java(obj, Cvt.class);
        assertTrue(x.equals(y));
    }

}
