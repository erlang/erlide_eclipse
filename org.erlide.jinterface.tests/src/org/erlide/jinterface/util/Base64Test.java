package org.erlide.jinterface.util;

import junit.framework.Assert;

import org.erlide.utils.Base64;
import org.junit.Test;

public class Base64Test {

    @Test
    public void encode_1() {
        final String arg = "";
        final String exp = "";
        Assert.assertEquals(exp, new String(Base64.encode(arg.getBytes())));
    }

    @Test
    public void encode_2() {
        final String arg = "f";
        final String exp = "Zg==";
        Assert.assertEquals(exp, new String(Base64.encode(arg.getBytes())));
    }

    @Test
    public void encode_3() {
        final String arg = "fo";
        final String exp = "Zm8=";
        Assert.assertEquals(exp, new String(Base64.encode(arg.getBytes())));
    }

    @Test
    public void encode_4() {
        final String arg = "foo";
        final String exp = "Zm9v";
        Assert.assertEquals(exp, new String(Base64.encode(arg.getBytes())));
    }

    @Test
    public void encode_5() {
        final String arg = "foob";
        final String exp = "Zm9vYg==";
        Assert.assertEquals(exp, new String(Base64.encode(arg.getBytes())));
    }

    @Test
    public void encode_6() {
        final String arg = "fooba";
        final String exp = "Zm9vYmE=";
        Assert.assertEquals(exp, new String(Base64.encode(arg.getBytes())));
    }

    @Test
    public void encode_7() {
        final String arg = "foobar";
        final String exp = "Zm9vYmFy";
        Assert.assertEquals(exp, new String(Base64.encode(arg.getBytes())));
    }

    @Test
    public void decode_1() {
        final String arg = "";
        final String exp = "";
        Assert.assertEquals(arg, new String(Base64.decode(exp.getBytes())));
    }

    @Test
    public void decode_2() {
        final String arg = "f";
        final String exp = "Zg==";
        Assert.assertEquals(arg, new String(Base64.decode(exp.getBytes())));
    }

    @Test
    public void decode_3() {
        final String arg = "fo";
        final String exp = "Zm8=";
        Assert.assertEquals(arg, new String(Base64.decode(exp.getBytes())));
    }

    @Test
    public void decode_4() {
        final String arg = "foo";
        final String exp = "Zm9v";
        Assert.assertEquals(arg, new String(Base64.decode(exp.getBytes())));
    }

    @Test
    public void decode_5() {
        final String arg = "foob";
        final String exp = "Zm9vYg==";
        Assert.assertEquals(arg, new String(Base64.decode(exp.getBytes())));
    }

    @Test
    public void decode_6() {
        final String arg = "fooba";
        final String exp = "Zm9vYmE=";
        Assert.assertEquals(arg, new String(Base64.decode(exp.getBytes())));
    }

    @Test
    public void decode_7() {
        final String arg = "foobar";
        final String exp = "Zm9vYmFy";
        Assert.assertEquals(arg, new String(Base64.decode(exp.getBytes())));
    }

}
