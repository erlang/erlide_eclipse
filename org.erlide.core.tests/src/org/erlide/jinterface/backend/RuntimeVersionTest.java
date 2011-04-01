package org.erlide.jinterface.backend;

import junit.framework.Assert;

import org.junit.Test;

import com.ericsson.otp.erlang.RuntimeVersion;

public class RuntimeVersionTest {

    @Test
    public void toString_1() {
        final String expect = "R12";
        final RuntimeVersion test = new RuntimeVersion(expect);
        Assert.assertEquals(expect, test.toString());
    }

    @Test
    public void toString_2() {
        final String expect = "R12A";
        final RuntimeVersion test = new RuntimeVersion(expect);
        Assert.assertEquals(expect, test.toString());
    }

    @Test
    public void toString_3() {
        final String expect = "R12A-3";
        final RuntimeVersion test = new RuntimeVersion(expect);
        Assert.assertEquals(expect, test.toString());
    }

    @Test
    public void compare_2() {
        final RuntimeVersion test1 = new RuntimeVersion("R12");
        final RuntimeVersion test2 = new RuntimeVersion("R12A");
        Assert.assertEquals(true, test1.compareTo(test2) < 0);
    }

    @Test
    public void compare_3() {
        final RuntimeVersion test1 = new RuntimeVersion("R12A");
        final RuntimeVersion test2 = new RuntimeVersion("R12B");
        Assert.assertEquals(true, test1.compareTo(test2) < 0);
    }

    @Test
    public void compare_4() {
        final RuntimeVersion test1 = new RuntimeVersion("R12A-1");
        final RuntimeVersion test2 = new RuntimeVersion("R12A-2");
        Assert.assertEquals(true, test1.compareTo(test2) < 0);
    }

    @Test
    public void compare_5() {
        final RuntimeVersion test1 = new RuntimeVersion("R12A-3");
        final RuntimeVersion test2 = new RuntimeVersion("R12B-1");
        Assert.assertEquals(true, test1.compareTo(test2) < 0);
    }

    @Test
    public void compare_1a() {
        final RuntimeVersion test1 = new RuntimeVersion("R13");
        final RuntimeVersion test2 = new RuntimeVersion("R12");
        Assert.assertEquals(true, test1.compareTo(test2) > 0);
    }

    @Test
    public void compare_2a() {
        final RuntimeVersion test1 = new RuntimeVersion("R13");
        final RuntimeVersion test2 = new RuntimeVersion("R13A");
        Assert.assertEquals(true, test1.compareTo(test2) < 0);
    }

    @Test
    public void compare_3a() {
        final RuntimeVersion test1 = new RuntimeVersion("R13A");
        final RuntimeVersion test2 = new RuntimeVersion("R13B");
        Assert.assertEquals(true, test1.compareTo(test2) < 0);
    }

    @Test
    public void compare_4a() {
        final RuntimeVersion test1 = new RuntimeVersion("R13A01");
        final RuntimeVersion test2 = new RuntimeVersion("R13A02");
        Assert.assertEquals(true, test1.compareTo(test2) < 0);
    }

    @Test
    public void compare_5a() {
        final RuntimeVersion test1 = new RuntimeVersion("R13A03");
        final RuntimeVersion test2 = new RuntimeVersion("R13B01");
        Assert.assertEquals(true, test1.compareTo(test2) < 0);
    }

}
