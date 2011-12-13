package org.erlide.core.common;

import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import com.google.common.collect.Lists;

public class CommonUtilsTest {

    @Test
    public void packList() {
        final List<String> input = Lists.newArrayList("aaa", "bbb", "ccc");
        final String expected = "aaa--bbb--ccc";
        final String actual = CommonUtils.packList(input, "--");
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void packArray() {
        final String[] input = new String[] { "aaa", "bbb", "ccc" };
        final String expected = "aaa--bbb--ccc";
        final String actual = CommonUtils.packArray(input, "--");
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void unpackArray() {
        final String input = "aaa--bbb--ccc";
        final String[] expected = new String[] { "aaa", "bbb", "ccc" };
        final String[] actual = CommonUtils.unpackArray(input, "--");
        Assert.assertArrayEquals(expected, actual);
    }

}
