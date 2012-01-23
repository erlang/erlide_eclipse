package org.erlide.jinterface.util;

import java.util.List;

import org.erlide.utils.ListsUtils;
import org.junit.Assert;
import org.junit.Test;

import com.google.common.collect.Lists;

public class ListsUtilsTest {

    @Test
    public void packList() {
        final List<String> input = Lists.newArrayList("aaa", "bbb", "ccc");
        final String expected = "aaa--bbb--ccc";
        final String actual = ListsUtils.packList(input, "--");
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void packArray() {
        final String[] input = new String[] { "aaa", "bbb", "ccc" };
        final String expected = "aaa--bbb--ccc";
        final String actual = ListsUtils.packArray(input, "--");
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void unpackArray() {
        final String input = "aaa--bbb--ccc";
        final String[] expected = new String[] { "aaa", "bbb", "ccc" };
        final String[] actual = ListsUtils.unpackArray(input, "--");
        Assert.assertArrayEquals(expected, actual);
    }

}
