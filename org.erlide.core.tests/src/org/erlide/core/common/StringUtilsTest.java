package org.erlide.core.common;

import java.util.List;

import junit.framework.Assert;

import org.junit.Test;

import com.google.common.collect.Lists;

public class StringUtilsTest {

    @Test
    public void commonPrefixTest_1() {
        final List<String> input = Lists.newArrayList("alfabeta", "alfagamma",
                "alfadelta");
        final String expected = "alfa";
        final String actual = StringUtils.getLongestPrefix(input);
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void commonPrefixTest_2() {
        final List<String> input = Lists.newArrayList("alf", "alfagamma",
                "alfadelta");
        final String expected = "alf";
        final String actual = StringUtils.getLongestPrefix(input);
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void commonPrefixTest_2a() {
        final List<String> input = Lists.newArrayList("alfabeta", "alfagamma",
                "alf");
        final String expected = "alf";
        final String actual = StringUtils.getLongestPrefix(input);
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void commonPrefixTest_3() {
        final List<String> input = Lists.newArrayList("zalfabeta", "alfagamma",
                "ualf");
        final String expected = "";
        final String actual = StringUtils.getLongestPrefix(input);
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void removeCommonPrefixTest_1() {
        final List<String> input = Lists.newArrayList("alf", "alfagamma",
                "alfadelta");
        final List<String> expected = Lists
                .newArrayList("", "agamma", "adelta");
        final List<String> actual = StringUtils.removeCommonPrefixes(input);
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void removeCommonPrefixTest_2() {
        final List<String> input = Lists.newArrayList("zalfabeta", "alfagamma",
                "ualf");
        final List<String> expected = input;
        final List<String> actual = StringUtils.removeCommonPrefixes(input);
        Assert.assertEquals(expected, actual);
    }

}
