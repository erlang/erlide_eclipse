package org.erlide.jinterface.util;

import java.util.List;

import junit.framework.Assert;

import org.erlide.utils.StringUtils;
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

    @Test
    public void removeCommonPrefixTest_3() {
        final List<String> input = Lists.newArrayList("beta");
        final List<String> expected = input;
        final List<String> actual = StringUtils.removeCommonPrefixes(input);
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void withoutInterrogationMark_1() {
        final String input = "?hello";
        final String expected = "hello";
        final String actual = StringUtils.withoutInterrogationMark(input);
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void withoutInterrogationMark_2() {
        final String input = "hello";
        final String expected = "hello";
        final String actual = StringUtils.withoutInterrogationMark(input);
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void unquote_1() {
        final String input = "'hello'";
        final String expected = "hello";
        final String actual = StringUtils.unquote(input);
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void unquote_2() {
        final String input = "h'ello'";
        final String expected = "h'ello'";
        final String actual = StringUtils.unquote(input);
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void unquote_3() {
        final String input = "h";
        final String expected = "h";
        final String actual = StringUtils.unquote(input);
        Assert.assertEquals(expected, actual);
    }

}
