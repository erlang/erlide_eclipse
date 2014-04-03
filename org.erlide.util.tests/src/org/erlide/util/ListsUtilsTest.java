package org.erlide.util;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.List;

import org.junit.Test;

import com.google.common.collect.Lists;

public class ListsUtilsTest {

    @Test
    public void packList() {
        final List<String> input = Lists.newArrayList("aaa", "bbb", "ccc");
        final String expected = "aaa--bbb--ccc";
        final String actual = ListsUtils.packList(input, "--");
        assertThat(actual, is(expected));
    }

    @Test
    public void packEmptyList() {
        final List<String> input = Lists.newArrayList();
        final String expected = "";
        final String actual = ListsUtils.packList(input, "--");
        assertThat(actual, is(expected));
    }

    @Test
    public void packOneList() {
        final List<String> input = Lists.newArrayList("aaa");
        final String expected = "aaa";
        final String actual = ListsUtils.packList(input, "--");
        assertThat(actual, is(expected));
    }

    @Test
    public void packArray() {
        final String[] input = new String[] { "aaa", "bbb", "ccc" };
        final String expected = "aaa--bbb--ccc";
        final String actual = ListsUtils.packArray(input, "--");
        assertThat(actual, is(expected));
    }

    @Test
    public void unpackArray() {
        final String input = "aaa--bbb--ccc";
        final String[] expected = new String[] { "aaa", "bbb", "ccc" };
        final String[] actual = ListsUtils.unpackArray(input, "--");
        assertThat(actual, is(expected));
    }

    @Test
    public void unpackEmptyArray() {
        final String input = "";
        final String[] expected = new String[] {};
        final String[] actual = ListsUtils.unpackArray(input, "--");
        assertThat(actual, is(expected));
    }

}
