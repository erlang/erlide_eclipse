package org.erlide.engine.services.correction;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import java.util.Collection;
import java.util.regex.Pattern;

import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;

public class MessageMatcherTest {

    MessageMatcher matcher;

    @Before
    public void setup() {
        matcher = new MessageMatcher();
    }

    @Test
    public void matchSimpleMessage() {
        final Pattern pattern = Pattern.compile("hello");
        final Collection<String> actual = matcher.matchMessage("hello", pattern);
        assertThat(actual, hasSize(0));
    }

    @Test
    public void noMatchSimpleMessage() {
        final Pattern pattern = Pattern.compile("hello");
        final Collection<String> actual = matcher.matchMessage("hellos", pattern);
        assertThat(actual, is(nullValue()));
    }

    @Test
    public void matchOneMessage() {
        final Pattern pattern = Pattern.compile("hello (.+?) world");
        final Collection<String> actual = matcher.matchMessage("hello ni ce world",
                pattern);
        assertThat(actual, Matchers.contains("ni ce"));
    }

    @Test
    public void noMatchOneMessage() {
        final Pattern pattern = Pattern.compile("hello (.+?) world");
        final Collection<String> actual = matcher.matchMessage("hellos nice world",
                pattern);
        assertThat(actual, is(nullValue()));
    }

    @Test
    public void matchTwoMessages() {
        final Pattern pattern = Pattern.compile("hello (.+?) world(.+?)");
        final Collection<String> actual = matcher.matchMessage("hello ni ce world!",
                pattern);
        assertThat(actual, Matchers.contains("ni ce", "!"));
    }

}
