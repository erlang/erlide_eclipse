package org.erlide.runtime;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;

import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.junit.Test;

public class RuntimeVersionTest {

    @Test
    public void toString_1() {
        final String expect = "R12";
        final RuntimeVersion test = new RuntimeVersion(expect);
        assertThat(test.toString(), is(expect));
    }

    @Test
    public void toString_2() {
        final String expect = "R12A";
        final RuntimeVersion test = new RuntimeVersion(expect);
        assertThat(test.toString(), is(expect));
    }

    @Test
    public void toString_3() {
        final String expect = "R12A-3";
        final RuntimeVersion test = new RuntimeVersion(expect);
        assertThat(test.toString(), is(expect));
    }

    @Test
    public void compare_2() {
        final RuntimeVersion test1 = new RuntimeVersion("R12");
        final RuntimeVersion test2 = new RuntimeVersion("R12A");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_3() {
        final RuntimeVersion test1 = new RuntimeVersion("R12A");
        final RuntimeVersion test2 = new RuntimeVersion("R12B");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_4() {
        final RuntimeVersion test1 = new RuntimeVersion("R12A-1");
        final RuntimeVersion test2 = new RuntimeVersion("R12A-2");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_5() {
        final RuntimeVersion test1 = new RuntimeVersion("R12A-3");
        final RuntimeVersion test2 = new RuntimeVersion("R12B-1");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_1a() {
        final RuntimeVersion test1 = new RuntimeVersion("R13");
        final RuntimeVersion test2 = new RuntimeVersion("R12");
        assertThat(test1.compareTo(test2), is(greaterThan(0)));
    }

    @Test
    public void compare_2a() {
        final RuntimeVersion test1 = new RuntimeVersion("R13");
        final RuntimeVersion test2 = new RuntimeVersion("R13A");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_3a() {
        final RuntimeVersion test1 = new RuntimeVersion("R13A");
        final RuntimeVersion test2 = new RuntimeVersion("R13B");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_4a() {
        final RuntimeVersion test1 = new RuntimeVersion("R13A01");
        final RuntimeVersion test2 = new RuntimeVersion("R13A02");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_5a() {
        final RuntimeVersion test1 = new RuntimeVersion("R13A03");
        final RuntimeVersion test2 = new RuntimeVersion("R13B01");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

}
