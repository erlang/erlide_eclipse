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
        final RuntimeVersion test = RuntimeVersion.Serializer.parse(expect);
        assertThat(test.toString(), is(expect));
    }

    @Test
    public void toString_2() {
        final String expect = "R12A";
        final RuntimeVersion test = RuntimeVersion.Serializer.parse(expect);
        assertThat(test.toString(), is(expect));
    }

    @Test
    public void toString_4() {
        final String expect = "R16B03-1";
        final RuntimeVersion test = RuntimeVersion.Serializer.parse(expect);
        assertThat(test.toString(), is(expect));
    }

    @Test
    public void toString_5() {
        final String expect = "17.0.0";
        final RuntimeVersion test = RuntimeVersion.Serializer.parse("17");
        assertThat(test.toString(), is(expect));
    }

    @Test
    public void toString_5a() {
        final String expect = "17.2.3";
        final RuntimeVersion test = RuntimeVersion.Serializer.parse(expect);
        assertThat(test.toString(), is(expect));
    }

    @Test
    public void toString_6() {
        final String expect = "17.1.1-rc1";
        final RuntimeVersion test = RuntimeVersion.Serializer.parse(expect);
        assertThat(test.toString(), is(expect));
    }

    @Test
    public void toString_7() {
        final String expect = "17.4.0-rc1";
        final RuntimeVersion test = RuntimeVersion.Serializer.parse("17.4-rc1");
        assertThat(test.toString(), is(expect));
    }

    @Test
    public void compare_2() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("R12");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("R12A");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_3() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("R12A");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("R12B");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_1a() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("R13");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("R12");
        assertThat(test1.compareTo(test2), is(greaterThan(0)));
    }

    @Test
    public void compare_2a() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("R13");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("R13A");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_3a() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("R13A");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("R13B");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_4a() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("R13A01");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("R13A02");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_5a() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("R13A03");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("R13B01");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_6a() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("R16B03");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("R16B03-1");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_7() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("17.2.0");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("18.1.0");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_8() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("17.0.0");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("17.1.0");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_9() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("17.1.1");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("17.1.2");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_10() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("17.0.0-rc1");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("17.0.0");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_10a() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("17.0.0-rc1");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("17.0.0-rc2");
        assertThat(test1.compareTo(test2), is(lessThan(0)));
    }

    @Test
    public void compare_11() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("R14B");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("R14");
        assertThat(test1.isCompatible(test2), is(true));
    }

    @Test
    public void compare_11d() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("R14B");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("R14B");
        assertThat(test1.isCompatible(test2), is(true));
    }

    @Test
    public void compare_11a() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("R14");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("R14B");
        assertThat(test1.isCompatible(test2), is(true));
    }

    @Test
    public void compare_11b() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("R15B");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("R14");
        assertThat(test1.isCompatible(test2), is(true));
    }

    @Test
    public void compare_11c() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("R14");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("R15B");
        assertThat(test1.isCompatible(test2), is(false));
    }

    // @Test
    // public void compare_10b() {
    // final RuntimeVersion test1 =
    // RuntimeVersion.Serializer.parse("17.0.0-alpha");
    // final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("17.0.0");
    // assertThat(test1.compareTo(test2), is(lessThan(0)));
    // }
}
