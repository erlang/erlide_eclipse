package org.erlide.runtime;

import static com.google.common.truth.Truth.assertThat;

import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.junit.Test;

public class RuntimeVersionTest {

    @Test
    public void toString_5() {
        final String expect = "17.0.0";
        final RuntimeVersion test = RuntimeVersion.Serializer.parse("17");
        assertThat(test.toString()).isEqualTo(expect);
    }

    @Test
    public void toString_5a() {
        final String expect = "17.2.3";
        final RuntimeVersion test = RuntimeVersion.Serializer.parse(expect);
        assertThat(test.toString()).isEqualTo(expect);
    }

    @Test
    public void toString_6() {
        final String expect = "17.1.1-rc1";
        final RuntimeVersion test = RuntimeVersion.Serializer.parse(expect);
        assertThat(test.toString()).isEqualTo(expect);
    }

    @Test
    public void toString_6a() {
        final String expect = "17.1.1_rc1";
        final RuntimeVersion test = RuntimeVersion.Serializer.parse(expect);
        assertThat(test.toString()).isEqualTo(expect);
    }

    @Test
    public void toString_6b() {
        final String expect = "17.1.1_rc-1";
        final RuntimeVersion test = RuntimeVersion.Serializer.parse(expect);
        assertThat(test.toString()).isEqualTo(expect);
    }

    @Test
    public void toString_7() {
        final String expect = "17.4.0-rc1";
        final RuntimeVersion test = RuntimeVersion.Serializer.parse("17.4-rc1");
        assertThat(test.toString()).isEqualTo(expect);
    }

    @Test
    public void compare_7() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("17.2.0");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("18.1.0");
        assertThat(test1.compareTo(test2)).isLessThan(0);
    }

    @Test
    public void compare_8() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("17.0.0");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("17.1.0");
        assertThat(test1.compareTo(test2)).isLessThan(0);
    }

    @Test
    public void compare_9() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("17.1.1");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("17.1.2");
        assertThat(test1.compareTo(test2)).isLessThan(0);
    }

    @Test
    public void compare_9a() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("17.1.1");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("17.1.1.2");
        assertThat(test1.compareTo(test2)).isGreaterThan(0);
    }

    @Test
    public void compare_10() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("17.0.0-rc1");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("17.0.0");
        assertThat(test1.compareTo(test2)).isLessThan(0);
    }

    @Test
    public void compare_10a() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("17.0.0-rc1");
        final RuntimeVersion test2 = RuntimeVersion.Serializer.parse("17.0.0-rc2");
        assertThat(test1.compareTo(test2)).isLessThan(0);
    }

    @Test
    public void stable_1() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("20.1.1");
        assertThat(test1.isStable());
    }

    @Test
    public void stable_2() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("20.1.1-rc1");
        assertThat(!test1.isStable());
    }

    @Test
    public void stable_3() {
        final RuntimeVersion test1 = RuntimeVersion.Serializer.parse("20.1.1.2");
        assertThat(test1.isStable());
    }

}
