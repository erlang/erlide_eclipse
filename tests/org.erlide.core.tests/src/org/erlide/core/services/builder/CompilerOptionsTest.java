package org.erlide.core.services.builder;

import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.core.builder.CompilerOptions;
import org.junit.Assert;
import org.junit.Test;

import com.google.common.base.Splitter;

public class CompilerOptionsTest {
    private static final String DEF_VALUES = "nowarn_export_all,nowarn_export_vars,nowarn_shadow_vars,warn_unused_function,warn_deprecated_function,nowarn_obsolete_guard,nowarn_unused_import,warn_unused_vars,warn_unused_record";

    @Test
    public void test_0() {
        final CompilerOptions prefs = new CompilerOptions();
        final String actual = prefs.export().toString();
        final String expect = "[" + CompilerOptionsTest.DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

    @Test
    public void test_1() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setBooleanOption(CompilerOptions.DEBUG_INFO, true);
        final String actual = prefs.export().toString();
        final String expect = "[debug_info," + CompilerOptionsTest.DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

    @Test
    public void test_2() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setBooleanOption(CompilerOptions.DEBUG_INFO, false);
        final String actual = prefs.export().toString();
        final String expect = "[" + CompilerOptionsTest.DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

    @SuppressWarnings("unchecked")
    @Test
    public void test_3() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setListOption(CompilerOptions.DEFINE,
                new Pair<String, String>("Macro", null));
        final String actual = prefs.export().toString();
        final String expect = "[{d,'Macro'}," + CompilerOptionsTest.DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

    @SuppressWarnings("unchecked")
    @Test
    public void test_4() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setListOption(CompilerOptions.DEFINE, new Pair<>("Macro", "[value,1]"));
        final String actual = prefs.export().toString();
        final String expect = "[{d,'Macro',[value,1]}," + CompilerOptionsTest.DEF_VALUES
                + "]";
        Assert.assertEquals(expect, actual);
    }

    @Test
    public void test_5() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setBooleanOption(CompilerOptions.WARN_UNUSED_FUNCTION, true);
        final String actual = prefs.export().toString();
        final String expect = "[" + CompilerOptionsTest.DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

    @Test
    public void test_6() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setBooleanOption(CompilerOptions.WARN_UNUSED_FUNCTION, false);
        final String actual = prefs.export().toString();
        final String expect = "[nowarn_export_all,nowarn_export_vars,nowarn_shadow_vars,nowarn_unused_function,warn_deprecated_function,nowarn_obsolete_guard,nowarn_unused_import,warn_unused_vars,warn_unused_record]";
        Assert.assertEquals(expect, actual);
    }

    @SuppressWarnings("unchecked")
    @Test
    public void test_7() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setListOption(CompilerOptions.DEFINE,
                new Pair<String, String>("Macro", null));
        prefs.removeOption(CompilerOptions.DEFINE);
        final String actual = prefs.export().toString();
        final String expect = "[" + CompilerOptionsTest.DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

    @Test
    public void test_8() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setBooleanOption(CompilerOptions.WARN_EXPORT_ALL, true);
        final String actual = prefs.export().toString();
        final String expect = "[" + CompilerOptionsTest.DEF_VALUES.substring(2) + "]";
        Assert.assertEquals(expect, actual);
    }

    @Test
    public void test_9() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setBooleanOption(CompilerOptions.WARN_EXPORT_ALL, false);
        final String actual = prefs.export().toString();
        final String expect = "[" + CompilerOptionsTest.DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

    private Iterable<String> parseIncludes(final String s) {
        return Splitter.on(',').trimResults().omitEmptyStrings().split(s);
    }

    @Test
    public void test_10() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setPathOption(CompilerOptions.INCLUDE_DIRS, parseIncludes(""));
        final String actual = prefs.export().toString();
        final String expect = "[" + CompilerOptionsTest.DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

    @Test
    public void test_11() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setPathOption(CompilerOptions.INCLUDE_DIRS, parseIncludes("/tmp/x"));
        final String actual = prefs.export().toString();
        final String expect = "[{i,\"/tmp/x\"}," + CompilerOptionsTest.DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

    @Test
    public void test_12() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setPathOption(CompilerOptions.INCLUDE_DIRS, parseIncludes("/tmp/x,/tmp/y"));
        final String actual = prefs.export().toString();
        final String expect = "[{i,\"/tmp/x\"},{i,\"/tmp/y\"},"
                + CompilerOptionsTest.DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

}
