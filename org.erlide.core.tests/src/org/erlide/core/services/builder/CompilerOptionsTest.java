package org.erlide.core.services.builder;

import junit.framework.Assert;

import org.erlide.utils.Tuple;
import org.junit.Test;

public class CompilerOptionsTest {
    private static final String DEF_VALUES = "nowarn_bif_clash,nowarn_export_all,nowarn_export_vars,nowarn_shadow_vars,warn_unused_function,warn_deprecated_function,nowarn_obsolete_guard,nowarn_unused_import,warn_unused_vars,warn_unused_record";

    @Test
    public void test_0() {
        final CompilerOptions prefs = new CompilerOptions();
        final String actual = prefs.export().toString();
        final String expect = "[" + DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

    @Test
    public void test_1() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setBooleanOption(CompilerOption.DEBUG_INFO, true);
        final String actual = prefs.export().toString();
        final String expect = "[debug_info," + DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

    @Test
    public void test_2() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setBooleanOption(CompilerOption.DEBUG_INFO, false);
        final String actual = prefs.export().toString();
        final String expect = "[" + DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

    @SuppressWarnings("unchecked")
    @Test
    public void test_3() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setListOption(CompilerOption.DEFINE, new Tuple<String, String>(
                "Macro", null));
        final String actual = prefs.export().toString();
        final String expect = "[{d,'Macro'}," + DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

    @SuppressWarnings("unchecked")
    @Test
    public void test_4() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setListOption(CompilerOption.DEFINE, new Tuple<String, String>(
                "Macro", "[value,1]"));
        final String actual = prefs.export().toString();
        final String expect = "[{d,'Macro',[value,1]}," + DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

    @Test
    public void test_5() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setBooleanOption(CompilerOption.WARN_UNUSED_FUNCTION, true);
        final String actual = prefs.export().toString();
        final String expect = "[" + DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

    @Test
    public void test_6() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setBooleanOption(CompilerOption.WARN_UNUSED_FUNCTION, false);
        final String actual = prefs.export().toString();
        final String expect = "[nowarn_bif_clash,nowarn_export_all,nowarn_export_vars,nowarn_shadow_vars,nowarn_unused_function,warn_deprecated_function,nowarn_obsolete_guard,nowarn_unused_import,warn_unused_vars,warn_unused_record]";
        Assert.assertEquals(expect, actual);
    }

    @SuppressWarnings("unchecked")
    @Test
    public void test_7() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setListOption(CompilerOption.DEFINE, new Tuple<String, String>(
                "Macro", null));
        prefs.removeOption(CompilerOption.DEFINE);
        final String actual = prefs.export().toString();
        final String expect = "[" + DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }

    @Test
    public void test_8() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setBooleanOption(CompilerOption.WARN_BIF_CLASH, true);
        final String actual = prefs.export().toString();
        final String expect = "[" + DEF_VALUES.substring(2) + "]";
        Assert.assertEquals(expect, actual);
    }

    @Test
    public void test_9() {
        final CompilerOptions prefs = new CompilerOptions();
        prefs.setBooleanOption(CompilerOption.WARN_BIF_CLASH, false);
        final String actual = prefs.export().toString();
        final String expect = "[" + DEF_VALUES + "]";
        Assert.assertEquals(expect, actual);
    }
}
