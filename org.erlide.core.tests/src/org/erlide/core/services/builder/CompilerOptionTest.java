package org.erlide.core.services.builder;

import java.util.List;

import junit.framework.Assert;

import org.erlide.core.services.builder.CompilerOption.BooleanOption;
import org.erlide.core.services.builder.CompilerOption.DefineOption;
import org.erlide.core.services.builder.CompilerOption.WarningOption;
import org.erlide.utils.TermParserException;
import org.erlide.utils.Tuple;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;

public class CompilerOptionTest {

    @Test
    public void testFind() {
        final CompilerOption option = CompilerOption.WARN_EXPORT_ALL;
        Assert.assertEquals("warn_export_all", option.getName());
        Assert.assertEquals(WarningOption.class, option.getClass());
    }

    @Test
    public void testToTerm_1() throws TermParserException {
        final BooleanOption option = CompilerOption.WARN_EXPORT_ALL;
        final OtpErlangObject actual = option.toTerm(true);
        final String expected = "warn_export_all";
        Assert.assertEquals(expected, actual.toString());
    }

    @Test
    public void testToTerm_2() throws TermParserException {
        final BooleanOption option = CompilerOption.WARN_EXPORT_ALL;
        final OtpErlangObject actual = option.toTerm(false);
        final String expected = "nowarn_export_all";
        Assert.assertEquals(expected, actual.toString());
    }

    @Test
    public void testToTerm_3() throws TermParserException {
        final BooleanOption option = CompilerOption.DEBUG_INFO;
        final OtpErlangObject actual = option.toTerm(true);
        final String expected = "debug_info";
        Assert.assertEquals(expected, actual.toString());
    }

    @Test
    public void testToTerm_4() throws TermParserException {
        final BooleanOption option = CompilerOption.DEBUG_INFO;
        final OtpErlangObject actual = option.toTerm(false);
        final String expected = null;
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void testToTerm_5() throws TermParserException {
        final DefineOption option = CompilerOption.DEFINE;
        @SuppressWarnings("unchecked")
        final List<Tuple<String, String>> values = Lists
                .newArrayList(new Tuple<String, String>("Macro", "[hej,1]"));
        final OtpErlangObject actual = option.toTerm(values);
        final String expected = "[{d,'Macro',[hej,1]}]";
        Assert.assertEquals(expected, actual.toString());
    }

    @Test
    public void testToTerm_5a() throws TermParserException {
        final DefineOption option = CompilerOption.DEFINE;
        @SuppressWarnings("unchecked")
        final List<Tuple<String, String>> values = Lists
                .newArrayList(new Tuple<String, String>("Macro", ""));
        final OtpErlangObject actual = option.toTerm(values);
        final String expected = "[{d,'Macro'}]";
        Assert.assertEquals(expected, actual.toString());
    }

    @Test
    public void testToTerm_6() throws TermParserException {
        final BooleanOption option = CompilerOption.WARN_EXPORT_ALL;
        final OtpErlangObject actual = option.toTerm(false);
        final String expected = "nowarn_export_all";
        Assert.assertEquals(expected, actual.toString());
    }

}
