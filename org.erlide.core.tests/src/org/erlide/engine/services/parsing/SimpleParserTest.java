package org.erlide.engine.services.parsing;

import static com.google.common.truth.Truth.assertThat;

import java.util.List;

import org.erlide.engine.ErlangEngine;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;

public class SimpleParserTest {

    SimpleParserService parser;

    @Before
    public void init() {
        parser = ErlangEngine.getInstance().getSimpleParserService();
    }

    @Test
    public void parse_1() {
        final String input = "a. 1. [c].";
        final List<OtpErlangObject> expected = Lists.newArrayList(new OtpErlangAtom("a"),
                new OtpErlangLong(1), new OtpErlangList(new OtpErlangAtom("c")));
        final List<OtpErlangObject> actual = parser.parse(input);
        assertThat(actual).containsAllIn(expected).inOrder();
    }

}
