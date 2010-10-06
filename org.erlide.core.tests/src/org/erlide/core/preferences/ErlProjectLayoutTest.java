package org.erlide.core.preferences;

import junit.framework.Assert;

import org.erlide.jinterface.util.ErlUtils;
import org.erlide.jinterface.util.ParserException;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlProjectLayoutTest {
	String src = "{layout," +
			"[\"s1\", \"s2\"]," +
			"[\"i1\", {'V1', \"i2\"}]," +
			"\"eb\"," +
			"[]," +
			"\"pv\"" +
			"}";
	OtpErlangObject input;

	@Before
	public void setUp() throws Exception {
		input = ErlUtils.parse(src);
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void fromTerm() throws OtpErlangException, ParserException {
		ErlProjectLayout layout = new ErlProjectLayout(input);
		Assert.assertEquals("print", input, layout.asTerm());
	}

}
