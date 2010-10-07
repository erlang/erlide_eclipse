package org.erlide.core.preferences;

import java.util.ArrayList;
import java.util.Collection;

import junit.framework.Assert;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.jinterface.util.ErlUtils;
import org.erlide.jinterface.util.ParserException;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;

public class ErlProjectLayoutTest {
	String src = "{layout," + "[\"s1/a/b\", \"s2\"],"
			+ "[\"i1/y\", {'V1', \"i2/x\"}]," + "\"eb/in\"," + "[\"d1/fg\"],"
			+ "\"pv\"" + "}";
	OtpErlangObject input;

	@Before
	public void setUp() throws Exception {
		input = ErlUtils.parse(src);
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void shouldConvertfromTermAndBack() throws OtpErlangException,
			ParserException {
		ErlProjectLayout layout = new ErlProjectLayout(input);
		Assert.assertEquals("convert from term and back", input,
				layout.asTerm());
	}

	@Test
	public void shouldAddSourceDir() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.addSource("hello");
		Collection<IPath> expected = Lists
				.newArrayList(ErlProjectLayout.OTP_LAYOUT.getSources());
		expected.add(new Path("hello"));
		MatcherAssert.assertThat("source dirs content", info.getSources(),
				Matchers.equalTo(expected));
	}

	@Test
	public void shouldAddIncludeDir() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.addInclude("hello");
		Collection<IPath> expected = Lists
				.newArrayList(ErlProjectLayout.OTP_LAYOUT.getIncludes());
		expected.add(new Path("hello"));
		MatcherAssert.assertThat("source dirs content", info.getIncludes(),
				Matchers.equalTo(expected));
	}

}
