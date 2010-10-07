package org.erlide.core.preferences;

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
	public void checkOtpLayout() throws ParserException {
		OtpErlangObject expected = ErlUtils
				.parse("{layout,[\"src\"],[\"include\"],"
						+ "\"ebin\",[\"doc\"],\"priv\"}");
		MatcherAssert.assertThat(ErlProjectLayout.OTP_LAYOUT.asTerm(),
				Matchers.equalTo(expected));
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
		Collection<IPath> result = info.getSources();
		hasListItemNamedHello(info, result);
	}

	private void hasListItemNamedHello(ErlProjectLayout info,
			Collection<IPath> result) {
		MatcherAssert.assertThat("not same object", info,
				Matchers.not(Matchers.equalTo(ErlProjectLayout.OTP_LAYOUT)));
		MatcherAssert.assertThat("source dirs size", result.size(),
				Matchers.equalTo(2));
		MatcherAssert.assertThat("source dirs content", result,
				Matchers.hasItem(new Path("hello")));
	}

	@Test
	public void shouldAddIncludeDir() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.addInclude("hello");
		Collection<IPath> result = info.getIncludes();
		hasListItemNamedHello(info, result);
	}

	@Test
	public void shouldAddDocDir() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.addDoc("hello");
		Collection<IPath> result = info.getDocs();
		hasListItemNamedHello(info, result);
	}

	@Test
	public void shouldSetOutput() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.setOutput("hello");
		IPath expected = new Path("hello");
		MatcherAssert.assertThat("not same object", info,
				Matchers.not(Matchers.equalTo(ErlProjectLayout.OTP_LAYOUT)));
		MatcherAssert.assertThat(info.getOutput(), Matchers.equalTo(expected));
	}

	@Test
	public void shouldSetPriv() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.setPriv("hello");
		IPath expected = new Path("hello");
		MatcherAssert.assertThat("not same object", info,
				Matchers.not(Matchers.equalTo(ErlProjectLayout.OTP_LAYOUT)));
		MatcherAssert.assertThat(info.getPriv(), Matchers.equalTo(expected));
	}

	@Test
	public void shouldRemoveSourceDir() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.removeSource("src");
		Collection<IPath> result = info.getSources();
		hasListNoItemNamed(info, result, "src");
	}

	private void hasListNoItemNamed(ErlProjectLayout info,
			Collection<IPath> result, String string) {
		MatcherAssert.assertThat("not same object", info,
				Matchers.not(Matchers.equalTo(ErlProjectLayout.OTP_LAYOUT)));
		MatcherAssert.assertThat("source dirs content", result,
				Matchers.not(Matchers.hasItem(new Path(string))));
	}

	@Test
	public void shouldRemoveIncludeDir() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.removeInclude("include");
		Collection<IPath> result = info.getIncludes();
		hasListNoItemNamed(info, result, "include");
	}

	@Test
	public void shouldRemoveDocDir() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.removeDoc("doc");
		Collection<IPath> result = info.getDocs();
		hasListNoItemNamed(info, result, "doc");
	}

}
