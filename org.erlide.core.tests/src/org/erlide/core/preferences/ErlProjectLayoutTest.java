package org.erlide.core.preferences;

import java.util.Collection;
import java.util.List;

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
	public void shouldSetSources() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		List<IPath> srcs = Lists.newArrayList();
		srcs.add(new Path("hello"));
		info = info.setSources(srcs);
		Collection<IPath> result = info.getSources();

		assertNotSameAs(info);
		assertListLength(result, 1);
		assertHasListItemNamed(result, "hello");
	}

	@Test
	public void shouldSetIncludes() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		List<IPath> srcs = Lists.newArrayList();
		srcs.add(new Path("hello"));
		info = info.setIncludes(srcs);
		Collection<IPath> result = info.getIncludes();

		assertNotSameAs(info);
		assertListLength(result, 1);
		assertHasListItemNamed(result, "hello");
	}

	@Test
	public void shouldSetOutput() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.setOutput("hello");
		IPath expected = new Path("hello");

		assertNotSameAs(info);
		MatcherAssert.assertThat(info.getOutput(), Matchers.equalTo(expected));
	}

	@Test
	public void shouldSetDocs() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		List<IPath> srcs = Lists.newArrayList();
		srcs.add(new Path("hello"));
		info = info.setDocs(srcs);
		Collection<IPath> result = info.getDocs();

		assertNotSameAs(info);
		assertListLength(result, 1);
		assertHasListItemNamed(result, "hello");
	}

	@Test
	public void shouldSetPriv() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.setPriv("hello");
		IPath expected = new Path("hello");

		assertNotSameAs(info);
		MatcherAssert.assertThat(info.getPriv(), Matchers.equalTo(expected));
	}

	@Test
	public void shouldAddSource() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.addSource("hello");
		Collection<IPath> result = info.getSources();

		assertNotSameAs(info);
		assertListLength(result, 2);
		assertHasListItemNamed(result, "hello");
		assertHasListItemNamed(result, "src");
	}

	@Test
	public void shouldAddSources() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		List<IPath> list = Lists.newArrayList();
		list.add(new Path("hello"));
		list.add(new Path("world"));
		info = info.addSources(list);
		Collection<IPath> result = info.getSources();

		assertNotSameAs(info);
		assertListLength(result, 2);
		assertHasListItemNamed(result, "hello");
		assertHasListItemNamed(result, "world");
	}

	@Test
	public void shouldAddInclude() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.addInclude("hello");
		Collection<IPath> result = info.getIncludes();

		assertNotSameAs(info);
		assertListLength(result, 2);
		assertHasListItemNamed(result, "hello");
	}

	@Test
	public void shouldAddDoc() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.addDoc("hello");
		Collection<IPath> result = info.getDocs();

		assertNotSameAs(info);
		assertListLength(result, 2);
		assertHasListItemNamed(result, "hello");
	}

	@Test
	public void shouldRemoveSource() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.removeSource("src");
		Collection<IPath> result = info.getSources();

		assertNotSameAs(info);
		assertListLength(result, 0);
		hasListNoItemNamed(info, result, "src");
	}

	private void hasListNoItemNamed(ErlProjectLayout info,
			Collection<IPath> result, String string) {
		MatcherAssert.assertThat("list has item", result,
				Matchers.not(Matchers.hasItem(new Path(string))));
	}

	@Test
	public void shouldRemoveInclude() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.removeInclude("include");
		Collection<IPath> result = info.getIncludes();
		hasListNoItemNamed(info, result, "include");
	}

	@Test
	public void shouldRemoveDoc() {
		ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
		info = info.removeDoc("doc");
		Collection<IPath> result = info.getDocs();
		hasListNoItemNamed(info, result, "doc");
	}

	private void assertListLength(Collection<IPath> result, int length) {
		MatcherAssert.assertThat("source dirs size", result.size(),
				Matchers.equalTo(length));
	}

	private void assertHasListItemNamed(Collection<IPath> result, String name) {
		MatcherAssert.assertThat("source dirs content", result,
				Matchers.hasItem(new Path(name)));
	}

	private void assertNotSameAs(ErlProjectLayout info) {
		MatcherAssert.assertThat("not same object", info,
				Matchers.not(Matchers.equalTo(ErlProjectLayout.OTP_LAYOUT)));
	}

}
