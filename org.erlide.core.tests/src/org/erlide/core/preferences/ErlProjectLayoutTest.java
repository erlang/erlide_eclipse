package org.erlide.core.preferences;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.core.internal.model.root.ErlProjectLayout;
import org.erlide.utils.ErlUtils;
import org.erlide.utils.TermParserException;
import org.hamcrest.Matchers;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;

public class ErlProjectLayoutTest {
    String src = "{layout,[\"s1/a/b\", \"s2\"],"
            + "[\"i1/y\", {'V1', \"i2/x\"}],\"eb/in\",[\"d1/fg\"],\"pv\"}";
    OtpErlangObject input;

    @Before
    public void setUp() throws Exception {
        input = ErlUtils.parse(src);
    }

    @After
    public void tearDown() throws Exception {
    }

    @Test
    public void checkOtpLayout() throws TermParserException {
        final OtpErlangObject expected = ErlUtils
                .parse("{layout,[\"src\"],[\"include\"],"
                        + "\"ebin\",[\"doc\"],\"priv\"}");
        assertThat(ErlProjectLayout.OTP_LAYOUT.asTerm(), equalTo(expected));
    }

    @Test
    public void shouldConvertfromTermAndBack() throws OtpErlangException,
            TermParserException {
        final ErlProjectLayout layout = new ErlProjectLayout(input);
        assertThat(input, is(layout.asTerm()));
    }

    @Test
    public void shouldSetSources() {
        ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
        final List<IPath> srcs = Lists.newArrayList();
        srcs.add(new Path("hello"));
        info = info.setSources(srcs);
        final Collection<IPath> result = info.getSources();

        assertNotSameAs(info);
        assertListLength(result, 1);
        assertHasListItemNamed(result, "hello");
    }

    @Test
    public void shouldSetIncludes() {
        ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
        final List<IPath> srcs = Lists.newArrayList();
        srcs.add(new Path("hello"));
        info = info.setIncludes(srcs);
        final Collection<IPath> result = info.getIncludes();

        assertNotSameAs(info);
        assertListLength(result, 1);
        assertHasListItemNamed(result, "hello");
    }

    @Test
    public void shouldSetOutput() {
        ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
        info = info.setOutput("hello");
        final IPath expected = new Path("hello");

        assertNotSameAs(info);
        assertThat(info.getOutput(), Matchers.equalTo(expected));
    }

    @Test
    public void shouldSetDocs() {
        ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
        final List<IPath> srcs = Lists.newArrayList();
        srcs.add(new Path("hello"));
        info = info.setDocs(srcs);
        final Collection<IPath> result = info.getDocs();

        assertNotSameAs(info);
        assertListLength(result, 1);
        assertHasListItemNamed(result, "hello");
    }

    @Test
    public void shouldSetPriv() {
        ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
        info = info.setPriv("hello");
        final IPath expected = new Path("hello");

        assertNotSameAs(info);
        assertThat(info.getPriv(), Matchers.equalTo(expected));
    }

    @Test
    public void shouldAddSource() {
        ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
        info = info.addSource("hello");
        final Collection<IPath> result = info.getSources();

        assertNotSameAs(info);
        assertListLength(result, 2);
        assertHasListItemNamed(result, "hello");
        assertHasListItemNamed(result, "src");
    }

    @Test
    public void shouldAddSources() {
        ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
        final List<IPath> list = Lists.newArrayList();
        list.add(new Path("hello"));
        list.add(new Path("world"));
        info = info.addSources(list);
        final Collection<IPath> result = info.getSources();

        assertNotSameAs(info);
        assertListLength(result, 2);
        assertHasListItemNamed(result, "hello");
        assertHasListItemNamed(result, "world");
    }

    @Test
    public void shouldAddInclude() {
        ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
        info = info.addInclude("hello");
        final Collection<IPath> result = info.getIncludes();

        assertNotSameAs(info);
        assertListLength(result, 2);
        assertHasListItemNamed(result, "hello");
    }

    @Test
    public void shouldAddDoc() {
        ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
        info = info.addDoc("hello");
        final Collection<IPath> result = info.getDocs();

        assertNotSameAs(info);
        assertListLength(result, 2);
        assertHasListItemNamed(result, "hello");
    }

    @Test
    public void shouldRemoveSource() {
        ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
        info = info.removeSource("src");
        final Collection<IPath> result = info.getSources();

        assertNotSameAs(info);
        assertListLength(result, 0);
        hasListNoItemNamed(info, result, "src");
    }

    private void hasListNoItemNamed(final ErlProjectLayout info,
            final Collection<IPath> result, final String string) {
        assertThat("list has item", result,
                Matchers.not(Matchers.hasItem(new Path(string))));
    }

    @Test
    public void shouldRemoveInclude() {
        ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
        info = info.removeInclude("include");
        final Collection<IPath> result = info.getIncludes();
        hasListNoItemNamed(info, result, "include");
    }

    @Test
    public void shouldRemoveDoc() {
        ErlProjectLayout info = ErlProjectLayout.OTP_LAYOUT;
        info = info.removeDoc("doc");
        final Collection<IPath> result = info.getDocs();
        hasListNoItemNamed(info, result, "doc");
    }

    private void assertListLength(final Collection<IPath> result,
            final int length) {
        assertThat("source dirs size", result.size(), Matchers.equalTo(length));
    }

    private void assertHasListItemNamed(final Collection<IPath> result,
            final String name) {
        assertThat("source dirs content", result,
                Matchers.hasItem(new Path(name)));
    }

    private void assertNotSameAs(final ErlProjectLayout info) {
        assertThat("not same object", info,
                Matchers.not(Matchers.equalTo(ErlProjectLayout.OTP_LAYOUT)));
    }

}
