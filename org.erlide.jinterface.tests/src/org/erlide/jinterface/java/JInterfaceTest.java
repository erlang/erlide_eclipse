package org.erlide.jinterface.java;

import junit.framework.Assert;

import org.erlide.jinterface.ParserException;
import org.erlide.jinterface.TermParser;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpOutputStream;

public class JInterfaceTest {

	@Test
	public void sublist_1() throws ParserException {
		final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3,4]");
		final OtpErlangList s = (OtpErlangList) TermParser.parse("[2,3,4]");
		final OtpErlangObject ss = r.getTail();
		Assert.assertEquals(s, ss);
	}

	@Test
	public void sublist_2() throws ParserException {
		final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3|4]");
		final OtpErlangList s = (OtpErlangList) TermParser.parse("[2,3|4]");
		final OtpErlangObject ss = r.getTail();
		Assert.assertEquals(s, ss);
	}

	@Test
	public void sublist_4() throws ParserException {
		final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3,4]");
		final OtpErlangList s = (OtpErlangList) TermParser.parse("[3,4]");
		final OtpErlangObject ss = r.getNthTail(2);
		Assert.assertEquals(s, ss);
	}

	@Test
	public void sublist_3() throws ParserException {
		final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3|4]");
		final OtpErlangList s = (OtpErlangList) TermParser.parse("[3|4]");
		final OtpErlangObject ss = r.getNthTail(2);
		Assert.assertEquals(s, ss);
	}

	@Test
	public void sublist_4a() throws ParserException {
		final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3,4]");
		final OtpErlangObject ss = r.getNthTail(0);
		Assert.assertEquals(r, ss);
	}

	@Test
	public void sublist_4b() throws ParserException {
		final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3,4]");
		final OtpErlangObject ss = r.getNthTail(4);
		Assert.assertEquals(new OtpErlangList(), ss);
	}

	@Test
	public void sublist_4c() throws ParserException {
		final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3,4]");
		final OtpErlangObject ss = r.getNthTail(5);
		Assert.assertEquals(null, ss);
	}

	@Test
	public void sublist_4d() throws ParserException {
		final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3|4]");
		final OtpErlangObject s = TermParser.parse("4");
		final OtpErlangObject ss = r.getNthTail(3);
		Assert.assertEquals(s, ss);
	}

	@Test
	public void sublist_5() throws ParserException {
		final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3,4]");
		final OtpErlangObject ss = r.getNthTail(2);
		final OtpOutputStream out = new OtpOutputStream();
		ss.encode(out);
		final byte[] contents1 = out.toByteArray();
		out.reset();
		final OtpErlangList s = (OtpErlangList) TermParser.parse("[3,4]");
		s.encode(out);
		final byte[] contents2 = out.toByteArray();

		Assert.assertEquals(contents2.length, contents1.length);
		for (int i = 0; i < contents1.length; i++) {
			Assert.assertEquals(contents2[i], contents1[i]);
		}
	}

	@Test
	public void sublist_6() throws ParserException {
		final OtpErlangList r = (OtpErlangList) TermParser.parse("[1,2,3|4]");
		final OtpErlangObject ss = r.getNthTail(2);
		final OtpOutputStream out = new OtpOutputStream();
		ss.encode(out);
		final byte[] contents1 = out.toByteArray();
		out.reset();
		final OtpErlangList s = (OtpErlangList) TermParser.parse("[3|4]");
		s.encode(out);
		final byte[] contents2 = out.toByteArray();

		Assert.assertEquals(contents2.length, contents1.length);
		for (int i = 0; i < contents1.length; i++) {
			Assert.assertEquals(contents2[i], contents1[i]);
		}
	}
}
