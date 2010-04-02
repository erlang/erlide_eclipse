package org.erlide.core.preferences;

import junit.framework.Assert;

import org.junit.Test;

public class Base64Test {

	@Test
	public void encode_1() {
		String arg = "";
		String exp = "";
		Assert.assertEquals(exp, new String(Base64.encode(arg.getBytes())));
	}

	@Test
	public void encode_2() {
		String arg = "f";
		String exp = "Zg==";
		Assert.assertEquals(exp, new String(Base64.encode(arg.getBytes())));
	}

	@Test
	public void encode_3() {
		String arg = "fo";
		String exp = "Zm8=";
		Assert.assertEquals(exp, new String(Base64.encode(arg.getBytes())));
	}

	@Test
	public void encode_4() {
		String arg = "foo";
		String exp = "Zm9v";
		Assert.assertEquals(exp, new String(Base64.encode(arg.getBytes())));
	}

	@Test
	public void encode_5() {
		String arg = "foob";
		String exp = "Zm9vYg==";
		Assert.assertEquals(exp, new String(Base64.encode(arg.getBytes())));
	}

	@Test
	public void encode_6() {
		String arg = "fooba";
		String exp = "Zm9vYmE=";
		Assert.assertEquals(exp, new String(Base64.encode(arg.getBytes())));
	}

	@Test
	public void encode_7() {
		String arg = "foobar";
		String exp = "Zm9vYmFy";
		Assert.assertEquals(exp, new String(Base64.encode(arg.getBytes())));
	}

	@Test
	public void decode_1() {
		String arg = "";
		String exp = "";
		Assert.assertEquals(arg, new String(Base64.decode(exp.getBytes())));
	}

	@Test
	public void decode_2() {
		String arg = "f";
		String exp = "Zg==";
		Assert.assertEquals(arg, new String(Base64.decode(exp.getBytes())));
	}

	@Test
	public void decode_3() {
		String arg = "fo";
		String exp = "Zm8=";
		Assert.assertEquals(arg, new String(Base64.decode(exp.getBytes())));
	}

	@Test
	public void decode_4() {
		String arg = "foo";
		String exp = "Zm9v";
		Assert.assertEquals(arg, new String(Base64.decode(exp.getBytes())));
	}

	@Test
	public void decode_5() {
		String arg = "foob";
		String exp = "Zm9vYg==";
		Assert.assertEquals(arg, new String(Base64.decode(exp.getBytes())));
	}

	@Test
	public void decode_6() {
		String arg = "fooba";
		String exp = "Zm9vYmE=";
		Assert.assertEquals(arg, new String(Base64.decode(exp.getBytes())));
	}

	@Test
	public void decode_7() {
		String arg = "foobar";
		String exp = "Zm9vYmFy";
		Assert.assertEquals(arg, new String(Base64.decode(exp.getBytes())));
	}

}
