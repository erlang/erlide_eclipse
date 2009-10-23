package org.erlide.runtime.backend;

import junit.framework.Assert;

import org.erlide.jinterface.backend.RuntimeVersion;
import org.junit.Test;

public class RuntimeVersionTest {

	@Test
	public void toString_1() {
		final String expect = "R11";
		final RuntimeVersion test = new RuntimeVersion(expect);
		Assert.assertEquals(expect, test.toString());
	}

	@Test
	public void toString_2() {
		final String expect = "R11A";
		final RuntimeVersion test = new RuntimeVersion(expect);
		Assert.assertEquals(expect, test.toString());
	}

	@Test
	public void toString_3() {
		final String expect = "R11A-3";
		final RuntimeVersion test = new RuntimeVersion(expect);
		Assert.assertEquals(expect, test.toString());
	}

	@Test
	public void compare_1() {
		final RuntimeVersion test1 = new RuntimeVersion("R11");
		final RuntimeVersion test2 = new RuntimeVersion("R12");
		Assert.assertEquals(true, test1.compareTo(test2) < 0);
	}

	@Test
	public void compare_2() {
		final RuntimeVersion test1 = new RuntimeVersion("R11");
		final RuntimeVersion test2 = new RuntimeVersion("R11A");
		Assert.assertEquals(true, test1.compareTo(test2) < 0);
	}

	@Test
	public void compare_3() {
		final RuntimeVersion test1 = new RuntimeVersion("R11A");
		final RuntimeVersion test2 = new RuntimeVersion("R11B");
		Assert.assertEquals(true, test1.compareTo(test2) < 0);
	}

	@Test
	public void compare_4() {
		final RuntimeVersion test1 = new RuntimeVersion("R11A-1");
		final RuntimeVersion test2 = new RuntimeVersion("R11A-2");
		Assert.assertEquals(true, test1.compareTo(test2) < 0);
	}

	@Test
	public void compare_5() {
		final RuntimeVersion test1 = new RuntimeVersion("R11A-3");
		final RuntimeVersion test2 = new RuntimeVersion("R11B-1");
		Assert.assertEquals(true, test1.compareTo(test2) < 0);
	}

	@Test
	public void compare_1a() {
		final RuntimeVersion test1 = new RuntimeVersion("R13");
		final RuntimeVersion test2 = new RuntimeVersion("R12");
		Assert.assertEquals(true, test1.compareTo(test2) > 0);
	}

	@Test
	public void compare_2a() {
		final RuntimeVersion test1 = new RuntimeVersion("R13");
		final RuntimeVersion test2 = new RuntimeVersion("R13A");
		Assert.assertEquals(true, test1.compareTo(test2) < 0);
	}

	@Test
	public void compare_3a() {
		final RuntimeVersion test1 = new RuntimeVersion("R13A");
		final RuntimeVersion test2 = new RuntimeVersion("R13B");
		Assert.assertEquals(true, test1.compareTo(test2) < 0);
	}

	@Test
	public void compare_4a() {
		final RuntimeVersion test1 = new RuntimeVersion("R13A01");
		final RuntimeVersion test2 = new RuntimeVersion("R13A02");
		Assert.assertEquals(true, test1.compareTo(test2) < 0);
	}

	@Test
	public void compare_5a() {
		final RuntimeVersion test1 = new RuntimeVersion("R13A03");
		final RuntimeVersion test2 = new RuntimeVersion("R13B01");
		Assert.assertEquals(true, test1.compareTo(test2) < 0);
	}

}
