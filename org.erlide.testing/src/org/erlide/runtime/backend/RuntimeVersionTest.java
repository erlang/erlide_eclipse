package org.erlide.runtime.backend;

import junit.framework.Assert;

import org.junit.Test;

public class RuntimeVersionTest {

	@Test
	public void toString_1() {
		String expect = "R11";
		RuntimeVersion test = new RuntimeVersion(expect);
		Assert.assertEquals(expect, test.toString());
	}

	@Test
	public void toString_2() {
		String expect = "R11A";
		RuntimeVersion test = new RuntimeVersion(expect);
		Assert.assertEquals(expect, test.toString());
	}

	@Test
	public void toString_3() {
		String expect = "R11A-3";
		RuntimeVersion test = new RuntimeVersion(expect);
		Assert.assertEquals(expect, test.toString());
	}

	@Test
	public void compare_1() {
		RuntimeVersion test1 = new RuntimeVersion("R11");
		RuntimeVersion test2 = new RuntimeVersion("R12");
		Assert.assertEquals(true, test1.compareTo(test2) < 0);
	}

	@Test
	public void compare_2() {
		RuntimeVersion test1 = new RuntimeVersion("R11");
		RuntimeVersion test2 = new RuntimeVersion("R11A");
		Assert.assertEquals(true, test1.compareTo(test2) < 0);
	}

	@Test
	public void compare_3() {
		RuntimeVersion test1 = new RuntimeVersion("R11A");
		RuntimeVersion test2 = new RuntimeVersion("R11B");
		Assert.assertEquals(true, test1.compareTo(test2) < 0);
	}

	@Test
	public void compare_4() {
		RuntimeVersion test1 = new RuntimeVersion("R11A-1");
		RuntimeVersion test2 = new RuntimeVersion("R11A-2");
		Assert.assertEquals(true, test1.compareTo(test2) < 0);
	}

	@Test
	public void compare_5() {
		RuntimeVersion test1 = new RuntimeVersion("R11A-3");
		RuntimeVersion test2 = new RuntimeVersion("R11B-1");
		Assert.assertEquals(true, test1.compareTo(test2) < 0);
	}

}
