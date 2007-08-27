/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface;


public class RpcTest {

	public RpcTest() {
		super();
	}

	public RpcTest(String f) {
		super();
	}

	public int square(int x) {
		return x * x;
	}

	public static int test_int(int x) {
		return x + 1;
	}

	public static int test_int_arr(int[] x) {
		return x.length;
	}

	public static String test_str(String arg1) {
		return arg1.toUpperCase();
	}

	public static String test_str_arr(String[] arg1) {
		return ">" + arg1.length + " "
				+ ((arg1.length > 0) ? arg1[0].toUpperCase() : "--");
	}

}
