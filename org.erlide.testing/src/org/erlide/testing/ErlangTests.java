/*******************************************************************************
 * Copyright (c) 2006 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.testing;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.erlide.testing.erlang.ErlangTest;

public class ErlangTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Erlang tests for org.erlide");
		// $JUnit-BEGIN$
		suite.addTestSuite(ErlangTest.class);
		// $JUnit-END$
		return suite;
	}

}
