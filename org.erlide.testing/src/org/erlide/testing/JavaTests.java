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

import org.erlide.testing.java.PatternMatchTest;

import junit.framework.Test;
import junit.framework.TestSuite;

public class JavaTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Java tests for org.erlide");
		// $JUnit-BEGIN$
		suite.addTestSuite(PatternMatchTest.class);
		// $JUnit-END$
		return suite;
	}

}
