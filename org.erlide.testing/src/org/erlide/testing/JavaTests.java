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

import org.erlide.testing.java.ErlRpcTest;
import org.erlide.testing.java.PatternMatchTest;
import org.erlide.testing.java.core.preferences.CoreTests;
import org.erlide.testing.java.runtime.backend.RuntimeTests;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses( { ErlRpcTest.class, PatternMatchTest.class,
		CoreTests.class, RuntimeTests.class })
public class JavaTests {

}
