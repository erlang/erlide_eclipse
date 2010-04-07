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
package org.erlide.jinterface;

import org.erlide.jinterface.java.ErlRpcTest;
import org.erlide.jinterface.java.JInterfaceTest;
import org.erlide.jinterface.java.PatternMatchTest;
import org.erlide.runtime.backend.RuntimeInfoTest;
import org.erlide.runtime.backend.RuntimeVersionTest;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.ericsson.otp.erlang.RpcConverterTest;

@RunWith(Suite.class)
@SuiteClasses( { PatternMatchTest.class, JInterfaceTest.class,
		RpcConverterTest.class, ErlRpcTest.class, RuntimeInfoTest.class,
		RuntimeVersionTest.class })
public class AllTests {

}
