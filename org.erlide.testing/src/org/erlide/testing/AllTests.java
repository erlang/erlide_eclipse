package org.erlide.testing;

import java.lang.reflect.Method;

import junit.framework.Test;
import junit.framework.TestSuite;

//@RunWith(Suite.class)
//@Suite.SuiteClasses( { org.erlide.core.AllTests.class })
public class AllTests {

	// FIXME this does only work with junit 3 !!!!
	public static Test suite() throws ClassNotFoundException {
		TestSuite suite = new TestSuite("Master test suite.");

		suite.addTest(getTest("org.erlide.core.AllTests"));
		suite.addTest(getTest("org.erlide.jinterface.AllTests"));
		suite.addTest(getTest("org.erlide.ui.AllTests"));
		return suite;
	}

	private static Test getTest(String suiteClassName) {
		try {
			Class<?> clazz = Class.forName(suiteClassName);
			Method suiteMethod = clazz.getMethod("suite", new Class[0]);
			return (Test) suiteMethod.invoke(null, new Object[0]);
		} catch (Exception e) {
			throw new RuntimeException("Error", e);
		}
	}
}
