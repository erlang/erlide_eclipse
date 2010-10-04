package org.erlide.testing;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses( { org.erlide.core.TestAll.class,
		org.erlide.jinterface.AllTests.class, org.erlide.ui.AllTests.class })
public class AllTests {

//	// FIXME this does only work with junit 3 !!!!
//	public static Test suite() throws ClassNotFoundException {
//		TestSuite suite = new TestSuite("Master test suite.");
//
//		suite.addTest(getTest("org.erlide.core.TestAll"));
//		suite.addTest(getTest("org.erlide.jinterface.AllTests"));
//		suite.addTest(getTest("org.erlide.ui.AllTests"));
//		return suite;
//	}
//
//	private static Test getTest(String suiteClassName) {
//		try {
//			Class<?> clazz = Class.forName(suiteClassName);
//			Method suiteMethod = clazz.getMethod("suite", new Class[0]);
//			return (Test) suiteMethod.invoke(null, new Object[0]);
//		} catch (Exception e) {
//			throw new RuntimeException("Error", e);
//		}
//	}
}
