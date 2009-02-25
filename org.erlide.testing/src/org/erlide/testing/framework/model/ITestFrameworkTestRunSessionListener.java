package org.erlide.testing.framework.model;

public interface ITestFrameworkTestRunSessionListener {

	/**
	 * @param testRunSession the new session, or <code>null</code>
	 */
	void sessionAdded(TestFrameworkTestRunSession testRunSession);

	void sessionRemoved(TestFrameworkTestRunSession testRunSession);
	
}
