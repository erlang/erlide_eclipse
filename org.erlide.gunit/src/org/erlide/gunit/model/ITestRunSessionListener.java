package org.erlide.gunit.model;

public interface ITestRunSessionListener {

	/**
	 * @param testRunSession the new session, or <code>null</code>
	 */
	void sessionAdded(TestRunSession testRunSession);

	void sessionRemoved(TestRunSession testRunSession);
	
}
