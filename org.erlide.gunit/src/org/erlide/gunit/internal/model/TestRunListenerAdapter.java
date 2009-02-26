/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.gunit.internal.model;

import org.erlide.gunit.TestRunListener;
import org.erlide.gunit.internal.model.TestElement.Status;
import org.erlide.gunit.internal.ui.GUnitPlugin;
import org.erlide.gunit.model.ITestCaseElement;

/**
 * Notifier for the callback listener API {@link TestRunListener}.
 */
public class TestRunListenerAdapter implements ITestSessionListener {

	private final TestRunSession fSession;

	public TestRunListenerAdapter(TestRunSession session) {
		this.fSession = session;
	}

	private Object[] getListeners() {
		return GUnitPlugin.getDefault().getNewTestRunListeners().getListeners();
	}

	private void fireSessionStarted() {
		Object[] listeners = getListeners();
		for (int i = 0; i < listeners.length; i++) {
			((TestRunListener) listeners[i]).sessionStarted(this.fSession);
		}
	}

	private void fireSessionFinished() {
		Object[] listeners = getListeners();
		for (int i = 0; i < listeners.length; i++) {
			((TestRunListener) listeners[i]).sessionFinished(this.fSession);
		}
	}

	private void fireTestCaseStarted(ITestCaseElement testCaseElement) {
		Object[] listeners = getListeners();
		for (int i = 0; i < listeners.length; i++) {
			((TestRunListener) listeners[i]).testCaseStarted(testCaseElement);
		}
	}

	private void fireTestCaseFinished(ITestCaseElement testCaseElement) {
		Object[] listeners = getListeners();
		for (int i = 0; i < listeners.length; i++) {
			((TestRunListener) listeners[i]).testCaseFinished(testCaseElement);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.erlide.gunit.internal.model.ITestSessionListener#sessionStarted()
	 */
	public void sessionStarted() {
		// wait until all test are added
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.erlide.gunit.internal.model.ITestSessionListener#sessionEnded(long)
	 */
	public void sessionEnded(long elapsedTime) {
		fireSessionFinished();
		this.fSession.swapOut();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.erlide.gunit.internal.model.ITestSessionListener#sessionStopped(long)
	 */
	public void sessionStopped(long elapsedTime) {
		fireSessionFinished();
		this.fSession.swapOut();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.erlide.gunit.internal.model.ITestSessionListener#sessionTerminated()
	 */
	public void sessionTerminated() {
		this.fSession.swapOut();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.erlide.gunit.internal.model.ITestSessionListener#testAdded(org.erlide
	 * .gunit.internal.model.TestElement)
	 */
	public void testAdded(TestElement testElement) {
		// do nothing
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.gunit.internal.model.ITestSessionListener#runningBegins()
	 */
	public void runningBegins() {
		fireSessionStarted();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.erlide.gunit.internal.model.ITestSessionListener#testStarted(org.
	 * erlide.gunit.internal.model.TestCaseElement)
	 */
	public void testStarted(TestCaseElement testCaseElement) {
		fireTestCaseStarted(testCaseElement);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.erlide.gunit.internal.model.ITestSessionListener#testEnded(org.erlide
	 * .gunit.internal.model.TestCaseElement)
	 */
	public void testEnded(TestCaseElement testCaseElement) {
		fireTestCaseFinished(testCaseElement);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.erlide.gunit.internal.model.ITestSessionListener#testFailed(org.erlide
	 * .gunit.internal.model.TestElement,
	 * org.erlide.gunit.internal.model.TestElement.Status, java.lang.String,
	 * java.lang.String, java.lang.String)
	 */
	public void testFailed(TestElement testElement, Status status,
			String trace, String expected, String actual) {
		// ignore
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.erlide.gunit.internal.model.ITestSessionListener#testReran(org.erlide
	 * .gunit.internal.model.TestCaseElement,
	 * org.erlide.gunit.internal.model.TestElement.Status, java.lang.String,
	 * java.lang.String, java.lang.String)
	 */
	public void testReran(TestCaseElement testCaseElement, Status status,
			String trace, String expectedResult, String actualResult) {
		// ignore
	}

	public boolean acceptsSwapToDisk() {
		return true;
	}
}
