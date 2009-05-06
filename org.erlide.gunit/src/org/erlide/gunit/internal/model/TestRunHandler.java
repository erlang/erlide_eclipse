/*******************************************************************************
 * Copyright (c) 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.erlide.gunit.internal.model;

import java.util.Stack;

import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlProject;
import org.erlide.gunit.internal.model.TestElement.Status;
import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

public class TestRunHandler extends DefaultHandler {

	/*
	 * TODO: validate (currently assumes correct XML)
	 */

	private int fId;

	private TestRunSession fTestRunSession;

	private TestSuiteElement fTestSuite;

	private TestCaseElement fTestCase;

	private final Stack<Boolean> fNotRun = new Stack<Boolean>();

	private StringBuffer fFailureBuffer;

	private boolean fInExpected;

	private boolean fInActual;

	private StringBuffer fExpectedBuffer;

	private StringBuffer fActualBuffer;

	private Locator fLocator;

	private Status fStatus;

	public TestRunHandler() {

	}

	public TestRunHandler(final TestRunSession testRunSession) {
		this.fTestRunSession = testRunSession;
	}

	@Override
	public void setDocumentLocator(final Locator locator) {
		this.fLocator = locator;
	}

	@Override
	public void startDocument() throws SAXException {
	}

	@Override
	public void startElement(final String uri, final String localName, final String qName,
			final Attributes attributes) throws SAXException {
		if (qName.equals(IXMLTags.NODE_TESTRUN)) {
			if (this.fTestRunSession == null) {
				final String name = attributes.getValue(IXMLTags.ATTR_NAME);
				final String project = attributes.getValue(IXMLTags.ATTR_PROJECT);
				IErlProject javaProject = null;
				if (project != null) {
					final IErlModel javaModel = ErlangCore.getModel();
					javaProject = javaModel.getErlangProject(project);
					if (!javaProject.exists()) {
						javaProject = null;
					}
				}
				this.fTestRunSession = new TestRunSession(name, javaProject);
				// TODO: read counts?

			} else {
				this.fTestRunSession.reset();
			}
			this.fTestSuite = this.fTestRunSession.getTestRoot();

		} else if (qName.equals(IXMLTags.NODE_TESTSUITES)) {
			// support Ant's 'junitreport' task; create suite from
			// NODE_TESTSUITE

		} else if (qName.equals(IXMLTags.NODE_TESTSUITE)) {
			final String name = attributes.getValue(IXMLTags.ATTR_NAME);

			if (this.fTestRunSession == null) {
				// support standalone suites and Ant's 'junitreport' task:
				this.fTestRunSession = new TestRunSession(name, null);
				this.fTestSuite = this.fTestRunSession.getTestRoot();
			}

			final String pack = attributes.getValue(IXMLTags.ATTR_PACKAGE);
			final String suiteName = pack == null ? name : pack + "." + name; //$NON-NLS-1$
			this.fTestSuite = (TestSuiteElement) this.fTestRunSession
			.createTestElement(this.fTestSuite, getNextId(), suiteName,
					true, 0);
			this.fNotRun.push(Boolean.valueOf(attributes
					.getValue(IXMLTags.ATTR_INCOMPLETE)));

		} else if (qName.equals(IXMLTags.NODE_PROPERTIES)
				|| qName.equals(IXMLTags.NODE_PROPERTY)) {
			// not interested

		} else if (qName.equals(IXMLTags.NODE_TESTCASE)) {
			final String name = attributes.getValue(IXMLTags.ATTR_NAME);
			final String classname = attributes.getValue(IXMLTags.ATTR_CLASSNAME);
			this.fTestCase = (TestCaseElement) this.fTestRunSession
			.createTestElement(this.fTestSuite, getNextId(), name + '('
					+ classname + ')', false, 0);
			this.fNotRun.push(Boolean.valueOf(attributes
					.getValue(IXMLTags.ATTR_INCOMPLETE)));
			this.fTestCase.setIgnored(Boolean.valueOf(
					attributes.getValue(IXMLTags.ATTR_IGNORED)).booleanValue());

		} else if (qName.equals(IXMLTags.NODE_ERROR)) {
			// TODO: multiple failures:
			// https://bugs.eclipse.org/bugs/show_bug.cgi?id=125296
			this.fStatus = Status.ERROR;
			this.fFailureBuffer = new StringBuffer();

		} else if (qName.equals(IXMLTags.NODE_FAILURE)) {
			// TODO: multiple failures:
			// https://bugs.eclipse.org/bugs/show_bug.cgi?id=125296
			this.fStatus = Status.FAILURE;
			this.fFailureBuffer = new StringBuffer();

		} else if (qName.equals(IXMLTags.NODE_EXPECTED)) {
			this.fInExpected = true;
			this.fExpectedBuffer = new StringBuffer();

		} else if (qName.equals(IXMLTags.NODE_ACTUAL)) {
			this.fInActual = true;
			this.fActualBuffer = new StringBuffer();

		} else if (qName.equals(IXMLTags.NODE_SYSTEM_OUT)
				|| qName.equals(IXMLTags.NODE_SYSTEM_ERR)) {
			// not interested

		} else {
			throw new SAXParseException(
					"unknown node '" + qName + "'", this.fLocator); //$NON-NLS-1$//$NON-NLS-2$
		}
	}

	@Override
	public void characters(final char[] ch, final int start, final int length)
	throws SAXException {
		if (this.fInExpected) {
			this.fExpectedBuffer.append(ch, start, length);

		} else if (this.fInActual) {
			this.fActualBuffer.append(ch, start, length);

		} else if (this.fFailureBuffer != null) {
			this.fFailureBuffer.append(ch, start, length);
		}
	}

	@Override
	public void endElement(final String uri, final String localName, final String qName)
	throws SAXException {
		if (qName.equals(IXMLTags.NODE_TESTRUN)) {
			// OK

		} else if (qName.equals(IXMLTags.NODE_TESTSUITES)) {
			// OK

		} else if (qName.equals(IXMLTags.NODE_TESTSUITE)) {
			handleTestElementEnd(this.fTestSuite);
			this.fTestSuite = this.fTestSuite.getParent();
			// TODO: end suite: compare counters?

		} else if (qName.equals(IXMLTags.NODE_PROPERTIES)
				|| qName.equals(IXMLTags.NODE_PROPERTY)) {
			// OK

		} else if (qName.equals(IXMLTags.NODE_TESTCASE)) {
			handleTestElementEnd(this.fTestCase);
			this.fTestCase = null;

		} else if (qName.equals(IXMLTags.NODE_FAILURE)
				|| qName.equals(IXMLTags.NODE_ERROR)) {
			TestElement testElement = this.fTestCase;
			if (testElement == null) {
				testElement = this.fTestSuite;
			}
			handleFailure(testElement);

		} else if (qName.equals(IXMLTags.NODE_EXPECTED)) {
			this.fInExpected = false;

		} else if (qName.equals(IXMLTags.NODE_ACTUAL)) {
			this.fInActual = false;

		} else if (qName.equals(IXMLTags.NODE_SYSTEM_OUT)
				|| qName.equals(IXMLTags.NODE_SYSTEM_ERR)) {
			// OK

		} else {

			handleUnknownNode(qName);
		}
	}

	private void handleTestElementEnd(final TestElement testElement) {
		final boolean completed = this.fNotRun.pop() != Boolean.TRUE;
		this.fTestRunSession.registerTestEnded(testElement, completed);
	}

	private void handleFailure(final TestElement testElement) {
		if (this.fFailureBuffer != null) {
			this.fTestRunSession.registerTestFailed(testElement, this.fStatus,
					this.fFailureBuffer.toString(),
					toString(this.fExpectedBuffer),
					toString(this.fActualBuffer));
			this.fFailureBuffer = null;
			this.fExpectedBuffer = null;
			this.fActualBuffer = null;
			this.fStatus = null;
		}
	}

	private String toString(final StringBuffer buffer) {
		return buffer != null ? buffer.toString() : null;
	}

	private void handleUnknownNode(final String qName) throws SAXException {
		// TODO: just log if debug option is enabled?
		String msg = "unknown node '" + qName + "'"; //$NON-NLS-1$//$NON-NLS-2$
		if (this.fLocator != null) {
			msg += " at line " + this.fLocator.getLineNumber() + ", column " + this.fLocator.getColumnNumber(); //$NON-NLS-1$//$NON-NLS-2$
		}
		throw new SAXException(msg);
	}

	@Override
	public void error(final SAXParseException e) throws SAXException {
		throw e;
	}

	@Override
	public void warning(final SAXParseException e) throws SAXException {
		throw e;
	}

	private String getNextId() {
		return Integer.toString(this.fId++);
	}

	/**
	 * @return the parsed test run session, or <code>null</code>
	 */
	public TestRunSession getTestRunSession() {
		return this.fTestRunSession;
	}
}
