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

import java.io.IOException;

import org.eclipse.core.runtime.Assert;
import org.erlide.core.erlang.IErlProject;
import org.erlide.gunit.model.ITestElement;
import org.erlide.gunit.model.TestRoot;
import org.erlide.gunit.model.TestSuiteElement;
import org.erlide.gunit.model.ITestElement.FailureTrace;
import org.erlide.gunit.model.ITestElement.ProgressState;
import org.erlide.gunit.model.ITestElement.Result;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.DTDHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.AttributesImpl;

public class TestRunSessionSerializer implements XMLReader {

	private static final String EMPTY = ""; //$NON-NLS-1$

	private static final String CDATA = "CDATA"; //$NON-NLS-1$

	private static final Attributes NO_ATTS = new AttributesImpl();

	private final TestRunSession fTestRunSession;

	private ContentHandler fHandler;

	private ErrorHandler fErrorHandler;

	/**
	 * @param testRunSession
	 *            the test run session to serialize
	 */
	public TestRunSessionSerializer(final TestRunSession testRunSession) {
		Assert.isNotNull(testRunSession);
		this.fTestRunSession = testRunSession;
	}

	public void parse(final InputSource input) throws IOException, SAXException {
		if (this.fHandler == null) {
			throw new SAXException("ContentHandler missing"); //$NON-NLS-1$
		}

		this.fHandler.startDocument();
		handleTestRun(this.fTestRunSession);
		this.fHandler.endDocument();
	}

	private void handleTestRun(final TestRunSession testRunSession)
	throws SAXException {
		final AttributesImpl atts = new AttributesImpl();
		addCDATA(atts, IXMLTags.ATTR_NAME, this.fTestRunSession
				.getTestRunName());
		final IErlProject project = this.fTestRunSession.getLaunchedProject();
		if (project != null) {
			addCDATA(atts, IXMLTags.ATTR_PROJECT, project.getName());
		}
		addCDATA(atts, IXMLTags.ATTR_TESTS, this.fTestRunSession
				.getTotalCount());
		addCDATA(atts, IXMLTags.ATTR_STARTED, this.fTestRunSession
				.getStartedCount());
		addCDATA(atts, IXMLTags.ATTR_FAILURES, this.fTestRunSession
				.getFailureCount());
		addCDATA(atts, IXMLTags.ATTR_ERRORS, this.fTestRunSession
				.getErrorCount());
		addCDATA(atts, IXMLTags.ATTR_IGNORED, this.fTestRunSession
				.getIgnoredCount());
		startElement(IXMLTags.NODE_TESTRUN, atts);

		final TestRoot testRoot = this.fTestRunSession.getTestRoot();
		final ITestElement[] topSuites = testRoot.getChildren();
		for (int i = 0; i < topSuites.length; i++) {
			handleTestElement(topSuites[i]);
		}

		endElement(IXMLTags.NODE_TESTRUN);
	}

	private void handleTestElement(final ITestElement testElement)
	throws SAXException {
		if (testElement instanceof TestSuiteElement) {
			final TestSuiteElement testSuiteElement = (TestSuiteElement) testElement;

			final AttributesImpl atts = new AttributesImpl();
			addCDATA(atts, IXMLTags.ATTR_NAME, testSuiteElement
					.getSuiteTypeName());
			// addCDATA(atts, IXMLTags.ATTR_TIME,
			// Integer.toString(testCaseElement.getTime()));
			if (testElement.getProgressState() != ProgressState.COMPLETED
					|| testElement.getTestResult(false) != Result.UNDEFINED) {
				addCDATA(atts, IXMLTags.ATTR_INCOMPLETE, Boolean.TRUE
						.toString());
			}

			startElement(IXMLTags.NODE_TESTSUITE, atts);
			addFailure(testElement);

			final ITestElement[] children = testSuiteElement.getChildren();
			for (int i = 0; i < children.length; i++) {
				handleTestElement(children[i]);
			}
			endElement(IXMLTags.NODE_TESTSUITE);

		} else if (testElement instanceof TestCaseElement) {
			final TestCaseElement testCaseElement = (TestCaseElement) testElement;

			final AttributesImpl atts = new AttributesImpl();
			addCDATA(atts, IXMLTags.ATTR_NAME, testCaseElement
					.getTestMethodName());
			addCDATA(atts, IXMLTags.ATTR_CLASSNAME, testCaseElement
					.getClassName());
			// addCDATA(atts, IXMLTags.ATTR_TIME,
			// Integer.toString(testCaseElement.getTime()));
			if (testElement.getProgressState() != ProgressState.COMPLETED) {
				addCDATA(atts, IXMLTags.ATTR_INCOMPLETE, Boolean.TRUE
						.toString());
			}
			if (testCaseElement.isIgnored()) {
				addCDATA(atts, IXMLTags.ATTR_IGNORED, Boolean.TRUE.toString());
			}

			startElement(IXMLTags.NODE_TESTCASE, atts);
			addFailure(testElement);

			endElement(IXMLTags.NODE_TESTCASE);

		} else {
			throw new IllegalStateException(String.valueOf(testElement));
		}

	}

	private void addFailure(final ITestElement testElement) throws SAXException {
		final FailureTrace failureTrace = testElement.getFailureTrace();
		if (failureTrace != null) {
			final AttributesImpl failureAtts = new AttributesImpl();
			// addCDATA(failureAtts, IXMLTags.ATTR_MESSAGE, xx);
			// addCDATA(failureAtts, IXMLTags.ATTR_TYPE, xx);
			final String failureKind = testElement.getTestResult(false) == Result.ERROR ? IXMLTags.NODE_ERROR
					: IXMLTags.NODE_FAILURE;
			startElement(failureKind, failureAtts);
			final String expected = failureTrace.getExpected();
			final String actual = failureTrace.getActual();
			if (expected != null) {
				startElement(IXMLTags.NODE_EXPECTED, NO_ATTS);
				this.fHandler.characters(expected.toCharArray(), 0, expected
						.length());
				endElement(IXMLTags.NODE_EXPECTED);
			}
			if (actual != null) {
				startElement(IXMLTags.NODE_ACTUAL, NO_ATTS);
				this.fHandler.characters(actual.toCharArray(), 0, actual
						.length());
				endElement(IXMLTags.NODE_ACTUAL);
			}
			final String trace = failureTrace.getTrace();
			this.fHandler.characters(trace.toCharArray(), 0, trace.length());
			endElement(failureKind);
		}
	}

	private void startElement(final String name, final Attributes atts) throws SAXException {
		this.fHandler.startElement(EMPTY, name, name, atts);
	}

	private void endElement(final String name) throws SAXException {
		this.fHandler.endElement(EMPTY, name, name);
	}

	private static void addCDATA(final AttributesImpl atts, final String name, final int value) {
		addCDATA(atts, name, Integer.toString(value));
	}

	private static void addCDATA(final AttributesImpl atts, final String name, final String value) {
		atts.addAttribute(EMPTY, EMPTY, name, CDATA, value);
	}

	public void setContentHandler(final ContentHandler handler) {
		this.fHandler = handler;
	}

	public ContentHandler getContentHandler() {
		return this.fHandler;
	}

	public void setErrorHandler(final ErrorHandler handler) {
		this.fErrorHandler = handler;
	}

	public ErrorHandler getErrorHandler() {
		return this.fErrorHandler;
	}

	// ignored:

	public void parse(final String systemId) throws IOException, SAXException {
	}

	public void setDTDHandler(final DTDHandler handler) {
	}

	public DTDHandler getDTDHandler() {
		return null;
	}

	public void setEntityResolver(final EntityResolver resolver) {
	}

	public EntityResolver getEntityResolver() {
		return null;
	}

	public void setProperty(final java.lang.String name, final java.lang.Object value) {
	}

	public Object getProperty(final java.lang.String name) {
		return null;
	}

	public void setFeature(final java.lang.String name, final boolean value) {
	}

	public boolean getFeature(final java.lang.String name) {
		return false;
	}
}
