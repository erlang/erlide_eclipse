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

public interface IXMLTags {

	public static final String NODE_TESTRUN = "testrun"; //$NON-NLS-1$
	public static final String NODE_TESTSUITES = "testsuites"; //$NON-NLS-1$
	public static final String NODE_TESTSUITE = "testsuite"; //$NON-NLS-1$
	public static final String NODE_PROPERTIES = "properties"; //$NON-NLS-1$
	public static final String NODE_PROPERTY = "property"; //$NON-NLS-1$
	public static final String NODE_TESTCASE = "testcase"; //$NON-NLS-1$
	public static final String NODE_ERROR = "error"; //$NON-NLS-1$
	public static final String NODE_FAILURE = "failure"; //$NON-NLS-1$
	public static final String NODE_EXPECTED = "expected"; //$NON-NLS-1$
	public static final String NODE_ACTUAL = "actual"; //$NON-NLS-1$
	public static final String NODE_SYSTEM_OUT = "system-out"; //$NON-NLS-1$
	public static final String NODE_SYSTEM_ERR = "system-err"; //$NON-NLS-1$

	/**
	 * value: String
	 */
	public static final String ATTR_NAME = "name"; //$NON-NLS-1$
	/**
	 * value: String
	 */
	public static final String ATTR_PROJECT = "project"; //$NON-NLS-1$
	/**
	 * value: Integer
	 */
	public static final String ATTR_TESTS = "tests"; //$NON-NLS-1$
	/**
	 * value: Integer
	 */
	public static final String ATTR_STARTED = "started"; //$NON-NLS-1$
	/**
	 * value: Integer
	 */
	public static final String ATTR_FAILURES = "failures"; //$NON-NLS-1$
	/**
	 * value: Integer
	 */
	public static final String ATTR_ERRORS = "errors"; //$NON-NLS-1$
	/**
	 * value: Boolean
	 */
	public static final String ATTR_IGNORED = "ignored"; //$NON-NLS-1$
	/**
	 * value: String
	 */
	public static final String ATTR_PACKAGE = "package"; //$NON-NLS-1$
	/**
	 * value: String
	 */
	public static final String ATTR_ID = "id"; //$NON-NLS-1$
	/**
	 * value: String
	 */
	public static final String ATTR_CLASSNAME = "classname"; //$NON-NLS-1$
	/**
	 * value: Boolean
	 */
	public static final String ATTR_INCOMPLETE = "incomplete"; //$NON-NLS-1$

	//	public static final String ATTR_TIME= "time"; //$NON-NLS-1$
	//	public static final String ATTR_MESSAGE= "message"; //$NON-NLS-1$
	//	public static final String ATTR_TYPE= "type"; //$NON-NLS-1$
}
