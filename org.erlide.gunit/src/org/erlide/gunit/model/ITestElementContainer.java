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
package org.erlide.gunit.model;

/**
 * Common protocol for test elements containers. This set consists of
 * {@link ITestSuiteElement} and {@link ITestRunSession}
 * 
 * <p>
 * This interface is not intended to be implemented by clients.
 * </p>
 * 
 * @since 3.3
 */
public interface ITestElementContainer extends ITestElement {

	/**
	 * Returns all tests (and test suites) contained in the suite.
	 * 
	 * @return returns all tests (and test suites) contained in the suite.
	 */
	public ITestElement[] getChildren();

}
