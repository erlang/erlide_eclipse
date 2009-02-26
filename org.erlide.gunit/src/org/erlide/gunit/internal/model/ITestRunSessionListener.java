/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.erlide.gunit.internal.model;

public interface ITestRunSessionListener {

	/**
	 * @param testRunSession
	 *            the new session, or <code>null</code>
	 */
	void sessionAdded(TestRunSession testRunSession);

	void sessionRemoved(TestRunSession testRunSession);

}
