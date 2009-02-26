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

package org.erlide.gunit.internal.ui;

import java.util.ArrayList;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.gunit.internal.model.TestCaseElement;
import org.erlide.gunit.internal.model.TestRoot;
import org.erlide.gunit.internal.model.TestSuiteElement;
import org.erlide.gunit.model.ITestElement;

public class TestSessionTableContentProvider implements
		IStructuredContentProvider {

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}

	public Object[] getElements(Object inputElement) {
		ArrayList<ITestElement> all = new ArrayList<ITestElement>();
		addAll(all, (TestRoot) inputElement);
		return all.toArray();
	}

	private void addAll(ArrayList<ITestElement> all, TestSuiteElement suite) {
		ITestElement[] children = suite.getChildren();
		for (int i = 0; i < children.length; i++) {
			ITestElement element = children[i];
			if (element instanceof TestSuiteElement) {
				if (((TestSuiteElement) element).getSuiteStatus()
						.isErrorOrFailure()) {
					all.add(element); // add failed suite to flat list too
				}
				addAll(all, (TestSuiteElement) element);
			} else if (element instanceof TestCaseElement) {
				all.add(element);
			}
		}
	}

	public void dispose() {
	}
}
