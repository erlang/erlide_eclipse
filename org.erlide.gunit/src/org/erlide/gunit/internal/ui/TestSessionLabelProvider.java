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

package org.erlide.gunit.internal.ui;

import org.eclipse.swt.graphics.Image;

import org.eclipse.jface.viewers.LabelProvider;

import org.erlide.gunit.internal.Messages;
import org.erlide.gunit.internal.model.TestCaseElement;
import org.erlide.gunit.internal.model.TestSuiteElement;
import org.erlide.gunit.internal.model.TestElement.Status;
import org.erlide.gunit.model.ITestCaseElement;
import org.erlide.gunit.model.ITestElement;
import org.erlide.gunit.model.ITestRunSession;
import org.erlide.gunit.model.ITestSuiteElement;

public class TestSessionLabelProvider extends LabelProvider implements
		IRichLabelProvider {

	private final TestRunnerViewPart fTestRunnerPart;
	private final int fLayoutMode;

	public TestSessionLabelProvider(TestRunnerViewPart testRunnerPart,
			int layoutMode) {
		fTestRunnerPart = testRunnerPart;
		fLayoutMode = layoutMode;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jdt.internal.ui.viewsupport.IRichLabelProvider#getRichTextLabel
	 * (java.lang.Object)
	 */
	public ColoredString getRichTextLabel(Object element) {
		String label = getSimpleLabel(element);
		if (label == null) {
			return new ColoredString(element.toString());
		}
		ColoredString text = new ColoredString(label);
		if (fLayoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL) {
			if (((ITestElement) element).getParentContainer() instanceof ITestRunSession) {
				String testKindDisplayName = fTestRunnerPart
						.getTestKindDisplayName();
				if (testKindDisplayName != null) {
					String decorated = Messages
							.format(
									JUnitMessages.TestSessionLabelProvider_testName_JUnitVersion,
									new Object[] { label, testKindDisplayName });
					return ColoredJavaElementLabels
							.decorateColoredString(text, decorated,
									ColoredJavaElementLabels.QUALIFIER_STYLE);
				}
			}
		} else {
			if (element instanceof ITestCaseElement) {
				String className = ((ITestCaseElement) element)
						.getTestClassName();
				String decorated = Messages
						.format(
								JUnitMessages.TestSessionLabelProvider_testMethodName_className,
								new Object[] { label, className });
				return ColoredJavaElementLabels.decorateColoredString(text,
						decorated, ColoredJavaElementLabels.QUALIFIER_STYLE);
			}
		}
		return text;
	}

	private String getSimpleLabel(Object element) {
		if (element instanceof ITestCaseElement) {
			return ((ITestCaseElement) element).getTestMethodName();
		} else if (element instanceof ITestSuiteElement) {
			return ((ITestSuiteElement) element).getSuiteTypeName();
		}
		return null;
	}

	@Override
	public String getText(Object element) {
		String label = getSimpleLabel(element);
		if (label == null) {
			return element.toString();
		}
		if (fLayoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL) {
			if (((ITestElement) element).getParentContainer() instanceof ITestRunSession) {
				String testKindDisplayName = fTestRunnerPart
						.getTestKindDisplayName();
				if (testKindDisplayName != null) {
					return Messages
							.format(
									JUnitMessages.TestSessionLabelProvider_testName_JUnitVersion,
									new Object[] { label, testKindDisplayName });
				}
			}
		} else {
			if (element instanceof ITestCaseElement) {
				String className = ((ITestCaseElement) element)
						.getTestClassName();
				return Messages
						.format(
								JUnitMessages.TestSessionLabelProvider_testMethodName_className,
								new Object[] { label, className });
			}
		}
		return label;
	}

	@Override
	public Image getImage(Object element) {
		if (element instanceof TestCaseElement) {
			TestCaseElement testCaseElement = ((TestCaseElement) element);
			if (testCaseElement.isIgnored())
				return fTestRunnerPart.fTestIgnoredIcon;

			Status status = testCaseElement.getStatus();
			if (status.isNotRun())
				return fTestRunnerPart.fTestIcon;
			else if (status.isRunning())
				return fTestRunnerPart.fTestRunningIcon;
			else if (status.isError())
				return fTestRunnerPart.fTestErrorIcon;
			else if (status.isFailure())
				return fTestRunnerPart.fTestFailIcon;
			else if (status.isOK())
				return fTestRunnerPart.fTestOkIcon;
			else
				throw new IllegalStateException(element.toString());

		} else if (element instanceof TestSuiteElement) {
			Status status = ((TestSuiteElement) element).getStatus();
			if (status.isNotRun())
				return fTestRunnerPart.fSuiteIcon;
			else if (status.isRunning())
				return fTestRunnerPart.fSuiteRunningIcon;
			else if (status.isError())
				return fTestRunnerPart.fSuiteErrorIcon;
			else if (status.isFailure())
				return fTestRunnerPart.fSuiteFailIcon;
			else if (status.isOK())
				return fTestRunnerPart.fSuiteOkIcon;
			else
				throw new IllegalStateException(element.toString());

		} else {
			throw new IllegalArgumentException(String.valueOf(element));
		}
	}

}
