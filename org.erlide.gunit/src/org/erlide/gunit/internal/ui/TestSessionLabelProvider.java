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

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.erlide.gunit.internal.Messages;
import org.erlide.gunit.internal.model.TestCaseElement;
import org.erlide.gunit.model.ITestCaseElement;
import org.erlide.gunit.model.ITestElement;
import org.erlide.gunit.model.ITestRunSession;
import org.erlide.gunit.model.ITestSuiteElement;
import org.erlide.gunit.model.TestSuiteElement;
import org.erlide.gunit.model.TestElement.Status;

public class TestSessionLabelProvider extends LabelProvider {

	private final TestRunnerViewPart fTestRunnerPart;

	private final int fLayoutMode;

	public TestSessionLabelProvider(final TestRunnerViewPart testRunnerPart,
			final int layoutMode) {
		this.fTestRunnerPart = testRunnerPart;
		this.fLayoutMode = layoutMode;
	}

	private String getSimpleLabel(final Object element) {
		if (element instanceof ITestCaseElement) {
			return ((ITestCaseElement) element).getTestMethodName();
		} else if (element instanceof ITestSuiteElement) {
			return ((ITestSuiteElement) element).getSuiteTypeName();
		}
		return null;
	}

	@Override
	public String getText(final Object element) {
		final String label = getSimpleLabel(element);
		if (label == null) {
			return element.toString();
		}
		if (this.fLayoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL) {
			if (((ITestElement) element).getParentContainer() instanceof ITestRunSession) {
				final String testKindDisplayName = this.fTestRunnerPart
				.getTestKindDisplayName();
				if (testKindDisplayName != null) {
					return Messages
					.format(
							GUnitMessages.TestSessionLabelProvider_testName_JUnitVersion,
							new Object[] { label, testKindDisplayName });
				}
			}
		} else {
			if (element instanceof ITestCaseElement) {
				final String className = ((ITestCaseElement) element)
				.getTestClassName();
				return Messages
				.format(
						GUnitMessages.TestSessionLabelProvider_testMethodName_className,
						new Object[] { label, className });
			}
		}
		return label;
	}

	@Override
	public Image getImage(final Object element) {
		if (element instanceof TestCaseElement) {
			final TestCaseElement testCaseElement = ((TestCaseElement) element);
			if (testCaseElement.isIgnored()) {
				return this.fTestRunnerPart.fTestIgnoredIcon;
			}

			final Status status = testCaseElement.getStatus();
			if (status.isNotRun()) {
				return this.fTestRunnerPart.fTestIcon;
			} else if (status.isRunning()) {
				return this.fTestRunnerPart.fTestRunningIcon;
			} else if (status.isError()) {
				return this.fTestRunnerPart.fTestErrorIcon;
			} else if (status.isFailure()) {
				return this.fTestRunnerPart.fTestFailIcon;
			} else if (status.isOK()) {
				return this.fTestRunnerPart.fTestOkIcon;
			} else {
				throw new IllegalStateException(element.toString());
			}

		} else if (element instanceof TestSuiteElement) {
			final Status status = ((TestSuiteElement) element).getStatus();
			if (status.isNotRun()) {
				return this.fTestRunnerPart.fSuiteIcon;
			} else if (status.isRunning()) {
				return this.fTestRunnerPart.fSuiteRunningIcon;
			} else if (status.isError()) {
				return this.fTestRunnerPart.fSuiteErrorIcon;
			} else if (status.isFailure()) {
				return this.fTestRunnerPart.fSuiteFailIcon;
			} else if (status.isOK()) {
				return this.fTestRunnerPart.fSuiteOkIcon;
			} else {
				throw new IllegalStateException(element.toString());
			}

		} else {
			throw new IllegalArgumentException(String.valueOf(element));
		}
	}

}
