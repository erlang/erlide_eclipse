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

import org.eclipse.jface.action.Action;
import org.eclipse.ui.PlatformUI;

/**
 * Requests to rerun a test.
 */
public class RerunAction extends Action {
	private final String fTestId;

	private final String fClassName;

	private final String fTestName;

	private final TestRunnerViewPart fTestRunner;

	private final String fLaunchMode;

	/**
	 * Constructor for RerunAction.
	 */
	public RerunAction(final String actionName, final TestRunnerViewPart runner,
			final String testId, final String className, final String testName, final String launchMode) {
		super(actionName);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(this,
				IGUnitHelpContextIds.RERUN_ACTION);
		this.fTestRunner = runner;
		this.fTestId = testId;
		this.fClassName = className;
		this.fTestName = testName;
		this.fLaunchMode = launchMode;
	}

	/*
	 * @see IAction#run()
	 */
	@Override
	public void run() {
		this.fTestRunner.rerunTest(this.fTestId, this.fClassName,
				this.fTestName, this.fLaunchMode);
	}
}
