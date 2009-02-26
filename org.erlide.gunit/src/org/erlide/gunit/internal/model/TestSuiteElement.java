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

import java.util.ArrayList;
import java.util.List;

import org.erlide.gunit.model.ITestElement;
import org.erlide.gunit.model.ITestSuiteElement;

public class TestSuiteElement extends TestElement implements ITestSuiteElement {

	private List/* <TestElement> */fChildren;
	private Status fChildrenStatus;

	public TestSuiteElement(TestSuiteElement parent, String id,
			String testName, int childrenCount) {
		super(parent, id, testName);
		fChildren = new ArrayList(childrenCount);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.gunit.ITestElement#getTestResult()
	 */
	@Override
	public Result getTestResult(boolean includeChildren) {
		if (includeChildren) {
			return getStatus().convertToResult();
		} else {
			return super.getStatus().convertToResult();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.gunit.ITestSuiteElement#getSuiteTypeName()
	 */
	public String getSuiteTypeName() {
		return getClassName();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.gunit.model.ITestSuiteElement#getChildren()
	 */
	public ITestElement[] getChildren() {
		return (ITestElement[]) fChildren.toArray(new ITestElement[fChildren
				.size()]);
	}

	public void addChild(TestElement child) {
		fChildren.add(child);
	}

	@Override
	public Status getStatus() {
		Status suiteStatus = getSuiteStatus();
		if (fChildrenStatus != null) {
			// must combine children and suite status here, since failures can
			// occur e.g. in @AfterClass
			return Status.combineStatus(fChildrenStatus, suiteStatus);
		} else {
			return suiteStatus;
		}
	}

	private Status getCumulatedStatus() {
		TestElement[] children = (TestElement[]) fChildren
				.toArray(new TestElement[fChildren.size()]); // copy list to
																// avoid
																// concurreny
																// problems
		if (children.length == 0)
			return getSuiteStatus();

		Status cumulated = children[0].getStatus();

		for (int i = 1; i < children.length; i++) {
			Status childStatus = children[i].getStatus();
			cumulated = Status.combineStatus(cumulated, childStatus);
		}
		// not necessary, see special code in Status.combineProgress()
		// if (suiteStatus.isErrorOrFailure() && cumulated.isNotRun())
		// return suiteStatus; //progress is Done if error in Suite and no
		// children run
		return cumulated;
	}

	public Status getSuiteStatus() {
		return super.getStatus();
	}

	public void childChangedStatus(TestElement child, Status childStatus) {
		int childCount = fChildren.size();
		if (child == fChildren.get(0) && childStatus.isRunning()) {
			// is first child, and is running -> copy status
			internalSetChildrenStatus(childStatus);
			return;
		}
		TestElement lastChild = (TestElement) fChildren.get(childCount - 1);
		if (child == lastChild) {
			if (childStatus.isDone()) {
				// all children done, collect cumulative status
				internalSetChildrenStatus(getCumulatedStatus());
				return;
			}
			// go on (child could e.g. be a TestSuiteElement with
			// RUNNING_FAILURE)

		} else if (!lastChild.getStatus().isNotRun()) {
			// child is not last, but last child has been run -> child has been
			// rerun or is rerunning
			internalSetChildrenStatus(getCumulatedStatus());
			return;
		}

		// finally, set RUNNING_FAILURE/ERROR if child has failed but suite has
		// not failed:
		if (childStatus.isFailure()) {
			if (fChildrenStatus == null || !fChildrenStatus.isErrorOrFailure()) {
				internalSetChildrenStatus(Status.RUNNING_FAILURE);
				return;
			}
		} else if (childStatus.isError()) {
			if (fChildrenStatus == null || !fChildrenStatus.isError()) {
				internalSetChildrenStatus(Status.RUNNING_ERROR);
				return;
			}
		}
	}

	private void internalSetChildrenStatus(Status status) {
		if (fChildrenStatus == status)
			return;
		fChildrenStatus = status;
		TestSuiteElement parent = getParent();
		if (parent != null)
			parent.childChangedStatus(this, getStatus());
	}

	@Override
	public String toString() {
		return "TestSuite: " + getSuiteTypeName() + " : " + super.toString() + " (" + fChildren.size() + ")"; //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
	}

}
