package org.erlide.testing.framework.model;

import java.util.ArrayList;
import java.util.List;

public class TestFrameworkTestElement {

	public static final int STATUS_NOT_RUN = 0;
	public static final int STATUS_RUNNING = 1;
	public static final int STATUS_OK = 2;
	public static final int STATUS_FAILED = 3;

	private String fName;
	private int fStatus = STATUS_NOT_RUN;
	private List<TestFrameworkTestElement> fChildren;
	private TestFrameworkTestElement fParent;

	public TestFrameworkTestElement(String name, TestFrameworkTestElement parent) {
		fName = name;
		fParent = parent;
		fChildren = new ArrayList<TestFrameworkTestElement>();
	}

	public void addChild(TestFrameworkTestElement testElement) {
		fChildren.add(testElement);
	}

	public List<TestFrameworkTestElement> getChildren() {
		return fChildren;
	}

	public TestFrameworkTestElement getParent() {
		return fParent;
	}

	public boolean hasChildren() {
		return !fChildren.isEmpty();
	}

	public int getStatus() {
		return fStatus;
	}

	public void setStatus(int status) {
		fStatus = status;
	}

	@Override
	public String toString() {
		return fName;
	}

	public void addChildren(List<TestFrameworkTestElement> testCases) {
		fChildren = testCases;
	}

	public String getName() {
		return fName;
	}

}
