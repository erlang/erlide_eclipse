package org.erlide.gunit.model;

import java.util.ArrayList;
import java.util.List;

public class TestElement {

	public static final int STATUS_NOT_RUN = 0;
	public static final int STATUS_RUNNING = 1;
	public static final int STATUS_OK = 2;
	public static final int STATUS_FAILED = 3;

	private String fName;
	private int fStatus = STATUS_NOT_RUN;
	private List<TestElement> fChildren;
	private TestElement fParent;

	public TestElement(String name, TestElement parent) {
		fName = name;
		fParent = parent;
		fChildren = new ArrayList<TestElement>();
	}

	public void addChild(TestElement testElement) {
		fChildren.add(testElement);
	}

	public List<TestElement> getChildren() {
		return fChildren;
	}

	public TestElement getParent() {
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

	public void addChildren(List<TestElement> testCases) {
		fChildren = testCases;
	}

	public String getName() {
		return fName;
	}

}
