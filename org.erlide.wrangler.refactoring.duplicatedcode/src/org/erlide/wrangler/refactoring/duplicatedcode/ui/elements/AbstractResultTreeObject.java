/**
 * 
 */
package org.erlide.wrangler.refactoring.duplicatedcode.ui.elements;

import org.eclipse.core.runtime.IAdaptable;

public abstract class AbstractResultTreeObject implements IAdaptable {
	// private String name;
	private AbstractResultTreeParent parent;

	/*
	 * public abstract ResultTreeObject(String name); { this.name = name; }
	 */

	public abstract String getName(); /*
									 * { return name; }
									 */

	public void setParent(AbstractResultTreeParent parent) {
		this.parent = parent;
	}

	public AbstractResultTreeParent getParent() {
		return parent;
	}

	@Override
	public String toString() {
		return getName();
	}

	@SuppressWarnings("unchecked")
	public Object getAdapter(Class adapter) {
		return null;
	}

	String suggestedCode = "";

	public String getSuggestedCode() {
		return suggestedCode;
	}

	public void setSuggestedCode(String str) {
		this.suggestedCode = str;
	}

}