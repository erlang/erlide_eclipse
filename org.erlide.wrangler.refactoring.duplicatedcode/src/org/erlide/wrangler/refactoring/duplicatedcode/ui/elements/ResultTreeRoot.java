package org.erlide.wrangler.refactoring.duplicatedcode.ui.elements;

public class ResultTreeRoot extends AbstractResultTreeParent {

	@Override
	public String getName() {
		return "";
	}

	public void dropChildren() {
		if (children != null)
			children.clear();
	}

}
