/**
 * 
 */
package org.erlide.wrangler.refactoring.duplicatedcode.ui.elements;

import java.util.ArrayList;

public abstract class AbstractResultTreeParent extends AbstractResultTreeObject {
	protected ArrayList<AbstractResultTreeObject> children = null;

	/*
	 * public ResultTreeParent(String name) { super(name); children = new
	 * ArrayList(); }
	 */

	public void addChild(AbstractResultTreeObject child) {
		if (children == null) {
			children = new ArrayList<AbstractResultTreeObject>();
		}
		children.add(child);
		child.setParent(this);
	}

	public void removeChild(AbstractResultTreeObject child) {
		if (children != null) {
			children.remove(child);
			child.setParent(null);
		}
	}

	public AbstractResultTreeObject[] getChildren() {
		if (children != null) {
			return children.toArray(new AbstractResultTreeObject[children
					.size()]);
		} else {
			return new AbstractResultTreeObject[0];
		}
	}

	public boolean hasChildren() {
		return children != null && children.size() > 0;
	}

}