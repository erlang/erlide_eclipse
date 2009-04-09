package org.erlide.wrangler.refactoring.duplicatedcode.ui.elements;

public class DuplicatedCodeElement extends AbstractResultTreeParent {

	String codePartId;
	DuplicatedCodeInstanceElement defaultCodePart;

	public DuplicatedCodeElement(DuplicatedCodeInstanceElement defaultInstance) {
		this.defaultCodePart = defaultInstance;
	}

	@Override
	public String getName() {
		return "\"" + defaultCodePart.getCodePartString() + "\"";
	}

	public DuplicatedCodeInstanceElement getCodePart() {
		return defaultCodePart;
	}
}
