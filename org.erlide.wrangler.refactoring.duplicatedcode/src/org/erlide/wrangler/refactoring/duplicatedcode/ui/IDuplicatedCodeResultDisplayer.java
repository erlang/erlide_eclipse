package org.erlide.wrangler.refactoring.duplicatedcode.ui;

import java.util.List;

import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;

public interface IDuplicatedCodeResultDisplayer {
	public void showResult(List<DuplicatedCodeElement> result);

}
