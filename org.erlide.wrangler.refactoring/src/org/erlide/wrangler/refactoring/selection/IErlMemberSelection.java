package org.erlide.wrangler.refactoring.selection;

import org.eclipse.jface.text.IDocument;
import org.erlide.wrangler.refactoring.util.IErlRange;

public interface IErlMemberSelection extends IErlSelection {
	public IErlRange getSelectionRange();

	public IErlRange getMemberRange();

	public IDocument getDocument();

}
