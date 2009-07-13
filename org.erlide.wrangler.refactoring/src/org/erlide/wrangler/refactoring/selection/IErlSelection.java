package org.erlide.wrangler.refactoring.selection;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ISelection;
import org.erlide.core.erlang.IErlElement;

import com.ericsson.otp.erlang.OtpErlangList;

public interface IErlSelection extends ISelection {
	public enum SelectionKind {
		MODULE, FUNCTION, FUNCTION_CLAUSE, EXPRESSION, VARIABLE;
	}

	public SelectionKind getKind();

	public SelectionKind getDetailedKind();

	public String getFilePath();

	public IFile getFile();

	public OtpErlangList getSearchPath();

	public IErlElement getErlElement();

}
