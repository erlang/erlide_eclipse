/**
 *
 */
package org.erlide.core.erlang.internal;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlComment;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.ISourceRange;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * @author jakob
 * 
 */
public class ErlComment extends SourceRefElement implements IErlComment {

	private final boolean fIsEdoc;

	private final boolean fIsHeader;

	private int fLevel;

	public ErlComment(IErlElement parent, String name, boolean isEdoc,
			boolean isHeader) {
		super(parent, name);
		fIsEdoc = isEdoc;
		fIsHeader = isHeader;
	}

	public String getHoverHelp() {
		// TODO Auto-generated method stub
		return null;
	}

	public ISourceRange getNameRange() {
		// TODO Auto-generated method stub
		return null;
	}

	public OtpErlangObject getParseTree() {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean isEdoc() {
		return fIsEdoc;
	}

	public boolean isHeader() {
		return fIsHeader;
	}

	@Override
	protected void closing(Object info) throws ErlModelException {
		// TODO Auto-generated method stub

	}

	@Override
	public void copy(IErlElement container, IErlElement sibling, String rename,
			boolean replace, IProgressMonitor monitor) throws ErlModelException {
		// TODO Auto-generated method stub

	}

	@Override
	public void delete(boolean force, IProgressMonitor monitor)
			throws ErlModelException {
		// TODO Auto-generated method stub

	}

	@Override
	public void move(IErlElement container, IErlElement sibling, String rename,
			boolean replace, IProgressMonitor monitor) throws ErlModelException {
		// TODO Auto-generated method stub

	}

	@Override
	public void rename(String name, boolean replace, IProgressMonitor monitor)
			throws ErlModelException {
		// TODO Auto-generated method stub

	}

	@Override
	public IResource getCorrespondingResource() throws ErlModelException {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @see org.erlide.core.erlang.IErlElement#getElementType()
	 */
	public ErlElementType getElementType() {
		return ErlElementType.COMMENT;
	}

	@Override
	public IResource getResource() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public IResource getUnderlyingResource() throws ErlModelException {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean isVisibleInOutline() {
		return false;
	}

	public int getLevel() {
		return fLevel;
	}

	// public ISourceRange getSourceRange() throws ErlModelException
	// {
	// return new SourceRange(fSourceRangeStart, fSourceRangeEnd -
	// fSourceRangeStart + 1);
	// }
	@Override
	public String toString() {
		String result = "<comment";
		if (isEdoc()) {
			result = result + ":edoc";
		}
		if (isHeader()) {
			result = result + ":header";
		}
		return result + ">";
	}

	public void setNameRangeStartEnd(int start, int end) {
		// TODO Auto-generated method stub

	}
}
