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

/**
 * @author jakob
 * 
 */
public class ErlComment extends SourceRefElement implements IErlComment {

	private final boolean fIsEdoc;

	private final boolean fIsHeader;

	public ErlComment(final IErlElement parent, final String name,
			final boolean isEdoc, final boolean isHeader) {
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

	public boolean isEdoc() {
		return fIsEdoc;
	}

	public boolean isHeader() {
		return fIsHeader;
	}

	@Override
	protected void closing(final Object info) throws ErlModelException {
		// TODO Auto-generated method stub

	}

	@Override
	public void copy(final IErlElement container, final IErlElement sibling,
			final String rename, final boolean replace,
			final IProgressMonitor monitor) throws ErlModelException {
		// TODO Auto-generated method stub

	}

	@Override
	public void delete(final boolean force, final IProgressMonitor monitor)
			throws ErlModelException {
		// TODO Auto-generated method stub

	}

	@Override
	public void move(final IErlElement container, final IErlElement sibling,
			final String rename, final boolean replace,
			final IProgressMonitor monitor) throws ErlModelException {
		// TODO Auto-generated method stub

	}

	@Override
	public void rename(final String name, final boolean replace,
			final IProgressMonitor monitor) throws ErlModelException {
		// TODO Auto-generated method stub

	}

	@Override
	public IResource getCorrespondingResource() throws ErlModelException {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @see org.erlide.core.erlang.IErlElement#getKind()
	 */
	public Kind getKind() {
		return Kind.COMMENT;
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

	@Override
	public String toString() {
		String result = "<comment";
		if (isEdoc()) {
			result = result + ":edoc";
		}
		if (isHeader()) {
			result = result + ":header";
		}
		return result + ", line=" + (getLineStart() + 1) + ">";
	}

	public void setNameRangeStartEnd(final int start, final int end) {
		// TODO Auto-generated method stub

	}
}
