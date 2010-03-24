package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlMember;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.jinterface.util.ErlLogger;

/**
 * 
 * @author Vlad Dumitrescu
 */
public abstract class ErlMember extends SourceRefElement implements IErlMember {
	int fNameRangeOffset, fNameRangeLength;

	protected ErlMember(final IErlElement parent, final String name) {
		super(parent, name);
	}

	// private OtpErlangObject fTree;

	// public void setParseTree(OtpErlangObject tree) {
	// fTree = tree;
	// }

	// public OtpErlangObject getParseTree() {
	// return fTree;
	// }

	public boolean isVisibleInOutline() {
		return true;
	}

	public void setNameRange(final int offset, final int length) {
		fNameRangeOffset = offset;
		fNameRangeLength = length;
	}

	public ISourceRange getNameRange() {
		if (fNameRangeOffset == 0 && fNameRangeLength == 0) {
			try {
				return getSourceRange();
			} catch (ErlModelException e) {
				ErlLogger.error(e); // will never happen
			}
		}
		return new SourceRange(fNameRangeOffset, fNameRangeLength);
	}

}
