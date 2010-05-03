package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlMember;
import org.erlide.core.erlang.ISourceRange;

/**
 * 
 * @author Vlad Dumitrescu
 */
public abstract class ErlMember extends SourceRefElement implements IErlMember {
	public static String uptoCommaOrParen(final String s) {
		if (s == null || s.length() == 0) {
			return s;
		}
		int i = 0;
		if (s.charAt(0) == '\'') {
			i = s.indexOf('\'', 1);
		}
		if (i == -1) {
			i = 0;
		}
		int j = s.indexOf(',', i);
		if (j == 0 || j == -1) {
			j = s.length();
		}
		final int k = s.indexOf('(', i);
		if (k < j && k > 0) {
			j = k;
		}
		return s.substring(0, j);
	}

	int fNameRangeStart, fNameRangeEnd;

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

	public void setNameRangeStartEnd(final int start, final int end) {
		fNameRangeStart = start;
		fNameRangeEnd = end;
	}

	public ISourceRange getNameRange() {
		if (fNameRangeStart == 0 && fNameRangeEnd == 0) {
			return new SourceRange(getSourceRangeStart(), getSourceRangeEnd()
					- getSourceRangeStart());
		}
		return new SourceRange(fNameRangeStart, fNameRangeEnd - fNameRangeStart);
	}

}
