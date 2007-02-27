package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlMember;
import org.erlide.core.erlang.ISourceRange;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * 
 * @author Vlad Dumitrescu
 */
public abstract class ErlMember extends SourceRefElement implements IErlMember {

	protected ErlMember(IErlElement parent, String name) {
		super(parent, name);
	}

	private OtpErlangObject fTree;

	public void setParseTree(OtpErlangObject tree) {
		fTree = tree;
	}

	public OtpErlangObject getParseTree() {
		return fTree;
	}

	public String getHoverHelp() {
		System.out.println("> hover help: " + this.getElementName());

		return toString();
	}

	public boolean isVisibleInOutline() {
		return true;
	}

	public ISourceRange getNameRange() {
		// TODO fix this
		return new SourceRange(this.getSourceRangeStart(), this
				.getSourceRangeEnd()
				- this.getSourceRangeStart() + 1);
	}

}
