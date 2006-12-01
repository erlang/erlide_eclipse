/**
 * 
 */
package org.erlide.core.erlang;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * @author jakob
 * 
 */
public interface IErlComment extends IErlMember, ISourceReference,
		ISourceManipulation {

	String getHoverHelp();

	/**
	 * Returns the compilation unit in which this member is declared, or
	 * <code>null</code> if this member is not declared in a compilation unit
	 * (for example, a binary type). This is a handle-only method.
	 * 
	 * @return the compilation unit in which this member is declared, or
	 *         <code>null</code> if this member is not declared in a
	 *         compilation unit (for example, a binary type)
	 */
	IErlModule getModule();

	OtpErlangObject getParseTree();

	boolean isHeader();

	boolean isEdoc();

	/**
	 * The number of %s preceding the comment. Useful for correct indentation
	 * 
	 * @return the number of %s preceding the comment
	 */
	int getLevel();

}
