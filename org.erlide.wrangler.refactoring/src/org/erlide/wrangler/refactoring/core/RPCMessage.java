package org.erlide.wrangler.refactoring.core;

import java.util.ArrayList;
import java.util.List;

import org.erlide.runtime.backend.RpcResult;
import org.erlide.wrangler.refactoring.core.exception.WranglerException;
import org.erlide.wrangler.refactoring.core.exception.WranglerRPCException;
import org.erlide.wrangler.refactoring.core.exception.WranglerRefactoringException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * An RPCMessage object is a layer between the <code>RPCResult</code> and the
 * refactor tool. Provides methods for checking the RPC's result, and getting
 * information from it.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class RPCMessage {

	private final RpcResult result;

	/**
	 * Sole constructor. Stores the given <code>RpcResult</code>
	 * 
	 * @param result
	 */
	public RPCMessage(RpcResult result) {
		this.result = result;
	}

	/**
	 * Checks if the RPC communication is successfully finished. If not an
	 * exception is raised. Else the message will be parsed and stored.
	 * 
	 * @throws WranglerException
	 */
	public void checkIsOK() throws WranglerException {
		if (result.isOk()) {
			OtpErlangTuple tuple = (OtpErlangTuple) result.getValue();

			checkOkResultCases(tuple);

		} else if (!result.isOk()) {
			OtpErlangTuple tuple = (OtpErlangTuple) result.getValue();
			if (tuple.elementAt(0).equals(new OtpErlangAtom("EXIT"))) {
				OtpErlangTuple msgTuple = (OtpErlangTuple) tuple.elementAt(1);
				OtpErlangString msg = (OtpErlangString) msgTuple.elementAt(1);
				throw new WranglerRefactoringException(msg.stringValue());
			} else {
				throw new WranglerRPCException();
			}
		}
	}

	/**
	 * Checks several cases when the RPC was successfully. Extending classes
	 * should override this method.
	 * 
	 * @param tuple
	 *            the RPC message in an Erlang object
	 * @throws WranglerRefactoringException
	 *             exception raised if the Wrangler's message contains some
	 *             error
	 */
	protected void checkOkResultCases(OtpErlangTuple tuple)
			throws WranglerRefactoringException {
		if (!tuple.elementAt(0).toString().equals("ok")) {
			String message = ((OtpErlangString) tuple.elementAt(1))
					.stringValue();
			throw new WranglerRefactoringException(message);
		}
	}

	/**
	 * Returns a new representation of the refactored file. Note that before
	 * calling this method <code>checkIsOK</code> must be called.
	 * 
	 * @return list of special Erlang file representations from which can be
	 *         retrieved <code>Change</code> objects
	 */
	public List<FileResourceChanges> getResult() {
		ArrayList<FileResourceChanges> res = new ArrayList<FileResourceChanges>();

		OtpErlangTuple rpcResp = (OtpErlangTuple) result.getValue();
		OtpErlangList changedFileTupleList = (OtpErlangList) rpcResp
				.elementAt(1);

		OtpErlangTuple e;
		OtpErlangString oldPath, newPath, newContent;
		for (int i = 0; i < changedFileTupleList.arity(); ++i) {
			e = (OtpErlangTuple) changedFileTupleList.elementAt(i);
			oldPath = (OtpErlangString) e.elementAt(0);
			newPath = (OtpErlangString) e.elementAt(1);
			newContent = (OtpErlangString) e.elementAt(2);

			res.add(new FileResourceChanges(oldPath.stringValue(), newPath
					.stringValue(), newContent.stringValue()));
		}
		// TODO: non changing refactorings do need other structure
		return res;
	}
}
