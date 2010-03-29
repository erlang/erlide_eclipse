package org.erlide.wrangler.refactoring.core;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.wrangler.refactoring.backend.internal.StateDataToRecordRpcMessage;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * Abstract refactoring for QuickCheck state data to record refactorings
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class QuickCheckStateRefactoring extends
		SimpleOneStepWranglerRefactoring {

	protected OtpErlangObject stateFuns;
	protected int fieldCount = 0;
	private String recordName;
	private List<String> fieldsNames;

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		// FIXME: what kind of preconditions do I need?
		IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
				.getWranglerSelection();
		StateDataToRecordRpcMessage message = runFirst(sel);
		if (!message.isSuccessful()) {
			return RefactoringStatus.createFatalErrorStatus(message
					.getMessageString());
		} else {
			fieldCount = message.getFieldCount();
			stateFuns = message.getStateFuns();
			return new RefactoringStatus();
		}
	}

	protected abstract StateDataToRecordRpcMessage runFirst(
			IErlMemberSelection sel);

	/**
	 * Returns the count of fields which a new record may need
	 * 
	 * @return coutn of fields
	 */
	public int getRecordFieldCount() {
		return fieldCount;
	}

	/**
	 * Record data setter
	 * 
	 * @param name
	 *            record name
	 * @param fieldNames
	 *            field names
	 */
	public void setRecordData(String name, List<String> fieldNames) {
		this.recordName = name;
		this.fieldsNames = fieldNames;
	}

	protected String getRecordName() {
		return recordName;
	}

	protected OtpErlangList getFieldsName() {
		ArrayList<OtpErlangString> f = new ArrayList<OtpErlangString>();
		for (String s : fieldsNames) {
			f.add(new OtpErlangString(s));
		}
		return new OtpErlangList(f.toArray(new OtpErlangString[0]));

	}
}
