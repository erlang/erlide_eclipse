package org.erlide.wrangler.refactoring.core.internal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.erlide.wrangler.refactoring.core.CostumWorkflowRefactoring;
import org.erlide.wrangler.refactoring.util.IErlRange;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Abstract class for integrating Wrangler refactorings which offer a selection
 * list for the user. These selections are tipically code parts.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class CostumWorkflowRefactoringWithPositionsSelection extends
		CostumWorkflowRefactoring {

	protected HashMap<IErlRange, OtpErlangTuple> positions;
	protected ArrayList<IErlRange> selectedPositions;

	public List<IErlRange> getPositions() {
		ArrayList<IErlRange> ret = new ArrayList<IErlRange>();
		for (IErlRange r : positions.keySet()) {
			ret.add(r);
		}

		return ret;
	}

	public void setSelectedPos(ArrayList<IErlRange> l) {
		selectedPositions = l;
	}

	protected OtpErlangList getSelectedPos() {
		OtpErlangList ret;
		OtpErlangObject[] selection = new OtpErlangObject[selectedPositions
				.size()];

		for (int i = 0; i < selectedPositions.size(); ++i) {
			selection[i] = positions.get(selectedPositions.get(i));
		}

		ret = new OtpErlangList(selection);
		return ret;
	}

}
