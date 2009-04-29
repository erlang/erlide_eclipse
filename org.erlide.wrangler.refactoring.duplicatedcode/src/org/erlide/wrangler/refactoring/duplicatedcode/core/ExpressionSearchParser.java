package org.erlide.wrangler.refactoring.duplicatedcode.core;

import java.util.ArrayList;
import java.util.Iterator;

import org.erlide.wrangler.refactoring.core.RefactoringParameters;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeInstanceElement;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ExpressionSearchParser extends AbstractDuplicatesParser {

	public ExpressionSearchParser(OtpErlangObject obj,
			RefactoringParameters parameter) {
		super(obj, parameter);
	}

	public void parse(OtpErlangObject object, RefactoringParameters parameter) {
		try {
			OtpErlangTuple res = (OtpErlangTuple) object;

			if (!res.elementAt(0).toString().equals("ok")) {
				setUnSuccessful(((OtpErlangString) res.elementAt(1))
						.stringValue());
				return;
			}
			if (res.elementAt(1).equals(new OtpErlangList())) {
				setUnSuccessful("No more instances found!");
				return;
			}

			OtpErlangList posList = (OtpErlangList) res.elementAt(1);
			OtpErlangTuple actPos;
			OtpErlangLong startLine, startColumn, endLine, endColumn;

			ArrayList<DuplicatedCodeInstanceElement> instances = new ArrayList<DuplicatedCodeInstanceElement>();

			Iterator<OtpErlangObject> it = posList.iterator();
			while (it.hasNext()) {
				actPos = (OtpErlangTuple) it.next();
				startLine = (OtpErlangLong) actPos.elementAt(0);
				startColumn = (OtpErlangLong) actPos.elementAt(1);
				endLine = (OtpErlangLong) actPos.elementAt(2);
				endColumn = (OtpErlangLong) actPos.elementAt(3);

				instances.add(new DuplicatedCodeInstanceElement(parameter
						.getFile(), startLine.intValue(), startColumn
						.intValue(), endLine.intValue(), endColumn.intValue()));
			}

			DuplicatedCodeInstanceElement defaultInstance = instances.get(0);

			DuplicatedCodeElement result = new DuplicatedCodeElement(
					defaultInstance);

			for (DuplicatedCodeInstanceElement instance : instances) {
				result.addChild(instance);
			}

			isSuccessful = true;
			errorMessage = null;
			duplicates = new ArrayList<DuplicatedCodeElement>();
			duplicates.add(result);

		} catch (Exception e) {
			setUnSuccessful(e.getMessage());
		}

	}
}
