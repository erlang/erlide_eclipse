package org.erlide.wrangler.refactoring.duplicatedcode.core;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.erlide.wrangler.refactoring.core.RefactoringParameters;
import org.erlide.wrangler.refactoring.core.exception.WranglerRefactoringException;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeInstanceElement;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedFileElement;
import org.erlide.wrangler.refactoring.util.EditorUtil;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class DuplicateDetectionParser extends AbstractDuplicatesParser {

	public DuplicateDetectionParser(OtpErlangObject obj,
			RefactoringParameters parameter) {
		super(obj, parameter);
	}

	// [{ [{ {filename(), integer(), integer()} , {filename(), integer(),
	// integer()} }], integer(), integer()}]
	public void parse(OtpErlangObject object, RefactoringParameters parameter) {
		try {
			// TODO testing all cases
			if (object instanceof OtpErlangTuple) {
				OtpErlangTuple objectTuple = (OtpErlangTuple) object;
				setUnSuccessful(objectTuple.elementAt(1).toString());

			} else {
				OtpErlangList resultList = (OtpErlangList) object;
				if (resultList.arity() == 0) {
					setUnSuccessful("There is not any duplicates...");
					return;
				}

				duplicates = new ArrayList<DuplicatedCodeElement>();
				for (int i = 0; i < resultList.arity(); ++i) {
					duplicates.add(parseDuplicates(resultList.elementAt(i)));
				}
				isSuccessful = true;
			}
		} catch (Exception e) {
			setUnSuccessful(e.getMessage());
		}

	}

	protected DuplicatedCodeElement parseDuplicates(OtpErlangObject object)
			throws OtpErlangRangeException, WranglerRefactoringException {
		OtpErlangTuple listElementTuple = (OtpErlangTuple) object;
		OtpErlangList duplicateCodeList = (OtpErlangList) listElementTuple
				.elementAt(0);
		LinkedHashMap<IFile, List<DuplicatedCodeInstanceElement>> values = new LinkedHashMap<IFile, List<DuplicatedCodeInstanceElement>>();

		OtpErlangObject[] elements = duplicateCodeList.elements();

		for (int i = 0; i < elements.length; ++i) {
			OtpErlangTuple elementPair = (OtpErlangTuple) elements[i];
			OtpErlangTuple firstElement = (OtpErlangTuple) elementPair
					.elementAt(0);
			OtpErlangTuple secondElement = (OtpErlangTuple) elementPair
					.elementAt(1);
			OtpErlangString fileName = (OtpErlangString) firstElement
					.elementAt(0);
			OtpErlangLong startLine = (OtpErlangLong) firstElement.elementAt(1);
			OtpErlangLong startCol = (OtpErlangLong) firstElement.elementAt(2);
			OtpErlangLong endLine = (OtpErlangLong) secondElement.elementAt(1);
			OtpErlangLong endCol = (OtpErlangLong) secondElement.elementAt(2);

			String fileNameStr = fileName.stringValue();
			IFile file = EditorUtil.geFileFromPath(fileNameStr);
			DuplicatedCodeInstanceElement instance = new DuplicatedCodeInstanceElement(file,
					startLine.intValue(), startCol.intValue(), endLine
							.intValue(), endCol.intValue());
			if (values.containsKey(file)) {
				values.get(file).add(instance);
			} else {
				ArrayList<DuplicatedCodeInstanceElement> dupList = new ArrayList<DuplicatedCodeInstanceElement>();
				dupList.add(instance);
				values.put(file, dupList);
			}
		}

		DuplicatedCodeElement result = new DuplicatedCodeElement(values.entrySet().iterator()
				.next().getValue().get(0));

		for (Map.Entry<IFile, List<DuplicatedCodeInstanceElement>> entry : values
				.entrySet()) {
			DuplicatedFileElement dupFile = new DuplicatedFileElement(entry.getKey());
			for (DuplicatedCodeInstanceElement instance : entry.getValue()) {
				dupFile.addChild(instance);
			}

			result.addChild(dupFile);
		}

		return result;
	}
}
