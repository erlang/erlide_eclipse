package org.erlide.wrangler.refactoring.duplicatedcode.core;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeInstanceElement;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedFileElement;
import org.erlide.wrangler.refactoring.exception.WranglerException;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class DuplicateDetectionParser extends AbstractDuplicatesParser {

	private final String emptyErrorMessage = "No expression found!";

	public DuplicateDetectionParser(OtpErlangObject obj) {
		super(obj);
	}

	// [{ [{ {filename(), integer(), integer()} , {filename(), integer(),
	// integer()} }], integer(), integer(), string()}]
	public void parse(OtpErlangObject object) {
		try {
			// TODO testing all cases
			if (object instanceof OtpErlangTuple) {
				OtpErlangTuple objectTuple = (OtpErlangTuple) object;
				setUnSuccessful(((OtpErlangString) objectTuple.elementAt(1))
						.stringValue());

			} else {
				OtpErlangList resultList = (OtpErlangList) object;
				if (resultList.arity() == 0) {
					setUnSuccessful(emptyErrorMessage);
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

	// { [{ {filename(), integer(), integer()} , {filename(), integer(),
	// integer()} }], integer(), integer(), string()}
	protected DuplicatedCodeElement parseDuplicates(OtpErlangObject object)
			throws OtpErlangRangeException, WranglerException {
		OtpErlangTuple listElementTuple = (OtpErlangTuple) object;
		OtpErlangList duplicateCodeList = (OtpErlangList) listElementTuple
				.elementAt(0);
		LinkedHashMap<IFile, List<DuplicatedCodeInstanceElement>> values = new LinkedHashMap<IFile, List<DuplicatedCodeInstanceElement>>();

		OtpErlangString suggestion = (OtpErlangString) listElementTuple
				.elementAt(3);
		String suggStr = suggestion.stringValue();

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
			IFile file = WranglerUtils.getFileFromPath(fileNameStr);
			DuplicatedCodeInstanceElement instance = new DuplicatedCodeInstanceElement(
					file, startLine.intValue(), startCol.intValue(), endLine
							.intValue(), endCol.intValue() + 1);
			instance.setSuggestedCode(suggStr);
			if (values.containsKey(file)) {
				values.get(file).add(instance);
			} else {
				ArrayList<DuplicatedCodeInstanceElement> dupList = new ArrayList<DuplicatedCodeInstanceElement>();
				dupList.add(instance);
				values.put(file, dupList);
			}
		}

		DuplicatedCodeElement result = new DuplicatedCodeElement(values
				.entrySet().iterator().next().getValue().get(0));
		result.setSuggestedCode(suggStr);

		for (Map.Entry<IFile, List<DuplicatedCodeInstanceElement>> entry : values
				.entrySet()) {
			DuplicatedFileElement dupFile = new DuplicatedFileElement(entry
					.getKey());
			dupFile.setSuggestedCode(suggStr);
			for (DuplicatedCodeInstanceElement instance : entry.getValue()) {
				dupFile.addChild(instance);
			}

			result.addChild(dupFile);
		}

		return result;
	}
}
