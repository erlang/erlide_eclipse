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

public class SimilarExpressionSearchParser extends AbstractDuplicatesParser {

	public SimilarExpressionSearchParser(OtpErlangObject obj) {
		super(obj);
	}

	/*
	 * Parse Wrangler messages which are in the following format: {ok,
	 * {[{{filename, startLine, startCol},{filePath, endLine, endCol}}],
	 * generalisation}
	 * 
	 * @see
	 * org.erlide.wrangler.refactoring.duplicatedcode.core.IResultParser#parse
	 * (com.ericsson.otp.erlang.OtpErlangObject)
	 */
	public void parse(OtpErlangObject object) {
		try {
			OtpErlangTuple res = (OtpErlangTuple) object;
			if (!res.elementAt(0).toString().equals("ok")) {
				setUnSuccessful(((OtpErlangString) res.elementAt(1))
						.stringValue());
				return;
			}

			OtpErlangTuple result = (OtpErlangTuple) res.elementAt(1);

			if (result.elementAt(0).equals(new OtpErlangList())) {
				setUnSuccessful("No more instances found!");
				return;
			}

			DuplicatedCodeElement dup = parseDuplicates(result);
			duplicates = new ArrayList<DuplicatedCodeElement>();
			duplicates.add(dup);
			isSuccessful = true;
			errorMessage = null;
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
				.elementAt(1);
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
