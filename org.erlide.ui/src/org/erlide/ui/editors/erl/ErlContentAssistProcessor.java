/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.graphics.Point;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.ui.ErlideUIPlugin;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideDoc;

public class ErlContentAssistProcessor implements IContentAssistProcessor {
	private final ISourceViewer sourceViewer;
	String prefix;

	private static final ICompletionProposal[] NO_COMPLETIONS = new ICompletionProposal[0];

	public ErlContentAssistProcessor(final ISourceViewer sourceViewer,
			final String prefix) {
		this.sourceViewer = sourceViewer;
		this.prefix = prefix;
	}

	// @Deprecated
	// private OtpErlangList getDocumentationFor(final OtpErlangList list,
	// final String mod) {
	// try {
	// final String s = ErlideUIPlugin.getDefault().getStateLocation()
	// .toString();
	// final OtpErlangObject r1 = ErlideDoc.getFunDoc(list, mod, s);
	// if (r1 instanceof OtpErlangList) {
	// return (OtpErlangList) r1;
	// }
	// return null;
	// } catch (final Exception e) {
	//
	// }
	// return null;
	// }

	public ICompletionProposal[] computeCompletionProposals(
			final ITextViewer viewer, final int offset) {
		try {
			final IDocument doc = viewer.getDocument();
			final String aprefix = lastText(doc, offset);
			// final String indent = lastIndent(doc, offset);

			final ArrayList<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
			final int k = aprefix.indexOf(':');
			final IBackend b = BackendManager.getDefault().getIdeBackend();
			if (k >= 0) {
				final String moduleName = aprefix.substring(0, k);
				externalCallCompletions(moduleName, offset, aprefix
						.substring(k + 1), result, k, b);
				return result.toArray(new ICompletionProposal[result.size()]);
			}
			moduleCompletions(offset, aprefix, result, k, b);
			return result.toArray(new ICompletionProposal[result.size()]);
		} catch (final Exception e) {
			e.printStackTrace();
			return NO_COMPLETIONS;
		}
	}

	private void moduleCompletions(final int offset, final String aprefix,
			final List<ICompletionProposal> result, final int k,
			final IBackend b) {
		final List<String> allErlangFiles = org.erlide.core.util.ResourceUtil
				.getAllErlangFiles();
		OtpErlangObject res = null;
		res = ErlideDoc.getModules(b, aprefix, allErlangFiles);
		if (res instanceof OtpErlangList) {
			final OtpErlangList resList = (OtpErlangList) res;
			for (int i = 0; i < resList.arity(); ++i) {
				final OtpErlangObject o = resList.elementAt(i);
				if (o instanceof OtpErlangString) {
					final OtpErlangString s = (OtpErlangString) o;
					final String cpl = s.stringValue() + ":";
					result
							.add(new CompletionProposal(cpl, offset
									- aprefix.length(), aprefix.length(), cpl
									.length()));
				}
			}
		}
	}

	private void externalCallCompletions(final String moduleName,
			final int offset, final String aprefix,
			final List<ICompletionProposal> result, final int k,
			final IBackend b) throws ErlangRpcException, BackendException,
			RpcException, OtpErlangRangeException {
		// we have an external call
		final String stateDir = ErlideUIPlugin.getDefault().getStateLocation()
				.toString();
		final OtpErlangObject res = ErlideDoc.getProposalsWithDoc(b,
				moduleName, aprefix, stateDir);
		// final OtpErlangObject res = ErlideDoc
		// .getExported(b, prefix, moduleName);
		if (res instanceof OtpErlangList) {
			final OtpErlangList resl = (OtpErlangList) res;
			for (int i = 0; i < resl.arity(); i++) {
				// {FunWithArity, FunWithParameters, [{Offset, Length}], Doc}
				final OtpErlangTuple f = (OtpErlangTuple) resl.elementAt(i);
				final String funWithArity = ((OtpErlangString) f.elementAt(0))
						.stringValue();
				final String funWithParameters = ((OtpErlangString) f
						.elementAt(1)).stringValue();
				final OtpErlangList parOffsets = (OtpErlangList) f.elementAt(2);
				final int nPars = parOffsets.arity();
				String docStr = null;
				if (f.arity() > 3) {
					final OtpErlangObject elt = f.elementAt(3);
					if (elt instanceof OtpErlangString) {
						docStr = ((OtpErlangString) elt).stringValue();
					}
				}
				final String cpl = funWithParameters.substring(aprefix.length());
				final List<Point> offsetsAndLengths = getOffsetsAndLengths(
						parOffsets, offset);
				int offs = cpl.length();
				if (nPars > 0) {
					offs = offsetsAndLengths.get(0).x;
				}
				// final ICompletionProposal c = new CompletionProposal(cpl,
				// offset, 0, offs, null, funWithArity, null, docStr);

				final ICompletionProposal c = new ErlCompletionProposal(
						offsetsAndLengths, funWithArity, cpl, offset, 0, offs,
						null, null, docStr, sourceViewer);

				result.add(c);
			}
		}
	}

	private List<Point> getOffsetsAndLengths(final OtpErlangList parOffsets,
			final int replacementOffset) {
		final int arity = parOffsets.arity();
		final List<Point> result = new ArrayList<Point>(arity);
		for (int i = 0; i < arity; i++) {
			final OtpErlangTuple t = (OtpErlangTuple) parOffsets.elementAt(i);
			final OtpErlangLong offset = (OtpErlangLong) t.elementAt(0);
			final OtpErlangLong length = (OtpErlangLong) t.elementAt(1);
			try {
				result.add(new Point(offset.intValue() + replacementOffset,
						length.intValue()));
			} catch (final OtpErlangRangeException e) {
			}
		}
		return result;
	}

	private String lastText(final IDocument doc, final int offset) {
		try {
			for (int n = offset - 1; n >= 0; n--) {
				final char c = doc.getChar(n);
				if (!isErlangIdentifierChar(c) && c != ':') {
					return doc.get(n + 1, offset - n - 1);
				}
			}
			return doc.get(0, offset);
		} catch (final BadLocationException e) {
		}
		return "";
	}

	static private boolean isErlangIdentifierChar(final char char1) {
		return Character.isJavaIdentifierPart(char1);
	}

	public IContextInformation[] computeContextInformation(
			final ITextViewer viewer, final int offset) {
		return null;
	}

	public char[] getCompletionProposalAutoActivationCharacters() {
		return new char[] { ':' };
	}

	public char[] getContextInformationAutoActivationCharacters() {
		return null;
	}

	public String getErrorMessage() {
		return null;
	}

	public IContextInformationValidator getContextInformationValidator() {
		return null;
	}
}
