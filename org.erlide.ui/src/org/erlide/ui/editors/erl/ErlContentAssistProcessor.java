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
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.util.ErlModelUtils;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideDoc;

public class ErlContentAssistProcessor implements IContentAssistProcessor {

	private final ICompletionProposal[] NO_COMPLETIONS = new ICompletionProposal[0];

	private final ErlangEditor fEditor;

	public ErlContentAssistProcessor(ErlangEditor editor) {
		fEditor = editor;
	}

	private OtpErlangList getDocumentationFor(OtpErlangList list, String mod) {
		try {
			final String s = ErlideUIPlugin.getDefault().getStateLocation()
					.toString();
			final OtpErlangObject r1 = ErlideDoc.getFunDoc(list, mod, s);
			if (r1 instanceof OtpErlangList) {
				return (OtpErlangList) r1;
			}
			return null;
		} catch (final Exception e) {

		}
		return null;
	}

	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer,
			int offset) {
		try {
			final IDocument doc = viewer.getDocument();
			final String prefix = lastText(doc, offset);
			// final String indent = lastIndent(doc, offset);

			final ArrayList<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
			final IErlModule module = ErlModelUtils.getModule(fEditor
					.getEditorInput());
			if (module == null) {
				return null;
			}
			final int k = prefix.indexOf(':');
			final IErlProject project = ErlModelUtils.getErlProject(fEditor);
			final IBackend b = BackendManager.getDefault().get(
					project.getProject());
			if (k >= 0) {
				final String moduleName = prefix.substring(0, k);
				externalCallCompletions(moduleName, offset, prefix
						.substring(k + 1), result, k, b, project);
				return result.toArray(new ICompletionProposal[result.size()]);
			} else {
				moduleCallCompletions(offset, prefix, result, k, b, project);
				return result.toArray(new ICompletionProposal[result.size()]);
			}
		} catch (final Exception e) {
			return NO_COMPLETIONS;
		}
	}

	private void moduleCallCompletions(int offset, String prefix,
			ArrayList<ICompletionProposal> result, int k, IBackend b,
			IErlProject project) {
		final List<String> allErlangFiles = org.erlide.core.util.ResourceUtil
				.getAllErlangFiles();
		OtpErlangObject res = null;
		try {
			res = ErlideDoc.getModules(b, prefix, allErlangFiles);
		} catch (final BackendException e) {
			e.printStackTrace();
		} catch (final RpcException e) {
			e.printStackTrace();
		}
		if (res instanceof OtpErlangList) {
			final OtpErlangList resList = (OtpErlangList) res;
			for (int i = 0; i < resList.arity(); ++i) {
				final OtpErlangObject o = resList.elementAt(i);
				if (o instanceof OtpErlangString) {
					final OtpErlangString s = (OtpErlangString) o;
					final String cpl = s.stringValue() + ":";
					result.add(new CompletionProposal(cpl, offset
							- prefix.length(), prefix.length(), cpl.length()));
				}
			}
		}
	}

	private void externalCallCompletions(String moduleName, int offset,
			String prefix, final ArrayList<ICompletionProposal> result,
			final int k, IBackend b, IErlProject project)
			throws ErlangRpcException, BackendException, RpcException,
			OtpErlangRangeException {
		// we have an external call
		final OtpErlangObject res = ErlideDoc
				.getExported(b, prefix, moduleName);
		if (res instanceof OtpErlangList) {
			final OtpErlangList resl = (OtpErlangList) res;
			final OtpErlangList docl = getDocumentationFor(resl, moduleName);
			for (int i = 0; i < resl.arity(); i++) {
				final OtpErlangTuple f = (OtpErlangTuple) resl.elementAt(i);
				final String fstr = ((OtpErlangAtom) f.elementAt(0))
						.atomValue();
				final int far = ((OtpErlangLong) f.elementAt(1)).intValue();
				final StringBuilder args = new StringBuilder(far * 2);
				for (int j = 0; j < far - 1; j++) {
					args.append(", ");
				}
				String docStr = null;
				if (docl != null) {
					final OtpErlangObject elt = docl.elementAt(i);
					if (elt instanceof OtpErlangString) {
						docStr = ((OtpErlangString) elt).stringValue();
					}
				}
				final String cpl = fstr.substring(prefix.length()) + "("
						+ args.toString() + ")";
				result
						.add(new CompletionProposal(cpl, offset, 0, cpl
								.length()
								- 1 - far * 2 + 2, null, fstr + "/" + far,
								null, docStr));
			}
		}
	}

	private String lastText(IDocument doc, int offset) {
		try {
			for (int n = offset - 1; n >= 0; n--) {
				final char c = doc.getChar(n);
				if (!isErlangIdentifierChar(c) && c != ':') {
					return doc.get(n + 1, offset - n - 1);
				}
			}
		} catch (final BadLocationException e) {
		}
		return "";
	}

	static private boolean isErlangIdentifierChar(char char1) {
		return Character.isJavaIdentifierPart(char1);
	}

	// private String lastIndent(IDocument doc, int offset) {
	// try {
	// int start = offset - 1;
	// while (start >= 0 && doc.getChar(start) != '\n') {
	// start--;
	// }
	// int end = start + 1;
	// while (end < offset && isspace(doc.getChar(end))) {
	// end++;
	// }
	// return doc.get(start + 1, end - start - 1);
	// } catch (final BadLocationException e) {
	// }
	// return "";
	// }

	// private boolean isspace(char ch) {
	// return ch == ' ' || ch == '\t';
	// }

	public IContextInformation[] computeContextInformation(ITextViewer viewer,
			int offset) {
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
