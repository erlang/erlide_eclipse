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
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.util.ErlModelUtils;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlContentAssistProcessor implements IContentAssistProcessor {

	private final ICompletionProposal[] NO_COMPLETIONS = new ICompletionProposal[0];

	private ErlangEditor fEditor;

	public ErlContentAssistProcessor(ErlangEditor editor) {
		fEditor = editor;
	}

	private OtpErlangList getDocumentationFor(OtpErlangList list,
			OtpErlangAtom mod) {
		try {
			final OtpErlangString s = new OtpErlangString(ErlideUIPlugin
					.getDefault().getStateLocation().toString());
			final OtpErlangObject r1 = BackendUtil.checkRpc(BackendManager
					.getDefault().getIdeBackend().rpc("erlide_otp_doc",
							"get_doc_from_fun_arity_list", mod, list, s));
			if (r1 instanceof OtpErlangList) {
				return (OtpErlangList) r1;
			} else {
				return null;
			}
		} catch (final BackendException e) {

		}
		return null;
	}

	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer,
			int offset) {
		try {
			final IDocument doc = viewer.getDocument();
			String prefix = lastText(doc, offset);
			final String indent = lastIndent(doc, offset);

			final ArrayList<ICompletionProposal> result = new ArrayList<ICompletionProposal>();

			int i = prefix.indexOf(':');
			if (i >= 0) {
				// we have a remote call
				final String mod = prefix.substring(0, i);
				prefix = prefix.substring(i + 1);
				final IErlProject project = ErlModelUtils
						.getErlProject(fEditor);
				final OtpErlangAtom modAtom = new OtpErlangAtom(mod);
				final IBackend b = BackendManager.getDefault().get(
						project.getProject());
				final OtpErlangObject res = BackendUtil.checkRpc(b.rpc(
						"erlide_model", "get_exported", modAtom,
						new OtpErlangString(prefix)));
				if (res instanceof OtpErlangList) {
					final OtpErlangList resl = (OtpErlangList) res;
					final OtpErlangList docl = getDocumentationFor(resl,
							modAtom);
					for (i = 0; i < resl.arity(); i++) {
						final OtpErlangTuple f = (OtpErlangTuple) resl
								.elementAt(i);
						final String fstr = ((OtpErlangAtom) f.elementAt(0))
								.atomValue();
						final int far = ((OtpErlangLong) f.elementAt(1))
								.intValue();
						String args = "";
						for (int j = 0; j < far - 1; j++) {
							args += ", ";
						}
						String docStr = null;
						if (docl != null) {
							final OtpErlangObject elt = docl.elementAt(i);
							if (elt instanceof OtpErlangString) {
								docStr = ((OtpErlangString) elt).stringValue();
							}
						}
						final String cpl = fstr.substring(prefix.length())
								+ "(" + args + ")";
						result.add(new CompletionProposal(cpl, offset, 0, cpl
								.length()
								- 1 - far * 2 + 2, null, fstr + "/" + far,
								null, docStr));
					}
					return (ICompletionProposal[]) result
							.toArray(new ICompletionProposal[result.size()]);
				} else {
					return NO_COMPLETIONS;
				}
			} else {
				// either a local call or a module name
				final IErlModule myMdl = ErlModelUtils.getModule(fEditor);
				if (myMdl != null) {
					myMdl.getContentProposals(prefix, indent, offset, result);
					return (ICompletionProposal[]) result
							.toArray(new ICompletionProposal[result.size()]);
				}
				return NO_COMPLETIONS;
			}
		} catch (final BackendException e) {
			return NO_COMPLETIONS;
		} catch (final OtpErlangRangeException e) {
			return NO_COMPLETIONS;
		}
	}

	/* NOT USED */
	/*
	 * private String lastWord(IDocument doc, int offset) { try { for (int n =
	 * offset - 1; n >= 0; n--) { if
	 * (!Character.isJavaIdentifierPart(doc.getChar(n))) { return doc.get(n + 1,
	 * offset - n - 1); } } } catch (final BadLocationException e) { } return
	 * ""; }
	 */

	private String lastText(IDocument doc, int offset) {
		try {
			for (int n = offset - 1; n >= 0; n--) {
				char c = doc.getChar(n);
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

	private String lastIndent(IDocument doc, int offset) {
		try {
			int start = offset - 1;
			while (start >= 0 && doc.getChar(start) != '\n') {
				start--;
			}
			int end = start + 1;
			while (end < offset && isspace(doc.getChar(end))) {
				end++;
			}
			return doc.get(start + 1, end - start - 1);
		} catch (final BadLocationException e) {
		}
		return "";
	}

	private boolean isspace(char ch) {
		return ch == ' ' || ch == '\t';
	}

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
