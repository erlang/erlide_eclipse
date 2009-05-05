/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.erlangsource.prefs;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.jface.text.templates.TemplateVariableResolver;

public class TemplateVariableProcessor implements IContentAssistProcessor {

	private static final Comparator<ICompletionProposal> fgTemplateVariableProposalComparator = new Comparator<ICompletionProposal>() {

		public int compare(ICompletionProposal arg0, ICompletionProposal arg1) {
			return arg0.getDisplayString().compareTo(arg1.getDisplayString());
		}

		@Override
		public boolean equals(Object arg0) {
			return false;
		}

		@Override
		public int hashCode() {
			return 42;
		}
	};

	/** the context type */
	private TemplateContextType fContextType;

	/**
	 * Sets the context type.
	 */
	public void setContextType(TemplateContextType contextType) {
		fContextType = contextType;
	}

	/**
	 * Gets the context type.
	 */
	public TemplateContextType getContextType() {
		return fContextType;
	}

	/*
	 * @see IContentAssistProcessor#computeCompletionProposals(ITextViewer, int)
	 */
	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer,
			int documentOffset) {

		if (fContextType == null) {
			return null;
		}

		final List<ICompletionProposal> proposals = new ArrayList<ICompletionProposal>();

		final String text = viewer.getDocument().get();
		final int start = getStart(text, documentOffset);
		final int end = documentOffset;

		final String string = text.substring(start, end);
		final String prefix = (string.length() >= 2) ? string.substring(2)
				: null;

		final int offset = start;
		final int length = end - start;

		for (final Iterator<?> iterator = fContextType.resolvers(); iterator
				.hasNext();) {
			final TemplateVariableResolver variable = (TemplateVariableResolver) iterator
					.next();

			if (prefix == null || variable.getType().startsWith(prefix)) {
				proposals.add(new TemplateVariableProposal(variable, offset,
						length, viewer));
			}
		}

		Collections.sort(proposals, fgTemplateVariableProposalComparator);
		return proposals.toArray(new ICompletionProposal[proposals.size()]);
	}

	/* Guesses the start position of the completion */
	private int getStart(String string, int end) {
		int start = end;

		if (start >= 1 && string.charAt(start - 1) == '$') {
			return start - 1;
		}

		while ((start != 0)
				&& Character.isUnicodeIdentifierPart(string.charAt(start - 1))) {
			start--;
		}

		if (start >= 2 && string.charAt(start - 1) == '{'
				&& string.charAt(start - 2) == '$') {
			return start - 2;
		}

		return end;
	}

	/*
	 * @see IContentAssistProcessor#computeContextInformation(ITextViewer, int)
	 */
	public IContextInformation[] computeContextInformation(ITextViewer viewer,
			int documentOffset) {
		return null;
	}

	/*
	 * @see
	 * IContentAssistProcessor#getCompletionProposalAutoActivationCharacters()
	 */
	public char[] getCompletionProposalAutoActivationCharacters() {
		return new char[] { '$' };
	}

	/*
	 * @see
	 * IContentAssistProcessor#getContextInformationAutoActivationCharacters()
	 */
	public char[] getContextInformationAutoActivationCharacters() {
		return null;
	}

	/*
	 * @see IContentAssistProcessor#getErrorMessage()
	 */
	public String getErrorMessage() {
		return null;
	}

	/*
	 * @see IContentAssistProcessor#getContextInformationValidator()
	 */
	public IContextInformationValidator getContextInformationValidator() {
		return null;
	}

}
