/*******************************************************************************
 * Copyright (c) 2004 Lukas Larsson and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Lukas Larsson
 *******************************************************************************/

package org.erlide.ui.wizards.templates;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.templates.DocumentTemplateContext;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateBuffer;
import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateException;
import org.eclipse.jface.text.templates.TemplateVariableResolver;
import org.erlide.jinterface.util.ErlLogger;

public class FileCommentVariableResolver extends TemplateVariableResolver {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jface.text.templates.TemplateVariableResolver#resolve(org
	 * .eclipse.jface.text.templates.TemplateContext)
	 */
	@Override
	protected String resolve(final TemplateContext context) {
		final Template template = ErlangSourceContextTypeComment.getDefault()
				.getTemplateStore().getTemplateData(
						"org.erlide.ui.erlangsource.filecomment").getTemplate();

		final DocumentTemplateContext commentContext = new DocumentTemplateContext(
				ErlangSourceContextTypeBehaviour.getDefault(), new Document(
						template.getPattern()), 0, template.getPattern()
						.length());
		TemplateBuffer tb;
		try {
			tb = commentContext.evaluate(template);
		} catch (final BadLocationException e) {
			ErlLogger.warn(e);
			return "Error: " + e.getMessage();
		} catch (final TemplateException e) {
			ErlLogger.warn(e);
			return "Error: " + template.getName() + " could not be validated!";
		}

		return tb.getString();
	}

}
