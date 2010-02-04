package org.erlide.ui.editors.erl.completion;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateCompletionProcessor;
import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.swt.graphics.Image;
import org.erlide.ui.ErlideUIPlugin;

public class ErlTemplateCompletionProcessor extends TemplateCompletionProcessor {

	@Override
	protected TemplateContextType getContextType(final ITextViewer viewer,
			final IRegion region) {
		return ErlideUIPlugin.getDefault().getContextTypeRegistry()
				.getContextType(ErlangContextType.ERLANG_CONTEXT_TYPE);
	}

	@Override
	protected Image getImage(final Template template) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Template[] getTemplates(final String contextTypeId) {
		return ErlideUIPlugin.getDefault().getTemplateStore().getTemplates();
	}

	// @Override
	// protected ICompletionProposal createProposal(final Template template,
	// final TemplateContext context, final IRegion region,
	// final int relevance) {
	// if (ErlTemplateCompletionPreferences.getIndentCode() || false) {
	// return new ErlIndentingTemplateProsoal(template, context, region,
	// getImage(template), relevance);
	// }
	// return super.createProposal(template, context, region, relevance);
	// }

	@Override
	protected TemplateContext createContext(final ITextViewer viewer,
			final IRegion region) {
		final TemplateContextType contextType = getContextType(viewer, region);
		if (contextType instanceof ErlangContextType) {
			final IDocument document = viewer.getDocument();
			return new ErlangTemplateContext(contextType, document, region
					.getOffset(), region.getLength());
		}
		return super.createContext(viewer, region);
	}

}
