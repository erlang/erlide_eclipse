package org.erlide.ui.templates;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateBuffer;
import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateException;
import org.eclipse.jface.text.templates.TemplateProposal;
import org.eclipse.swt.graphics.Image;

public class ErlTemplateProposal extends TemplateProposal {

    public ErlTemplateProposal(final Template template,
            final TemplateContext context, final IRegion region,
            final Image image, final int relevance) {
        super(template, context, region, image, relevance);
    }

    @Override
    public String getAdditionalProposalInfo() {
        try {
            final TemplateContext context = getContext();
            context.setReadOnly(true);
            TemplateBuffer templateBuffer;
            try {
                final Template template = getTemplate();
                if (context instanceof ErlangTemplateContext) {
                    final ErlangTemplateContext etc = (ErlangTemplateContext) context;
                    templateBuffer = etc.evaluate(template, true);
                } else {
                    templateBuffer = context.evaluate(template);
                }
            } catch (final TemplateException e) {
                return null;
            }
            return templateBuffer.getString();
        } catch (final BadLocationException e) {
            return null;
        }
    }

}
