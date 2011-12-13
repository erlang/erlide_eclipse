package org.erlide.ui.templates;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateCompletionProcessor;
import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Shell;
import org.erlide.ui.editors.erl.completion.ErlangContextType;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.prefs.PreferenceConstants;
import org.erlide.ui.views.SourceViewerInformationControl;

public class ErlTemplateCompletionProcessor extends TemplateCompletionProcessor {

    private final IDocument fDocument;
    private final int offset;
    private final int length;

    public ErlTemplateCompletionProcessor(final IDocument doc,
            final int offset, final int length) {
        super();
        fDocument = doc;
        this.offset = offset;
        this.length = length;
    }

    @Override
    protected TemplateContextType getContextType(final ITextViewer viewer,
            final IRegion region) {
        return ErlideUIPlugin.getDefault().getContextTypeRegistry()
                .getContextType(ErlangContextType.ERLANG_CONTEXT_TYPE_ID);
    }

    @Override
    protected Image getImage(final Template template) {
        return null;
    }

    @Override
    protected Template[] getTemplates(final String contextTypeId) {
        final Template[] templates = ErlideUIPlugin.getDefault()
                .getTemplateStore().getTemplates();
        final TemplateContextType type = ErlideUIPlugin.getDefault()
                .getContextTypeRegistry().getContextType(contextTypeId);
        if (type instanceof ErlangContextType) {
            final List<Template> result = new ArrayList<Template>(
                    templates.length);
            final ErlangTemplateContext etc = new ErlangTemplateContext(type,
                    fDocument, offset, length);
            for (final Template template : templates) {
                if (etc.canEvaluate(template)) {
                    result.add(template);
                }
            }
            return result.toArray(new Template[result.size()]);
        }
        return templates;
    }

    @Override
    protected ICompletionProposal createProposal(final Template template,
            final TemplateContext context, final IRegion region,
            final int relevance) {
        final ErlTemplateProposal p = new ErlTemplateProposal(template,
                context, region, getImage(template), relevance);
        p.setInformationControlCreator(new IInformationControlCreator() {

            @Override
            public IInformationControl createInformationControl(
                    final Shell parent) {
                return new SourceViewerInformationControl(parent,
                        PreferenceConstants.EDITOR_TEXT_FONT);
            }
        });
        return p;
    }

    @Override
    protected TemplateContext createContext(final ITextViewer viewer,
            final IRegion region) {
        final TemplateContextType contextType = getContextType(viewer, region);
        if (contextType instanceof ErlangContextType) {
            final IDocument document = viewer.getDocument();
            return new ErlangTemplateContext(contextType, document,
                    region.getOffset(), region.getLength());
        }
        return super.createContext(viewer, region);
    }

}
