package org.erlide.ui.templates;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.templates.DocumentTemplateContext;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateBuffer;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.jface.text.templates.TemplateException;
import org.eclipse.jface.text.templates.TemplateTranslator;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.ui.editors.erl.actions.IndentAction;

public class ErlangTemplateContext extends DocumentTemplateContext {

    public ErlangTemplateContext(final TemplateContextType contextType,
            final IDocument document, final int offset, final int length) {
        super(contextType, document, offset, length);
    }

    @Override
    public TemplateBuffer evaluate(final Template template)
            throws BadLocationException, TemplateException {
        return evaluate(template, false);
    }

    public TemplateBuffer evaluate(Template template, final boolean indentFrom0)
            throws BadLocationException, TemplateException {
        if (!canEvaluate(template)) {
            return null;
        }

        if (ErlTemplateCompletionPreferences.getIndentCode()) {
            template = indentTemplatePattern(template, indentFrom0);
        }

        final TemplateTranslator translator = new TemplateTranslator();
        final TemplateBuffer buffer = translator.translate(template);

        getContextType().resolve(buffer, this);

        return buffer;
    }

    @Override
    public boolean canEvaluate(final Template template) {
        final String key = getKey();
        return key.length() != 0
                && template.getName().toLowerCase()
                        .startsWith(key.toLowerCase());
    }

    private Template indentTemplatePattern(final Template template,
            final boolean indentFrom0) {
        String pattern = template.getPattern();
        final String whiteSpacePrefix = indentFrom0 ? ""
                : getWhiteSpacePrefix();
        try {
            pattern = IndentAction.indentLines(0, 0, pattern, true,
                    whiteSpacePrefix);
        } catch (final RpcException e) {
            ErlLogger.error(e);
        }
        return new Template(template.getName(), template.getDescription(),
                template.getContextTypeId(), pattern,
                template.isAutoInsertable());
    }

    private String getWhiteSpacePrefix() {
        final int start = getStart();
        final IDocument document = getDocument();
        int line;
        try {
            line = document.getLineOfOffset(start);
            final int lineStart = document.getLineOffset(line);
            for (int i = 0; lineStart + i <= start; ++i) {
                final char c = document.getChar(lineStart + i);
                if (c != ' ' && c != '\t') {
                    return document.get(lineStart, i);
                }
            }
        } catch (final BadLocationException e) {
            ErlLogger.error(e);
        }
        return "";
    }
}
