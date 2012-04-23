package org.erlide.ui.editors.erl.hyperlink;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.hyperlink.AbstractHyperlinkDetector;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.erlide.core.model.erlang.ErlToken;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.ui.actions.OpenAction;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.IErlangEditorActionDefinitionIds;

public class ErlangHyperlinkDetector extends AbstractHyperlinkDetector {

    public ErlangHyperlinkDetector() {
    }

    @Override
    public IHyperlink[] detectHyperlinks(final ITextViewer textViewer,
            final IRegion region, final boolean canShowMultipleHyperlinks) {
        if (region == null) {
            return null;
        }
        final IDocument document = textViewer.getDocument();
        if (document == null) {
            return null;
        }
        return detectHyperlinks(document, region.getOffset());
    }

    private IHyperlink[] detectHyperlinks(final IDocument doc, final int offset) {
        final ErlangEditor editor = (ErlangEditor) getAdapter(ErlangEditor.class);
        if (editor == null) {
            return null;
        }
        final IErlModule module = editor.getModule();
        if (module == null) {
            return null;
        }
        final ErlToken token = module.getScannerTokenAt(offset);
        if (token == null) {
            return null;
        }
        final int tokenKind = token.getKind();
        if (tokenKind != ErlToken.KIND_ATOM
                && tokenKind != ErlToken.KIND_STRING
                && tokenKind != ErlToken.KIND_MACRO
                && tokenKind != ErlToken.KIND_VAR) {
            return null;
        }
        try {
            final ITypedRegion partition = doc.getPartition(offset);
            final ErlRegion region = new ErlRegion(token.getOffset(),
                    token.getLength(), partition.getType());
            if (!IDocument.DEFAULT_CONTENT_TYPE.equals(region.getType())) {
                return null;
            }
            return new IHyperlink[] { new ErlangHyperlink(editor, region) };
        } catch (final BadLocationException e) {
            return null;
        }
    }

    static class ErlRegion extends Region {
        String type;

        public ErlRegion(final int offset, final int length, final String type) {
            super(offset, length);
            this.type = type;
        }

        public String getType() {
            return type;
        }

        public void setType(final String string) {
            type = string;
        }

    }

    private static class ErlangHyperlink implements IHyperlink {
        private final ErlangEditor editor;
        private final ErlRegion region;

        public ErlangHyperlink(final ErlangEditor editor,
                final ErlRegion partion) {
            this.editor = editor;
            region = partion;
        }

        @Override
        public String getTypeLabel() {
            return null;
        }

        @Override
        public String getHyperlinkText() {
            return null;
        }

        @Override
        public void open() {
            final OpenAction action = (OpenAction) editor
                    .getAction(IErlangEditorActionDefinitionIds.OPEN);
            if (action != null) {
                action.run();
            }
        }

        @Override
        public IRegion getHyperlinkRegion() {
            return region;
        }
    }

}
