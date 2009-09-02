package org.erlide.ui.editors.erl.hyperlink;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.eclipse.jface.text.hyperlink.IHyperlinkDetector;
import org.erlide.core.erlang.ErlToken;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlScanner;
import org.erlide.ui.actions.OpenAction;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.IErlangEditorActionDefinitionIds;
import org.erlide.ui.util.ErlModelUtils;

public class ErlangHyperlinkDetector implements IHyperlinkDetector {

	private final ErlangEditor editor;

	/**
	 * @param editor
	 */
	public ErlangHyperlinkDetector(final ErlangEditor editor) {
		this.editor = editor;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jface.text.hyperlink.IHyperlinkDetector#detectHyperlinks(
	 * org.eclipse.jface.text.ITextViewer, org.eclipse.jface.text.IRegion,
	 * boolean)
	 */
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

	/**
	 * @param doc
	 * @param offset
	 * @return
	 */
	private IHyperlink[] detectHyperlinks(final IDocument doc, final int offset) {

		final IErlModule module = ErlModelUtils.getModule(editor);
		if (module == null) {
			return null;
		}
		final IErlScanner scanner = module.getScanner();
		if (scanner == null) {
			return null;
		}
		final ErlToken token = scanner.getTokenAt(offset);
		if (token == null) {
			return null;
		}
		final String tokenKind = token.getKind();
		if (!tokenKind.equals("atom") && !tokenKind.equals("string")
				&& !tokenKind.equals("macro") && !tokenKind.equals("var")) {
			return null;
		}
		try {
			final ITypedRegion partition = doc.getPartition(offset);
			final ErlPartition erlPartition = new ErlPartition(token
					.getOffset(), token.getLength(), partition.getType());
			if (!IDocument.DEFAULT_CONTENT_TYPE.equals(erlPartition.getType())) {
				return null;
			}
			return new IHyperlink[] { new ErlangSubHyperlink(editor,
					erlPartition) };
		} catch (final BadLocationException e) {
			return null;
		}
	}

	/**
	 * 
	 * 
	 */
	static class ErlPartition extends Region {
		String type;

		public ErlPartition(final int offset, final int length,
				final String type) {
			super(offset, length);
			this.type = type;
		}

		/**
		 * @return
		 */
		public String getType() {
			return type;
		}

		/**
		 * @param string
		 */
		public void setType(final String string) {
			type = string;
		}

	}

	/**
	 * 
	 * 
	 */
	private static class ErlangSubHyperlink implements IHyperlink {
		private final ErlangEditor editor;
		private final ErlPartition subNameRegion;

		/**
		 * @param editor
		 * @param partion
		 */
		public ErlangSubHyperlink(final ErlangEditor editor,
				final ErlPartition partion) {
			this.editor = editor;
			subNameRegion = partion;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.jface.text.hyperlink.IHyperlink#getTypeLabel()
		 */
		public String getTypeLabel() {
			return null;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.jface.text.hyperlink.IHyperlink#getHyperlinkText()
		 */
		public String getHyperlinkText() {
			return null;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.jface.text.hyperlink.IHyperlink#open()
		 */
		public void open() {
			final OpenAction action = (OpenAction) editor
					.getAction(IErlangEditorActionDefinitionIds.OPEN);
			if (action != null) {
				action.run();
			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.jface.text.hyperlink.IHyperlink#getHyperlinkRegion()
		 */
		public IRegion getHyperlinkRegion() {
			return subNameRegion;
		}
	}

}
