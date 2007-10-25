package org.erlide.ui.editors.erl;

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
import org.erlide.ui.actions.OpenAction;
import org.erlide.ui.util.ErlModelUtils;

public class ErlangHyperlinkDetector implements IHyperlinkDetector {

	private final ErlangEditor editor;

	private IErlModule fModule;

	/**
	 * @param editor
	 */
	public ErlangHyperlinkDetector(ErlangEditor editor) {
		this.editor = editor;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.text.hyperlink.IHyperlinkDetector#detectHyperlinks(org.eclipse.jface.text.ITextViewer,
	 *      org.eclipse.jface.text.IRegion, boolean)
	 */
	public IHyperlink[] detectHyperlinks(ITextViewer textViewer,
			IRegion region, boolean canShowMultipleHyperlinks) {

		if (region == null) {
			return null;
		}

		IDocument document = textViewer.getDocument();
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
	private IHyperlink[] detectHyperlinks(IDocument doc, int offset) {

		ITypedRegion partition;
		ErlPartition aPartion = new ErlPartition();

		fModule = ErlModelUtils.getModule(editor);
		ErlToken token = fModule.getScanner().getTokenAt(offset);

		if (token == null || !token.getKind().matches("atom")) {
			return null;
		}

		Region ErlRegion = new Region(token.getOffset(), token.getLength());

		try {
			partition = doc.getPartition(offset);

			aPartion.setLength(ErlRegion.getLength());
			aPartion.setOffset(ErlRegion.getOffset());
			aPartion.setAType(partition.getType());

			if (!IDocument.DEFAULT_CONTENT_TYPE.equals(aPartion.getAType())) {
				return null;
			}
		} catch (BadLocationException e) {
			return null;
		}

		return new IHyperlink[] { new ErlangSubHyperlink(editor, token
				.getContent(), aPartion) };
	}

	/**
	 * 
	 * 
	 */
	private static class ErlPartition implements IRegion {
		int length;
		int offset;
		String aType;

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.jface.text.IRegion#getLength()
		 */
		public int getLength() {
			return this.length;
		}

		/**
		 * @param length
		 */
		public void setLength(int length) {
			this.length = length;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.jface.text.IRegion#getOffset()
		 */
		public int getOffset() {
			return this.offset;
		}

		/**
		 * @param offset
		 */
		public void setOffset(int offset) {
			this.offset = offset;
		}

		/**
		 * @return
		 */
		public String getAType() {
			return this.aType;
		}

		/**
		 * @param string
		 */
		public void setAType(String string) {
			this.aType = string;
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
		 * @param subName
		 * @param partion
		 */
		public ErlangSubHyperlink(ErlangEditor editor, String subName,
				ErlPartition partion) {
			this.editor = editor;
			partion.setOffset(partion.getOffset() - 1);
			this.subNameRegion = partion;
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

			OpenAction action = (OpenAction) editor
					.getAction("org.erlide.ui.actions.open");

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
