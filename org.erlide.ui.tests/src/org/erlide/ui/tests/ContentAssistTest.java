package org.erlide.ui.tests;

import junit.framework.Assert;

import org.eclipse.jface.text.IAutoIndentStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IEventConsumer;
import org.eclipse.jface.text.IFindReplaceTarget;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.ITextInputListener;
import org.eclipse.jface.text.ITextListener;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.text.IUndoManager;
import org.eclipse.jface.text.IViewportListener;
import org.eclipse.jface.text.TextPresentation;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationHover;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlModule;
import org.erlide.test.support.ErlideTestUtils;
import org.erlide.ui.editors.erl.completion.ErlContentAssistProcessor;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

@SuppressWarnings("deprecation")
public class ContentAssistTest {

	private static final class MockSourceViewer implements ISourceViewer {
		private IDocument document;
		private int offset;

		public MockSourceViewer(final IDocument document, final int offset) {
			this.document = document;
			setOffset(offset);
		}

		public void setVisibleRegion(final int offset, final int length) {
		}

		public void setUndoManager(final IUndoManager undoManager) {
		}

		public void setTopIndex(final int index) {
		}

		public void setTextHover(final ITextHover textViewerHover,
				final String contentType) {
		}

		public void setTextDoubleClickStrategy(
				final ITextDoubleClickStrategy strategy,
				final String contentType) {
		}

		public void setTextColor(final Color color, final int offset,
				final int length, final boolean controlRedraw) {
		}

		public void setTextColor(final Color color) {
		}

		public void setSelectedRange(final int offset, final int length) {
		}

		public void setIndentPrefixes(final String[] indentPrefixes,
				final String contentType) {
		}

		public void setEventConsumer(final IEventConsumer consumer) {
		}

		public void setEditable(final boolean editable) {
		}

		public void setDocument(final IDocument document,
				final int modelRangeOffset, final int modelRangeLength) {
		}

		public void setDocument(final IDocument document) {
			this.document = document;
		}

		public void setDefaultPrefixes(final String[] defaultPrefixes,
				final String contentType) {
		}

		public void setAutoIndentStrategy(final IAutoIndentStrategy strategy,
				final String contentType) {
		}

		public void revealRange(final int theOffset, final int length) {
		}

		public void resetVisibleRegion() {
		}

		public void resetPlugins() {
		}

		public void removeViewportListener(final IViewportListener listener) {
		}

		public void removeTextListener(final ITextListener listener) {
		}

		public void removeTextInputListener(final ITextInputListener listener) {
		}

		public boolean overlapsWithVisibleRegion(final int theOffset,
				final int length) {
			return false;
		}

		public boolean isEditable() {
			return false;
		}

		public void invalidateTextPresentation() {
		}

		public IRegion getVisibleRegion() {
			return null;
		}

		public int getTopInset() {
			return 0;
		}

		public int getTopIndexStartOffset() {
			return 0;
		}

		public int getTopIndex() {
			return 0;
		}

		public StyledText getTextWidget() {
			return null;
		}

		public ITextOperationTarget getTextOperationTarget() {
			return null;
		}

		public ISelectionProvider getSelectionProvider() {
			return new ISelectionProvider() {

				public void setSelection(final ISelection selection) {
				}

				public void removeSelectionChangedListener(
						final ISelectionChangedListener listener) {
				}

				public ISelection getSelection() {
					return new TextSelection(document, offset, 0);
				}

				public void addSelectionChangedListener(
						final ISelectionChangedListener listener) {
				}
			};
		}

		public Point getSelectedRange() {
			return new Point(offset, 0);
		}

		public IFindReplaceTarget getFindReplaceTarget() {
			return null;
		}

		public IDocument getDocument() {
			return document;
		}

		public int getBottomIndexEndOffset() {
			return 0;
		}

		public int getBottomIndex() {
			return 0;
		}

		public void changeTextPresentation(final TextPresentation presentation,
				final boolean controlRedraw) {

		}

		public void addViewportListener(final IViewportListener listener) {
		}

		public void addTextListener(final ITextListener listener) {
		}

		public void addTextInputListener(final ITextInputListener listener) {
		}

		public void activatePlugins() {
		}

		public void showAnnotations(final boolean show) {
		}

		public void setRangeIndicator(final Annotation rangeIndicator) {
		}

		public void setRangeIndication(final int offset, final int length,
				final boolean moveCursor) {
		}

		public void setDocument(final IDocument document,
				final IAnnotationModel annotationModel,
				final int modelRangeOffset, final int modelRangeLength) {
		}

		public void setDocument(final IDocument document,
				final IAnnotationModel annotationModel) {
		}

		public void setAnnotationHover(final IAnnotationHover annotationHover) {
		}

		public void removeRangeIndication() {
		}

		public IRegion getRangeIndication() {
			return null;
		}

		public IAnnotationModel getAnnotationModel() {
			return null;
		}

		public void configure(final SourceViewerConfiguration configuration) {
		}

		public void setOffset(final int offset) {
			this.offset = offset;
		}
	}

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	@Before
	public void setUp() throws Exception {
		ErlideTestUtils.initModules();
	}

	@After
	public void tearDown() throws Exception {
		ErlideTestUtils.deleteModules();
	}

	@Test
	public void recordCompletionLettersTest() throws Exception {
		final String initialText = "-record(aa, {a, b}).\n-record(ab, {a, b}).\n-record(bb, {a, b}).\nf() ->\n#";
		completionTest(initialText, 3, "a", 2, "aa");
	}

	@Test
	public void recordCompletionSingleQuoteTest() throws Exception {
		final String initialText = "-record('AA', {a, b}).\n-record('BB', {a, b}).\n-record(ab, {a, b}).\nf() ->\n#";
		completionTest(initialText, 3, "'", 2, "'AA'");
	}

	private void completionTest(final String initialText,
			final int nTotalExpectedCompletions, final String completionChar,
			final int nExpectedCompletions, final String expectedFirstCompletion)
			throws ErlModelException {
		// http://www.assembla.com/spaces/erlide/tickets/593-completion--don-t-work-records-with-quoted-names-
		final int offset = initialText.length();
		IDocument document = new StringDocument(initialText);
		final IErlModule module = ErlideTestUtils
				.createModuleFromText(initialText);
		// final IErlModule module = ErlangCore.getModelManager()
		// .getModuleFromText(null, "test1", initialText,
		// "test_" + initialText.hashCode());
		final MockSourceViewer sourceViewer = new MockSourceViewer(document,
				offset);
		final IContentAssistProcessor p = new ErlContentAssistProcessor(
				sourceViewer, module, null); // null is ok since we don't call
												// setToPrefs
		ICompletionProposal[] completionProposals = p
				.computeCompletionProposals(sourceViewer, offset);
		Assert.assertEquals(nTotalExpectedCompletions,
				completionProposals.length);
		document = new StringDocument(initialText + completionChar);
		sourceViewer.setDocument(document);
		sourceViewer.setOffset(offset + 1);
		completionProposals = p.computeCompletionProposals(sourceViewer,
				offset + 1);
		Assert.assertEquals(nExpectedCompletions, completionProposals.length);
		Assert.assertEquals(expectedFirstCompletion,
				completionProposals[0].getDisplayString());
	}
}
