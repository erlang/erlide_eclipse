package org.erlide.ui.tests;

import junit.framework.Assert;

import org.eclipse.core.runtime.CoreException;
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
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlProject;
import org.erlide.test.support.ErlideTestUtils;
import org.erlide.ui.editors.erl.completion.ErlContentAssistProcessor;
import org.erlide.ui.editors.erl.completion.ErlStringContentAssistProcessor;
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

        @Override
        public void setVisibleRegion(final int offset, final int length) {
        }

        @Override
        public void setUndoManager(final IUndoManager undoManager) {
        }

        @Override
        public void setTopIndex(final int index) {
        }

        @Override
        public void setTextHover(final ITextHover textViewerHover,
                final String contentType) {
        }

        @Override
        public void setTextDoubleClickStrategy(
                final ITextDoubleClickStrategy strategy,
                final String contentType) {
        }

        @Override
        public void setTextColor(final Color color, final int offset,
                final int length, final boolean controlRedraw) {
        }

        @Override
        public void setTextColor(final Color color) {
        }

        @Override
        public void setSelectedRange(final int offset, final int length) {
        }

        @Override
        public void setIndentPrefixes(final String[] indentPrefixes,
                final String contentType) {
        }

        @Override
        public void setEventConsumer(final IEventConsumer consumer) {
        }

        @Override
        public void setEditable(final boolean editable) {
        }

        @Override
        public void setDocument(final IDocument document,
                final int modelRangeOffset, final int modelRangeLength) {
        }

        @Override
        public void setDocument(final IDocument document) {
            this.document = document;
        }

        @Override
        public void setDefaultPrefixes(final String[] defaultPrefixes,
                final String contentType) {
        }

        @Override
        public void setAutoIndentStrategy(final IAutoIndentStrategy strategy,
                final String contentType) {
        }

        @Override
        public void revealRange(final int theOffset, final int length) {
        }

        @Override
        public void resetVisibleRegion() {
        }

        @Override
        public void resetPlugins() {
        }

        @Override
        public void removeViewportListener(final IViewportListener listener) {
        }

        @Override
        public void removeTextListener(final ITextListener listener) {
        }

        @Override
        public void removeTextInputListener(final ITextInputListener listener) {
        }

        @Override
        public boolean overlapsWithVisibleRegion(final int theOffset,
                final int length) {
            return false;
        }

        @Override
        public boolean isEditable() {
            return false;
        }

        @Override
        public void invalidateTextPresentation() {
        }

        @Override
        public IRegion getVisibleRegion() {
            return null;
        }

        @Override
        public int getTopInset() {
            return 0;
        }

        @Override
        public int getTopIndexStartOffset() {
            return 0;
        }

        @Override
        public int getTopIndex() {
            return 0;
        }

        @Override
        public StyledText getTextWidget() {
            return null;
        }

        @Override
        public ITextOperationTarget getTextOperationTarget() {
            return null;
        }

        @Override
        public ISelectionProvider getSelectionProvider() {
            return new ISelectionProvider() {

                @Override
                public void setSelection(final ISelection selection) {
                }

                @Override
                public void removeSelectionChangedListener(
                        final ISelectionChangedListener listener) {
                }

                @Override
                public ISelection getSelection() {
                    return new TextSelection(document, offset, 0);
                }

                @Override
                public void addSelectionChangedListener(
                        final ISelectionChangedListener listener) {
                }
            };
        }

        @Override
        public Point getSelectedRange() {
            return new Point(offset, 0);
        }

        @Override
        public IFindReplaceTarget getFindReplaceTarget() {
            return null;
        }

        @Override
        public IDocument getDocument() {
            return document;
        }

        @Override
        public int getBottomIndexEndOffset() {
            return 0;
        }

        @Override
        public int getBottomIndex() {
            return 0;
        }

        @Override
        public void changeTextPresentation(final TextPresentation presentation,
                final boolean controlRedraw) {

        }

        @Override
        public void addViewportListener(final IViewportListener listener) {
        }

        @Override
        public void addTextListener(final ITextListener listener) {
        }

        @Override
        public void addTextInputListener(final ITextInputListener listener) {
        }

        @Override
        public void activatePlugins() {
        }

        @Override
        public void showAnnotations(final boolean show) {
        }

        @Override
        public void setRangeIndicator(final Annotation rangeIndicator) {
        }

        @Override
        public void setRangeIndication(final int offset, final int length,
                final boolean moveCursor) {
        }

        @Override
        public void setDocument(final IDocument document,
                final IAnnotationModel annotationModel,
                final int modelRangeOffset, final int modelRangeLength) {
        }

        @Override
        public void setDocument(final IDocument document,
                final IAnnotationModel annotationModel) {
        }

        @Override
        public void setAnnotationHover(final IAnnotationHover annotationHover) {
        }

        @Override
        public void removeRangeIndication() {
        }

        @Override
        public IRegion getRangeIndication() {
            return null;
        }

        @Override
        public IAnnotationModel getAnnotationModel() {
            return null;
        }

        @Override
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
        ErlideTestUtils.initModulesAndIncludes();
    }

    @After
    public void tearDown() throws Exception {
        ErlideTestUtils.deleteModules();
    }

    @Test
    public void recordCompletionLettersTest() throws Exception {
        final String initialText = "-record(aa, {a, b}).\n-record(ab, {a, b}).\n-record(bb, {a, b}).\nf() ->\n#";
        completionTestWithoutParsing(initialText, 3, "a", 2, "aa");
    }

    @Test
    public void recordCompletionSingleQuoteTest() throws Exception {
        final String initialText = "-record('AA', {a, b}).\n-record('BB', {a, b}).\n-record(ab, {a, b}).\nf() ->\n#";
        completionTestWithoutParsing(initialText, 3, "'", 2, "'AA'");
    }

    public void completionTestWithParsing(final IErlProject project,
            final String name, final String text, final int offset,
            final String expectedText1,
            final boolean stringContentAssistProcessor) throws CoreException {
        final IDocument document = new StringDocument(text);
        final IErlModule module = ErlideTestUtils.createModule(project, name,
                text);
        module.open(null);
        final MockSourceViewer sourceViewer = new MockSourceViewer(document,
                offset);
        final IContentAssistProcessor p = stringContentAssistProcessor ? new ErlStringContentAssistProcessor(
                sourceViewer, module, null) : new ErlContentAssistProcessor(
                sourceViewer, module, null);
        final ICompletionProposal[] completionProposals = p
                .computeCompletionProposals(sourceViewer, offset);
        Assert.assertEquals(1, completionProposals.length);
        final String displayString1 = completionProposals[0].getDisplayString();
        Assert.assertEquals(expectedText1, displayString1);
    }

    // http://www.assembla.com/spaces/erlide/tickets/947
    // completion of include and include_lib
    @Test
    public void includeCompletionTest() throws Exception {
        ErlideTestUtils.initProjects();
        final String name1 = "testproject1";
        final IErlProject project = ErlideTestUtils.createProject(
                ErlideTestUtils.getTmpPath(name1), name1);
        try {
            ErlideTestUtils.createInclude(project, "a.hrl", "-define(A, a).\n");
            // check that quotes are added if needed
            completionTestWithParsing(project, "a.erl", "-include().\n", 9,
                    "\"a.hrl\"", false);
            // check that completion works in strings
            completionTestWithParsing(project, "b.erl", "-include(\"\").\n",
                    10, "a.hrl", true);
        } finally {
            ErlideTestUtils.deleteProjects();
        }
    }

    private void completionTestWithoutParsing(final String initialText,
            final int nTotalExpectedCompletions, final String completionChar,
            final int nExpectedCompletions, final String expectedFirstCompletion)
            throws ErlModelException {
        // http://www.assembla.com/spaces/erlide/tickets/593-completion--don-t-work-records-with-quoted-names-
        final int offset = initialText.length();
        IDocument document = new StringDocument(initialText);
        final IErlModule module = ErlideTestUtils
                .createModuleFromText(initialText);
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
