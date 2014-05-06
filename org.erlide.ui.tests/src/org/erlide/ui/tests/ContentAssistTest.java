package org.erlide.ui.tests;

import java.util.List;

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
import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.util.ErlideTestUtils;
import org.erlide.ui.editors.erl.completion.ErlContentAssistProcessor;
import org.erlide.ui.editors.erl.completion.ErlStringContentAssistProcessor;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.google.common.collect.Lists;

@SuppressWarnings("deprecation")
public class ContentAssistTest {

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
    public void moduleCompletionTest() throws Exception {
        ErlideTestUtils.initProjects();
        final String name1 = "testproject1";
        final IErlProject project = ErlideTestUtils.createProject(
                ErlideTestUtils.getTmpPath(name1), name1);
        try {
            final String initialText = "application_";
            completionTest(project, "z.erl", initialText, initialText.length(),
                    Lists.newArrayList("application_controller:", "application_master:",
                            "application_starter:"), false);
        } finally {
            ErlideTestUtils.deleteProjects();
        }
    }

    @Test
    public void moduleCompletion1Test() throws Exception {
        ErlideTestUtils.initProjects();
        final String name1 = "testproject1";
        final IErlProject project = ErlideTestUtils.createProject(
                ErlideTestUtils.getTmpPath(name1), name1);
        try {
            completionTest(project, "ay.erl", "alarm_h", 7,
                    Lists.newArrayList("alarm_handler:"), false);
            completionTest(project, "azx.erl", "az", 2, Lists.newArrayList("azx:"), false);
        } finally {
            ErlideTestUtils.deleteProjects();
        }
    }

    @Test
    public void moduleCompletion2Test() throws Exception {
        ErlideTestUtils.initProjects();
        final String name1 = "testproject1";
        final IErlProject project = ErlideTestUtils.createProject(
                ErlideTestUtils.getTmpPath(name1), name1);
        try {
            completionTest(project, "a.erl", "'CosEventChannelAdmin_A", 23,
                    Lists.newArrayList("'CosEventChannelAdmin_AlreadyConnected':"), false);
        } finally {
            ErlideTestUtils.deleteProjects();
        }
    }

    @Test
    public void recordCompletionLettersTest() throws Exception {
        ErlideTestUtils.initProjects();
        final String name1 = "testproject1";
        final IErlProject project = ErlideTestUtils.createProject(
                ErlideTestUtils.getTmpPath(name1), name1);
        try {
            final String initialText = "-record(aa, {a, b}).\n-record(ab, {a, b}).\n-record(bb, {a, b}).\nf() ->\n#a";
            completionTest(project, "w.erl", initialText, initialText.length() - 1,
                    Lists.newArrayList("aa", "ab", "bb"), false);
            completionTest(project, "w2.erl", initialText, initialText.length(),
                    Lists.newArrayList("aa", "ab"), false);
        } finally {
            ErlideTestUtils.deleteProjects();
        }
    }

    @Test
    public void recordCompletionSingleQuoteTest() throws Exception {
        ErlideTestUtils.initProjects();
        final String name1 = "testproject1";
        final IErlProject project = ErlideTestUtils.createProject(
                ErlideTestUtils.getTmpPath(name1), name1);
        try {
            final String initialText = "-record('AA', {a, b}).\n-record('B', {a, b}).\n"
                    + "-record(ab, {a, b}).\nf() ->\n#'A";
            final int len = initialText.length();
            completionTest(project, "a1.erl", initialText, len - 2,
                    Lists.newArrayList("'AA'", "'B'", "ab"), false);
            completionTest(project, "a2.erl", initialText, len - 1,
                    Lists.newArrayList("'AA'", "'B'"), false);
            completionTest(project, "a3.erl", initialText, len,
                    Lists.newArrayList("'AA'"), false);
        } finally {
            ErlideTestUtils.deleteProjects();
        }
    }

    @Test
    public void caseInsensitiveProposalsTest() throws Exception {
        ErlideTestUtils.initProjects();
        final String name1 = "testproject1";
        final IErlProject project = ErlideTestUtils.createProject(
                ErlideTestUtils.getTmpPath(name1), name1);
        try {
            final String initialText1 = "-define(abc,abc).\n-define(aBc, aBc).\nf()->?ab";
            completionTest(project, "w.erl", initialText1, initialText1.length() - 1,
                    Lists.newArrayList("abc", "aBc"), false);
            final String initialText2 = "-define(abc,abc).\n-define(aBc, aBc).\nf()->?aB";
            completionTest(project, "w2.erl", initialText2, initialText2.length(),
                    Lists.newArrayList("aBc", "abc"), false);
        } finally {
            ErlideTestUtils.deleteProjects();
        }
    }

    public void completionTest(final IErlProject project, final String name,
            final String text, final int offset, final List<String> expected,
            final boolean inStrings) throws CoreException {
        final IDocument document = new StringDocument(text);
        final IErlModule module = ErlideTestUtils.createModule(project, name, text);
        module.open(null);
        final MockSourceViewer sourceViewer = new MockSourceViewer(document, offset);
        final IContentAssistProcessor p = inStrings ? new ErlStringContentAssistProcessor(
                sourceViewer, module, project, null) : new ErlContentAssistProcessor(
                sourceViewer, module, project, null);
        final ICompletionProposal[] completionProposals = p.computeCompletionProposals(
                sourceViewer, offset);

        MatcherAssert.assertThat(ListExtensions.map(
                Lists.newArrayList(completionProposals),
                new Functions.Function1<ICompletionProposal, String>() {
                    @Override
                    public String apply(final ICompletionProposal cp) {
                        return cp.getDisplayString();
                    }
                }), Matchers.is(expected));
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
            completionTest(project, "a.erl", "-include().\n", 9,
                    Lists.newArrayList("\"a.hrl\""), false);
            // check that completion works in strings
            completionTest(project, "b.erl", "-include(\"\").\n", 10,
                    Lists.newArrayList("a.hrl"), true);
        } finally {
            ErlideTestUtils.deleteProjects();
        }
    }

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
        public void setTextDoubleClickStrategy(final ITextDoubleClickStrategy strategy,
                final String contentType) {
        }

        @Override
        public void setTextColor(final Color color, final int offset, final int length,
                final boolean controlRedraw) {
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
        public void setDocument(final IDocument document, final int modelRangeOffset,
                final int modelRangeLength) {
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
        public boolean overlapsWithVisibleRegion(final int theOffset, final int length) {
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
                final IAnnotationModel annotationModel, final int modelRangeOffset,
                final int modelRangeLength) {
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

}
