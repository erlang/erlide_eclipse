package org.erlide.ui.internal.folding;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.projection.IProjectionListener;
import org.eclipse.jface.text.source.projection.IProjectionPosition;
import org.eclipse.jface.text.source.projection.ProjectionAnnotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotationModel;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.internal.model.root.ErlElementDelta;
import org.erlide.core.model.erlang.IErlComment;
import org.erlide.core.model.erlang.IErlMember;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.ISourceRange;
import org.erlide.core.model.erlang.ISourceReference;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElement.Kind;
import org.erlide.core.model.root.IErlElementDelta;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IParent;
import org.erlide.core.model.util.IElementChangedListener;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.folding.IErlangFoldingStructureProvider;
import org.erlide.ui.editors.erl.folding.IErlangFoldingStructureProviderExtension;
import org.erlide.ui.internal.DocumentCharacterIterator;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.prefs.PreferenceConstants;
import org.erlide.ui.util.ErlModelUtils;
import org.erlide.ui.util.PerformanceTuning;

public class DefaultErlangFoldingStructureProvider implements
        IProjectionListener, IErlangFoldingStructureProvider,
        IErlangFoldingStructureProviderExtension {

    private static final class ErlangProjectionAnnotation extends
            ProjectionAnnotation {

        private IErlElement fErlElement;

        private final boolean fIsComment;

        public ErlangProjectionAnnotation(final IErlElement element,
                final boolean isCollapsed, final boolean isComment) {
            super(isCollapsed);
            fErlElement = element;
            fIsComment = isComment;
        }

        public IErlElement getElement() {
            return fErlElement;
        }

        public void setElement(final IErlElement element) {
            fErlElement = element;
        }

        public boolean isComment() {
            return fIsComment;
        }

        /*
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "ErlangProjectionAnnotation:\n" + //$NON-NLS-1$
                    "\telement: \t'"
                    + fErlElement.toString()
                    + "' " + fErlElement.getKind() + "/" + fErlElement.getParent().toString() + "\n" + //$NON-NLS-1$ //$NON-NLS-2$
                    "\tcollapsed: \t" + isCollapsed() + "\n" + //$NON-NLS-1$ //$NON-NLS-2$
                    "\tcomment: \t" + fIsComment + "\n"; //$NON-NLS-1$ //$NON-NLS-2$
        }
    }

    private static final class Tuple {

        ErlangProjectionAnnotation annotation;

        Position position;

        Tuple(final ErlangProjectionAnnotation ann, final Position pos) {
            annotation = ann;
            position = pos;
        }
    }

    /**
     * Filter for annotations.
     * 
     * @since 3.2
     */
    private static interface Filter {

        boolean match(ErlangProjectionAnnotation annotation);
    }

    private static abstract class MatchCollapsedFilter implements Filter {

        private final boolean fMatchCollapsed;

        public MatchCollapsedFilter(final boolean matchCollapsed) {
            fMatchCollapsed = matchCollapsed;
        }

        public boolean stateMatch(final ErlangProjectionAnnotation annotation) {
            return fMatchCollapsed == annotation.isCollapsed();

        }

    }

    private static final class ErlangElementSetFilter extends
            MatchCollapsedFilter {

        private final Set<IErlElement> fSet;

        ErlangElementSetFilter(final Set<IErlElement> set,
                final boolean matchCollapsed) {
            super(matchCollapsed);
            fSet = set;
        }

        @Override
        public boolean match(final ErlangProjectionAnnotation annotation) {
            if (stateMatch(annotation) && !annotation.isComment()
                    && !annotation.isMarkedDeleted()) {
                final IErlElement element = annotation.getElement();
                if (fSet.contains(element)) {
                    return true;
                }
            }
            return false;
        }
    }

    /**
     * Projection position that will return two foldable regions: one folding
     * away the region from after the '/**' to the beginning of the content, the
     * other from after the first content line until after the comment.
     * 
     * @since 3.1
     */
    private static final class CommentPosition extends Position implements
            IProjectionPosition {

        CommentPosition(final int off, final int len) {
            super(off, len);
        }

        /*
         * @seeorg.eclipse.jface.text.source.projection.IProjectionPosition#
         * computeFoldingRegions(org.eclipse.jface.text.IDocument)
         */
        @Override
        public IRegion[] computeProjectionRegions(final IDocument document)
                throws BadLocationException {
            final DocumentCharacterIterator sequence = new DocumentCharacterIterator(
                    document, offset, offset + length);
            final int prefixEnd = 0;
            final int contentStart = findFirstContent(sequence, prefixEnd);

            final int firstLine = document.getLineOfOffset(offset + prefixEnd);
            final int captionLine = document.getLineOfOffset(offset
                    + contentStart);
            final int lastLine = document.getLineOfOffset(offset + length);

            Assert.isTrue(firstLine <= captionLine,
                    "first folded line is greater than the caption line"); //$NON-NLS-1$
            Assert.isTrue(captionLine <= lastLine,
                    "caption line is greater than the last folded line"); //$NON-NLS-1$

            IRegion preRegion;
            if (firstLine < captionLine) {
                // preRegion= new Region(offset + prefixEnd, contentStart -
                // prefixEnd);
                final int preOffset = document.getLineOffset(firstLine);
                final IRegion preEndLineInfo = document
                        .getLineInformation(captionLine);
                final int preEnd = preEndLineInfo.getOffset();
                preRegion = new Region(preOffset, preEnd - preOffset);
            } else {
                preRegion = null;
            }

            if (captionLine < lastLine) {
                final int postOffset = document.getLineOffset(captionLine + 1);
                final IRegion postRegion = new Region(postOffset, offset
                        + length - postOffset);

                if (preRegion == null) {
                    return new IRegion[] { postRegion };
                }

                return new IRegion[] { preRegion, postRegion };
            }

            if (preRegion != null) {
                return new IRegion[] { preRegion };
            }

            return null;
        }

        /**
         * Finds the offset of the first identifier part within
         * <code>content</code>. Returns 0 if none is found.
         * 
         * @param content
         *            the content to search
         * @return the first index of a unicode identifier part, or zero if none
         *         can be found
         */
        private int findFirstContent(final CharSequence content,
                final int prefixEnd) {
            final int lenght = content.length();
            for (int i = prefixEnd; i < lenght; i++) {
                if (Character.isUnicodeIdentifierPart(content.charAt(i))) {
                    return i;
                }
            }
            return 0;
        }

        // /**
        // * Finds the offset of the first identifier part within
        // * <code>content</code>. Returns 0 if none is found.
        // *
        // * @param content
        // * the content to search
        // * @return the first index of a unicode identifier part, or zero if
        // none
        // * can be found
        // */
        // private int findPrefixEnd(final CharSequence content) {
        // // return the index after the leading '/*' or '/**'
        // final int len = content.length();
        // int i = 0;
        // while (i < len && isWhiteSpace(content.charAt(i))) {
        // i++;
        // }
        // if (len >= i + 2 && content.charAt(i) == '/'
        // && content.charAt(i + 1) == '*') {
        // if (len >= i + 3 && content.charAt(i + 2) == '*') {
        // return i + 3;
        // } else {
        // return i + 2;
        // }
        // } else {
        // return i;
        // }
        // }

        // private boolean isWhiteSpace(final char c) {
        // return c == ' ' || c == '\t';
        // }

        /*
         * @seeorg.eclipse.jface.text.source.projection.IProjectionPosition#
         * computeCaptionOffset(org.eclipse.jface.text.IDocument)
         */
        @Override
        public int computeCaptionOffset(final IDocument document) {
            // return 0;
            final DocumentCharacterIterator sequence = new DocumentCharacterIterator(
                    document, offset, offset + length);
            return findFirstContent(sequence, 0);
        }
    }

    /**
     * Projection position that will return two foldable regions: one folding
     * away the lines before the one containing the simple name of the erlang
     * element, one folding away any lines after the caption.
     * 
     * @since 3.1
     */
    private static final class ErlangElementPosition extends Position implements
            IProjectionPosition {

        private IErlMember fMember;

        public ErlangElementPosition(final int off, final int len,
                final IErlMember member) {
            super(off, len);
            fMember = member;
        }

        public void setMember(final IErlMember member) {
            fMember = member;
        }

        @Override
        public String toString() {
            return "ErlangElementPosition " + offset + " " + length + " "
                    + fMember.getName();

        }

        /*
         * @seeorg.eclipse.jface.text.source.projection.IProjectionPosition#
         * computeFoldingRegions(org.eclipse.jface.text.IDocument)
         */
        @Override
        public IRegion[] computeProjectionRegions(final IDocument document)
                throws BadLocationException {
            int nameStart = offset;
            /*
             * The member's name range may not be correct. However, reconciling
             * would trigger another element delta which would lead to reentrant
             * situations. Therefore, we optimistically assume that the name
             * range is correct, but double check the received lines below.
             */
            final ISourceRange nameRange = fMember.getNameRange();
            if (nameRange != null) {
                nameStart = nameRange.getOffset();
            }

            final int firstLine = document.getLineOfOffset(offset);
            int captionLine = document.getLineOfOffset(nameStart);
            final int lastLine = document.getLineOfOffset(offset + length);

            /*
             * see comment above - adjust the caption line to be inside the
             * entire folded region, and rely on later element deltas to correct
             * the name range.
             */
            if (captionLine < firstLine) {
                captionLine = firstLine;
            }
            if (captionLine > lastLine) {
                captionLine = lastLine;
            }

            IRegion preRegion;
            if (firstLine < captionLine) {
                final int preOffset = document.getLineOffset(firstLine);
                final IRegion preEndLineInfo = document
                        .getLineInformation(captionLine);
                final int preEnd = preEndLineInfo.getOffset();
                preRegion = new Region(preOffset, preEnd - preOffset);
            } else {
                preRegion = null;
            }

            if (captionLine < lastLine) {
                final int postOffset = document.getLineOffset(captionLine + 1);
                final IRegion postRegion = new Region(postOffset, offset
                        + length - postOffset);

                if (preRegion == null) {
                    return new IRegion[] { postRegion };
                }

                return new IRegion[] { preRegion, postRegion };
            }

            if (preRegion != null) {
                return new IRegion[] { preRegion };
            }

            return null;
        }

        /*
         * @seeorg.eclipse.jface.text.source.projection.IProjectionPosition#
         * computeCaptionOffset(org.eclipse.jface.text.IDocument)
         */
        @Override
        public int computeCaptionOffset(final IDocument document)
                throws BadLocationException {
            int nameStart = offset;
            final ISourceRange nameRange = fMember.getNameRange();
            if (nameRange != null) {
                nameStart = nameRange.getOffset();
            }

            return nameStart - offset;
        }

    }

    private IDocument fCachedDocument;

    private ProjectionAnnotationModel fCachedModel;

    private ITextEditor fEditor;

    private ProjectionViewer fViewer;

    IErlModule fModule;

    private IElementChangedListener fElementListener;

    private boolean fAllowCollapsing = false;

    private boolean fFirstTimeInitialCollapse = true;

    private boolean fCollapseHeaderComments = true;

    private boolean fCollapseComments = false;

    private boolean fCollapseClauses = false;

    private boolean fCollapseTypespecs = false;

    private static class FunctionsFilter extends MatchCollapsedFilter {

        public FunctionsFilter(final boolean matchCollapsed) {
            super(matchCollapsed);
        }

        @Override
        public boolean match(final ErlangProjectionAnnotation annotation) {
            if (stateMatch(annotation) && !annotation.isComment()
                    && !annotation.isMarkedDeleted()) {
                final IErlElement element = annotation.getElement();
                final Kind kind = element.getKind();
                return kind == Kind.FUNCTION || kind == Kind.CLAUSE;
            }
            return false;
        }
    }

    private final class CommentsFilter extends MatchCollapsedFilter {

        public CommentsFilter(final boolean matchCollapsed) {
            super(matchCollapsed);
        }

        @Override
        public boolean match(final ErlangProjectionAnnotation annotation) {
            if (stateMatch(annotation) && annotation.isComment()
                    && !annotation.isMarkedDeleted()) {
                return true;
            }
            return false;
        }
    }

    /* filters */
    /**
     * Member filter, matches nested members (but not top-level types).
     * 
     * @since 3.2
     */
    private final Filter fCollapseFunctionsFilter = new FunctionsFilter(false);

    /**
     * Comment filter, matches comments.
     * 
     * @since 3.2
     */
    private final Filter fCollapseCommentsFilter = new CommentsFilter(false);

    private final Filter fExpandAllFilter = new Filter() {

        @Override
        public boolean match(final ErlangProjectionAnnotation annotation) {
            return annotation.isCollapsed();
        }

    };

    public DefaultErlangFoldingStructureProvider() {
    }

    @Override
    public void install(final ITextEditor editor, final ProjectionViewer viewer) {
        if (editor instanceof ErlangEditor) {
            fFirstTimeInitialCollapse = true;
            fEditor = editor;
            fViewer = viewer;
            fViewer.addProjectionListener(this);
            final IErlModel mdl = ErlModelManager.getErlangModel();
            mdl.addModelChangeListener(this);
        }
    }

    @Override
    public void uninstall() {
        if (isInstalled()) {
            projectionDisabled();
            fViewer.removeProjectionListener(this);
            fViewer = null;
            fEditor = null;
            ErlModelManager.getErlangModel().removeModelChangeListener(this);
        }
    }

    protected boolean isInstalled() {
        return fEditor != null;
    }

    /*
     * @seeorg.eclipse.jface.text.source.projection.IProjectionListener#
     * projectionEnabled()
     */
    @Override
    public void projectionEnabled() {
        // http://home.ott.oti.com/teams/wswb/anon/out/vms/index.html
        // projectionEnabled messages are not always paired with
        // projectionDisabled
        // i.e. multiple enabled messages may be sent out.
        // we have to make sure that we disable first when getting an enable
        // message.
        projectionDisabled();

        initialize();
        if (fEditor instanceof ErlangEditor && fModule != null) {
            boolean structureKnown = false;
            try {
                structureKnown = fModule.isStructureKnown();
            } catch (final ErlModelException e1) {
            }
            if (structureKnown) {
                final IErlElementDelta d = new ErlElementDelta(
                        IErlElementDelta.CHANGED, IErlElementDelta.F_CONTENT,
                        fModule);
                processDelta(d);
            } else {
                try {
                    fModule.open(null);
                } catch (final ErlModelException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    /*
     * @seeorg.eclipse.jface.text.source.projection.IProjectionListener#
     * projectionDisabled()
     */
    @Override
    public void projectionDisabled() {
        fCachedDocument = null;
        if (fElementListener != null) {
            ErlModelManager.getErlangModel().removeElementChangedListener(
                    fElementListener);
            fElementListener = null;
        }
    }

    @Override
    public void initialize() {
        if (!isInstalled()) {
            return;
        }

        initializePreferences();
        try {
            fModule = ErlModelUtils.getModule(fEditor.getEditorInput());
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }
    }

    private void initializePreferences() {
        final IPreferenceStore store = ErlideUIPlugin.getDefault()
                .getPreferenceStore();
        fAllowCollapsing = store
                .getBoolean(PreferenceConstants.EDITOR_FOLDING_ENABLED);
        fCollapseClauses = store
                .getBoolean(PreferenceConstants.EDITOR_FOLDING_CLAUSES);
        fCollapseHeaderComments = store
                .getBoolean(PreferenceConstants.EDITOR_FOLDING_HEADER_COMMENTS);
        fCollapseComments = store
                .getBoolean(PreferenceConstants.EDITOR_FOLDING_COMMENTS);
        fCollapseTypespecs = store
                .getBoolean(PreferenceConstants.EDITOR_FOLDING_TYPESPECS);
    }

    private void computeAdditions(final IErlModule erlModule,
            final Map<ErlangProjectionAnnotation, Position> map) {
        if (erlModule == null) {
            return;
        }
        try {
            computeAdditions((IParent) erlModule, map);
            computeAdditions(erlModule.getComments(), map);
        } catch (final ErlModelException x) {
            ErlLogger.warn(x);
        }
    }

    private void computeAdditions(final IParent parent,
            final Map<ErlangProjectionAnnotation, Position> map) {
        if (parent == null) {
            return;
        }
        try {
            computeAdditions(parent.getChildren(), map);
        } catch (final ErlModelException x) {
            ErlLogger.warn(x);
        }
    }

    private void computeAdditions(
            final Collection<? extends IErlElement> elements,
            final Map<ErlangProjectionAnnotation, Position> map)
            throws ErlModelException {
        if (elements == null) {
            return;
        }
        for (final IErlElement element : elements) {
            computeAdditions(element, map);
            if (element instanceof IParent) {
                final IParent parent = (IParent) element;
                computeAdditions(parent.getChildren(), map);
            }
        }
    }

    private void computeAdditions(final IErlElement element,
            final Map<ErlangProjectionAnnotation, Position> map) {
        boolean createProjection = false;
        boolean collapse = false;

        if (element.getKind() == IErlElement.Kind.CLAUSE
                || element.getKind() == IErlElement.Kind.FUNCTION) {
            collapse = fAllowCollapsing && fCollapseClauses;
            createProjection = true;
        } else if (element.getKind() == IErlElement.Kind.COMMENT) {
            final IErlComment c = (IErlComment) element;
            if (c.isHeader()) {
                collapse = fAllowCollapsing && fCollapseHeaderComments;
            } else {
                collapse = fAllowCollapsing && fCollapseComments;
            }
            createProjection = true;
        } else if (element.getKind() == IErlElement.Kind.ATTRIBUTE) {
            createProjection = true;
        } else if (element.getKind() == IErlElement.Kind.EXPORT) {
            createProjection = true;
        } else if (element.getKind() == IErlElement.Kind.RECORD_DEF) {
            createProjection = true;
        } else if (element.getKind() == IErlElement.Kind.MACRO_DEF) {
            createProjection = true;
        } else if (element.getKind() == IErlElement.Kind.TYPESPEC) {
            collapse = fAllowCollapsing && fCollapseTypespecs;
            createProjection = true;
        }
        if (createProjection) {
            final IRegion region = computeProjectionRanges(element);
            if (region != null) {
                final Position position = createProjectionPosition(region,
                        element);
                if (position != null) {
                    map.put(new ErlangProjectionAnnotation(element, collapse
                            && fFirstTimeInitialCollapse,
                            element instanceof IErlComment), position);
                }
            }
        }
    }

    // private boolean isInnerType(IType type) {
    // return type.getDeclaringType() != null;
    // }

    /**
     * Computes the projection ranges for a given <code>IErlElement</code>. More
     * than one range may be returned if the element has a leading comment which
     * gets folded separately. If there are no foldable regions,
     * <code>null</code> is returned.
     * 
     * @param element
     *            the erlang element that can be folded
     * @return the regions to be folded, or <code>null</code> if there are none
     */
    private IRegion computeProjectionRanges(final IErlElement element) {
        if (element instanceof ISourceReference) {
            final ISourceReference reference = (ISourceReference) element;
            final ISourceRange range = reference.getSourceRange();
            return new Region(range.getOffset(), range.getLength());
        }
        return null;
    }

    private Position createProjectionPosition(final IRegion region,
            final IErlElement element) {

        if (fCachedDocument == null) {
            return null;
        }

        try {

            final int start = fCachedDocument.getLineOfOffset(region
                    .getOffset());
            final int end = fCachedDocument.getLineOfOffset(region.getOffset()
                    + region.getLength());
            if (start != end) {
                final int offset = fCachedDocument.getLineOffset(start);
                int endOffset;
                if (fCachedDocument.getNumberOfLines() > end + 1) {
                    endOffset = fCachedDocument.getLineOffset(end + 1);
                } else if (end > start) {
                    endOffset = fCachedDocument.getLineOffset(end)
                            + fCachedDocument.getLineLength(end);
                } else {
                    return null;
                }
                if (element instanceof IErlComment) {
                    return new CommentPosition(offset, endOffset - offset);
                }
                if (element instanceof IErlMember) {
                    return new ErlangElementPosition(offset,
                            endOffset - offset, (IErlMember) element);
                }
            }

        } catch (final BadLocationException x) {
        }

        return null;
    }

    protected void processDelta(final IErlElementDelta delta) {
        if (!isInstalled()) {
            return;
        }

        if ((delta.getFlags() & (IErlElementDelta.F_CONTENT | IErlElementDelta.F_CHILDREN)) == 0) {
            return;
        }

        final IErlElement de = delta.getElement();
        if (de instanceof IErlModule && de != fModule) {
            return;
        }

        final ProjectionAnnotationModel model = (ProjectionAnnotationModel) fEditor
                .getAdapter(ProjectionAnnotationModel.class);
        if (model == null) {
            return;
        }

        final IDocumentProvider provider = fEditor.getDocumentProvider();

        try {
            fCachedModel = model;
            fCachedDocument = provider.getDocument(fEditor.getEditorInput());
            if (fCachedDocument.getNumberOfLines() > PerformanceTuning.get()
                    .getFoldingLimit()) {
                // disable folding for files larger than this
                model.removeAllAnnotations();
                return;
            }

            final Map<ErlangProjectionAnnotation, Position> additions = new HashMap<ErlangProjectionAnnotation, Position>();
            final List<ErlangProjectionAnnotation> deletions = new ArrayList<ErlangProjectionAnnotation>();
            final List<ErlangProjectionAnnotation> updates = new ArrayList<ErlangProjectionAnnotation>();

            // use a linked map to maintain ordering of comments
            final Map<ErlangProjectionAnnotation, Position> updated = new LinkedHashMap<ErlangProjectionAnnotation, Position>();

            computeAdditions(fModule, updated);
            final Map<Object, List<Tuple>> previous = createAnnotationMap(model);

            for (final Entry<ErlangProjectionAnnotation, Position> entry : updated
                    .entrySet()) {
                final ErlangProjectionAnnotation newAnnotation = entry.getKey();
                final IErlElement element = newAnnotation.getElement();
                final Position newPosition = entry.getValue();

                final List<Tuple> annotations = previous.get(element);
                if (annotations == null) {

                    additions.put(newAnnotation, newPosition);

                } else {
                    final Iterator<Tuple> x = annotations.iterator();
                    boolean matched = false;
                    while (x.hasNext()) {
                        final Tuple tuple = x.next();
                        final ErlangProjectionAnnotation existingAnnotation = tuple.annotation;
                        final Position existingPosition = tuple.position;
                        if (newAnnotation.isComment() == existingAnnotation
                                .isComment()) {
                            if (existingPosition != null
                                    && !newPosition.equals(existingPosition)) {
                                existingPosition.setOffset(newPosition
                                        .getOffset());
                                existingPosition.setLength(newPosition
                                        .getLength());
                                updates.add(existingAnnotation);
                            }
                            matched = true;
                            x.remove();
                            break;
                        }
                    }
                    if (!matched) {
                        additions.put(newAnnotation, newPosition);
                    }

                    if (annotations.isEmpty()) {
                        previous.remove(element);
                    }
                }
            }

            for (final List<Tuple> l : previous.values()) {
                for (final Tuple t : l) {
                    deletions.add(t.annotation);
                }
            }

            match(deletions, additions, updates);

            final Annotation[] removals = new Annotation[deletions.size()];
            deletions.toArray(removals);
            final Annotation[] changes = new Annotation[updates.size()];
            updates.toArray(changes);
            model.modifyAnnotations(removals, additions, changes);
            fFirstTimeInitialCollapse = false;
        } finally {
            fCachedDocument = null;
            fCachedModel = null;

            // fFirstType= null;
            // fHasHeaderComment = false;
        }
    }

    /**
     * Matches deleted annotations to changed or added ones. A deleted
     * annotation/position tuple that has a matching addition / change is
     * updated and marked as changed. The matching tuple is not added (for
     * additions) or marked as deletion instead (for changes). The result is
     * that more annotations are changed and fewer get deleted/re-added.
     */
    private void match(final List<ErlangProjectionAnnotation> deletions,
            final Map<ErlangProjectionAnnotation, Position> additions,
            final List<ErlangProjectionAnnotation> changes) {
        if (deletions.isEmpty() || additions.isEmpty() && changes.isEmpty()) {
            return;
        }

        final List<ErlangProjectionAnnotation> newDeletions = new ArrayList<ErlangProjectionAnnotation>();
        final List<ErlangProjectionAnnotation> newChanges = new ArrayList<ErlangProjectionAnnotation>();

        final Iterator<ErlangProjectionAnnotation> deletionIterator = deletions
                .iterator();
        while (deletionIterator.hasNext()) {
            final ErlangProjectionAnnotation deleted = deletionIterator.next();
            final Position deletedPosition = fCachedModel.getPosition(deleted);
            if (deletedPosition == null) {
                continue;
            }

            final Tuple deletedTuple = new Tuple(deleted, deletedPosition);

            Tuple match = findMatch(deletedTuple, changes, null);
            boolean addToDeletions = true;
            if (match == null) {
                match = findMatch(deletedTuple, additions.keySet(), additions);
                addToDeletions = false;
            }

            if (match != null) {
                final IErlElement element = match.annotation.getElement();
                deleted.setElement(element);
                deletedPosition.setLength(match.position.getLength());
                if (deletedPosition instanceof ErlangElementPosition
                        && element instanceof IErlMember) {
                    final ErlangElementPosition eep = (ErlangElementPosition) deletedPosition;
                    eep.setMember((IErlMember) element);
                }

                deletionIterator.remove();
                newChanges.add(deleted);

                if (addToDeletions) {
                    newDeletions.add(match.annotation);
                }
            }
        }

        deletions.addAll(newDeletions);
        changes.addAll(newChanges);
    }

    /**
     * Finds a match for <code>tuple</code> in a collection of annotations. The
     * positions for the <code>ErlangProjectionAnnotation</code> instances in
     * <code>annotations</code> can be found in the passed
     * <code>positionMap</code> or <code>fCachedModel</code> if
     * <code>positionMap</code> is <code>null</code>.
     * <p>
     * A tuple is said to match another if their annotations have the same
     * comment flag and their position offsets are equal.
     * </p>
     * <p>
     * If a match is found, the annotation gets removed from
     * <code>annotations</code>.
     * </p>
     * 
     * @param tuple
     *            the tuple for which we want to find a match
     * @param annotations
     *            collection of <code>ErlangProjectionAnnotation</code>
     * @param positionMap
     *            a <code>Map&lt;Annotation, Position&gt;</code> or
     *            <code>null</code>
     * @return a matching tuple or <code>null</code> for no match
     */
    private Tuple findMatch(final Tuple tuple,
            final Collection<ErlangProjectionAnnotation> annotations,
            final Map<ErlangProjectionAnnotation, Position> positionMap) {
        final Iterator<ErlangProjectionAnnotation> it = annotations.iterator();
        while (it.hasNext()) {
            final ErlangProjectionAnnotation annotation = it.next();
            if (tuple.annotation.isComment() == annotation.isComment()) {
                final Position position = positionMap == null ? fCachedModel
                        .getPosition(annotation) : positionMap.get(annotation);
                if (position == null) {
                    continue;
                }

                if (tuple.position.getOffset() == position.getOffset()) {
                    it.remove();
                    return new Tuple(annotation, position);
                }
            }
        }

        return null;
    }

    private Map<Object, List<Tuple>> createAnnotationMap(
            final IAnnotationModel model) {
        final Map<Object, List<Tuple>> map = new HashMap<Object, List<Tuple>>();
        final Iterator<?> e = model.getAnnotationIterator();
        while (e.hasNext()) {
            final Object annotation = e.next();
            if (annotation instanceof ErlangProjectionAnnotation) {
                final ErlangProjectionAnnotation epa = (ErlangProjectionAnnotation) annotation;
                final Position position = model.getPosition(epa);
                List<Tuple> list = map.get(epa.getElement());
                if (list == null) {
                    list = new ArrayList<Tuple>(2);
                    map.put(epa.getElement(), list);
                }
                list.add(new Tuple(epa, position));
            }
        }

        final Comparator<Tuple> comparator = new Comparator<Tuple>() {

            @Override
            public int compare(final Tuple o1, final Tuple o2) {
                return o1.position.getOffset() - o2.position.getOffset();
            }
        };
        for (final List<Tuple> name : map.values()) {
            final List<Tuple> list = name;
            Collections.sort(list, comparator);
        }
        return map;
    }

    @Override
    public void collapseFunctions() {
        modifyFiltered(fCollapseFunctionsFilter, false);
    }

    @Override
    public void collapseComments() {
        modifyFiltered(fCollapseCommentsFilter, false);
    }

    @Override
    public void expandAll() {
        modifyFiltered(fExpandAllFilter, true);
    }

    @Override
    public void collapseElements(final IErlElement[] elements) {
        final Set<IErlElement> set = new HashSet<IErlElement>(
                Arrays.asList(elements));
        modifyFiltered(new ErlangElementSetFilter(set, false), false);
    }

    @Override
    public void expandElements(final IErlElement[] elements) {
        final Set<IErlElement> set = new HashSet<IErlElement>(
                Arrays.asList(elements));
        modifyFiltered(new ErlangElementSetFilter(set, true), true);
    }

    /**
     * Collapses all annotations matched by the passed filter.
     * 
     * @param filter
     *            the filter to use to select which annotations to collapse
     * @param expand
     *            <code>true</code> to expand the matched annotations,
     *            <code>false</code> to collapse them
     * @since 3.2
     */
    private void modifyFiltered(final Filter filter, final boolean expand) {
        if (!isInstalled()) {
            return;
        }

        final ProjectionAnnotationModel model = (ProjectionAnnotationModel) fEditor
                .getAdapter(ProjectionAnnotationModel.class);
        if (model == null) {
            return;
        }

        final List<ErlangProjectionAnnotation> modified = new ArrayList<ErlangProjectionAnnotation>();
        final Iterator<?> iter = model.getAnnotationIterator();
        while (iter.hasNext()) {
            final Object annotation = iter.next();
            if (annotation instanceof ErlangProjectionAnnotation) {
                final ErlangProjectionAnnotation epa = (ErlangProjectionAnnotation) annotation;

                if (filter.match(epa)) {
                    if (expand) {
                        epa.markExpanded();
                    } else {
                        epa.markCollapsed();
                    }
                    modified.add(epa);
                }

            }
        }

        model.modifyAnnotations(null, null,
                modified.toArray(new Annotation[modified.size()]));
    }

    @Override
    public void elementChanged(final IErlElement element) {
        if (fEditor == null) {
            return;
        }
        final IDocumentProvider provider = fEditor.getDocumentProvider();
        if (provider == null) {
            return;
        }
        final IEditorInput input = fEditor.getEditorInput();
        if (input == null) {
            return;
        }
        fCachedDocument = provider.getDocument(input);
        if (fCachedDocument == null) {
            return;
        }
        // fFirstType= null;
        // fHasHeaderComment = false;
        try {

            final ProjectionAnnotationModel model = (ProjectionAnnotationModel) fEditor
                    .getAdapter(ProjectionAnnotationModel.class);
            if (model == null) {
                return;
            }
            fCachedModel = model;
            if (element instanceof IErlModule && element != fModule) {
                return;
            }
            final ErlElementDelta d = new ErlElementDelta(
                    IErlElementDelta.CHANGED, IErlElementDelta.F_CONTENT,
                    fModule);
            processDelta(d);
        } finally {
            fCachedDocument = null;
            fCachedModel = null;
        }
    }
}
