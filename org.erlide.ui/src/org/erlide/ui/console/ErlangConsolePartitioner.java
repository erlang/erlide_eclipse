package org.erlide.ui.console;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.TypedRegion;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.console.IConsoleDocumentPartitioner;

/**
 * This document partitioner does not actually partition things, but receives
 * ranges with the valid styles, so, each range maps to a style.
 * 
 * Aside from that, all is marked as the default content type.
 */
public class ErlangConsolePartitioner implements IConsoleDocumentPartitioner {

    private static final String[] LEGAL_CONTENT_TYPES = new String[] { IDocument.DEFAULT_CONTENT_TYPE };

    private final List<ScriptStyleRange> ranges = new ArrayList<ScriptStyleRange>();

    public ErlangConsolePartitioner() {
    }

    /**
     * Adds a given style range.
     * 
     * When a range is added, the ranges that were added before must be
     * removed/updated if the added range has some intersection with a previous
     * one.
     * 
     * The ranges must all be set sequentially, so, all the ranges that have
     * some intersection with that range must be removed/updated.
     * 
     * @param r
     *            the range to be added.
     */
    public void addRange(final ScriptStyleRange r) {
        if (r.length > 0) { // only add ranges with some len.
            for (int i = ranges.size() - 1; i >= 0; i--) {
                final ScriptStyleRange last = ranges.get(i);
                final int end = last.start + last.length;
                if (end > r.start) {
                    if (r.start <= last.start) {
                        ranges.remove(i);
                    } else {
                        last.length = r.start - last.start;
                    }
                } else {
                    break;
                }
            }

            boolean updatedRange = false;
            if (ranges.size() > 0) {
                final ScriptStyleRange lastRange = ranges
                        .get(ranges.size() - 1);
                if (lastRange.scriptType == r.scriptType) {
                    if (lastRange.start + lastRange.length == r.start) {
                        lastRange.length += r.length;
                        updatedRange = true;
                    }
                }
            }

            if (!updatedRange) {
                ranges.add(r);
            }
        }
    }

    /**
     * @return the ranges that intersect with the given offset/length.
     */
    @Override
    public ScriptStyleRange[] getStyleRanges(final int offset, final int length) {
        int lastOffset = -1;

        boolean found = false;

        final List<ScriptStyleRange> result = new ArrayList<ScriptStyleRange>();
        for (int i = ranges.size() - 1; i >= 0; i--) {
            final ScriptStyleRange r = ranges.get(i);
            if (r.start >= offset && r.start <= offset + length
                    || r.start < offset && r.start + r.length > offset) {
                found = true;
                // it must always be a copy because it may be changed later by
                // the TextConsole when
                // some hyperlink has a matching position.
                result.add(0, (ScriptStyleRange) r.clone());
                if (lastOffset == -1) {
                    lastOffset = r.start + r.length;
                }
            } else if (found) {
                // break on 1st not found (after finding the 1st)
                break;
            }
        }

        if (lastOffset == -1) {
            lastOffset = offset;
        }

        if (lastOffset < offset + length) {
            // if we haven't been able to cover the whole range, there's
            // probably something wrong (so, let's
            // leave it in gray so that we know about that).
            final ScriptStyleRange lastPart = new ScriptStyleRange(lastOffset,
                    offset + length - lastOffset, Display.getDefault()
                            .getSystemColor(SWT.COLOR_GRAY), Display
                            .getDefault().getSystemColor(SWT.COLOR_WHITE),
                    ScriptStyleRange.UNKNOWN);
            result.add(lastPart);
        }

        return result.toArray(new ScriptStyleRange[result.size()]);
    }

    // -------------------- Just return default content type for any related
    // request ------------------------------------

    @Override
    public boolean isReadOnly(final int offset) {
        return false;
    }

    @Override
    public ITypedRegion[] computePartitioning(final int offset, final int length) {
        return new TypedRegion[] { new TypedRegion(offset, length,
                IDocument.DEFAULT_CONTENT_TYPE) };
    }

    @Override
    public void connect(final IDocument document) {
    }

    @Override
    public void disconnect() {
    }

    @Override
    public void documentAboutToBeChanged(final DocumentEvent event) {
    }

    @Override
    public boolean documentChanged(final DocumentEvent event) {
        return false;
    }

    @Override
    public String getContentType(final int offset) {
        return IDocument.DEFAULT_CONTENT_TYPE;
    }

    @Override
    public String[] getLegalContentTypes() {
        return LEGAL_CONTENT_TYPES;
    }

    @Override
    public ITypedRegion getPartition(final int offset) {
        return new TypedRegion(offset, 1, IDocument.DEFAULT_CONTENT_TYPE);
    }
}
