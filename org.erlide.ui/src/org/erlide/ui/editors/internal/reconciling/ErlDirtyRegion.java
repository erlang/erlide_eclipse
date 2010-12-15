package org.erlide.ui.editors.internal.reconciling;

public class ErlDirtyRegion {

    /** The region's offset. */
    private int fOffset;
    /** The region's length. (Removed length) */
    private int fLength;
    /** The text which has been inserted. */
    private String fText;

    /**
     * Creates a new dirty region.
     * 
     * @param offset
     *            the offset within the document where the change occurred
     * @param length
     *            the length of the text within the document that changed
     * @param text
     *            the substitution text
     */
    public ErlDirtyRegion(final int offset, final int length, final String text) {
        fOffset = offset;
        fLength = length;
        fText = text;
    }

    public int getOffset() {
        return fOffset;
    }

    public int getLength() {
        return fLength;
    }

    public int getTextLength() {
        return fText == null ? 0 : fText.length();
    }

    /**
     * Returns the text that changed as part of the region change.
     * 
     * @return the changed text
     */
    public String getText() {
        return fText;
    }

    /**
     * Modify the receiver so that it encompasses the region specified by the
     * dirty region.
     * 
     * @param dr
     *            the dirty region to merge with
     */
    public void mergeWith(final ErlDirtyRegion dr) {
        fOffset = Math.min(fOffset, dr.fOffset);
        fLength = fLength + dr.fLength;
        fText = dr.fText == null ? fText : fText == null ? dr.fText : fText
                + dr.fText;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("Ofs ").append(fOffset).append("  del ").append(fLength)
                .append("  ins ").append(getTextLength());
        return sb.toString();
    }

    /**
     * Check if the regions are adjacent and can be merged to one
     * 
     * @param nextMerge
     * @return
     */
    public boolean isMergable(final ErlDirtyRegion nextMerge) {
        return getOffset() - getLength() + getTextLength() == nextMerge
                .getOffset()
                || getOffset() == nextMerge.getOffset() - nextMerge.getLength()
                        + nextMerge.getTextLength();
    }
}
