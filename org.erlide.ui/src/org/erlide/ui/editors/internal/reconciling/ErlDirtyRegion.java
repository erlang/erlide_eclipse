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
	 *            the dirty region with which to merge
	 */
	void mergeWith(final ErlDirtyRegion dr) {
		final int start = Math.min(fOffset, dr.fOffset);
		final int end = Math.max(fOffset + fLength, dr.fOffset + dr.fLength);
		fOffset = start;
		fLength = end - start;
		fText = dr.fText == null ? fText : fText == null ? dr.fText : fText
				+ dr.fText;
	}
}
