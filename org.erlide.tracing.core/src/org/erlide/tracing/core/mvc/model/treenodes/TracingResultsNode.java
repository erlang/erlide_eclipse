package org.erlide.tracing.core.mvc.model.treenodes;

import java.util.Date;

import org.eclipse.swt.graphics.Image;
import org.erlide.tracing.core.Activator;
import org.erlide.tracing.core.Images;

/**
 * Node representing tracing results.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracingResultsNode extends TreeNode implements
        Comparable<TracingResultsNode> {

    private Date startDate;
    private Date endDate;
    private String fileName;
    private long size;

    public TracingResultsNode() {
        this(null);
    }

    public TracingResultsNode(final String label) {
        this(label, Activator.getDefault().getImageRegistry()
                .get(Images.ROOT_NODE.toString()));
    }

    public TracingResultsNode(final String label, final Image image) {
        super(label, image);
    }

    @Override
    public int hashCode() {
        // in set, when two objects have same hash code they are compared using
        // theirs equals methods
        return 0;
    }

    @Override
    public boolean equals(final Object o) {
        if (o == null || !o.getClass().equals(TracingResultsNode.class)) {
            return false;
        }
        final TracingResultsNode trn = (TracingResultsNode) o;
        return fileName.equals(trn.fileName);
    }

    @Override
    public int compareTo(final TracingResultsNode trn) {
        if (trn == null) {
            throw new NullPointerException();
        }

        if (equals(trn)) {
            return 0;
        }
        if (!startDate.equals(trn.startDate)) {
            return startDate.compareTo(trn.startDate);
        } else {
            return endDate.compareTo(trn.endDate);
        }
    }

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(final Date startDate) {
        this.startDate = startDate;
    }

    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(final Date endDate) {
        this.endDate = endDate;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(final String fileName) {
        this.fileName = fileName;
    }

    public long getSize() {
        return size;
    }

    public void setSize(final long size) {
        this.size = size;
    }
}
