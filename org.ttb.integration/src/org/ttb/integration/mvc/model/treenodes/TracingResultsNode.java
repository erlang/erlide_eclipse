package org.ttb.integration.mvc.model.treenodes;

import java.util.Date;

import org.eclipse.swt.graphics.Image;
import org.ttb.integration.Activator;
import org.ttb.integration.Images;

/**
 * Node representing tracing results.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracingResultsNode extends TreeNode implements Comparable<TracingResultsNode> {

    private Date startDate;
    private Date endDate;
    private String fileName;
    private long size;

    public TracingResultsNode() {
        this(null);
    }

    public TracingResultsNode(String label) {
        this(label, Activator.getDefault().getImageRegistry().get(Images.ROOT_NODE.toString()));
    }

    public TracingResultsNode(String label, Image image) {
        super(label, image);
    }

    @Override
    public int hashCode() {
        // in set, when two objects have same hash code they are compared using
        // theirs equals methods
        return 0;
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || !o.getClass().equals(TracingResultsNode.class))
            return false;
        TracingResultsNode trn = (TracingResultsNode) o;
        return fileName.equals(trn.fileName);
    }

    public int compareTo(TracingResultsNode trn) {
        if (trn == null)
            throw new NullPointerException();

        if (this.equals(trn))
            return 0;
        if (!startDate.equals(trn.startDate))
            return startDate.compareTo(trn.startDate);
        else
            return endDate.compareTo(trn.endDate);
    }

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public long getSize() {
        return size;
    }

    public void setSize(long size) {
        this.size = size;
    }
}
