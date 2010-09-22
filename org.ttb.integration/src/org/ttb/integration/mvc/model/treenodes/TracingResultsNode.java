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
public class TracingResultsNode extends TreeNode {

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
