package org.ttb.integration.mvc.model.treenodes;

import java.text.DateFormat;
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

    public TracingResultsNode() {
        this(null);
    }

    public TracingResultsNode(String label) {
        this(label, Activator.getDefault().getImageRegistry().get(Images.ROOT_NODE.toString()));
    }

    public TracingResultsNode(String label, Image image) {
        super(label, image);
    }

    /**
     * Creates node's label based on start and end date. It uses given formatter
     * to convert date into {@link String}.
     * 
     * @param formatter
     *            date formatter
     */
    public void generateLabel(DateFormat formatter) {
        StringBuilder builder = new StringBuilder();
        String s = builder.append(formatter.format(startDate)).append(" - ").append(formatter.format(endDate)).append(": ").append(getChildren().size())
                .append(" traces").toString();
        setLabel(s);
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
}
