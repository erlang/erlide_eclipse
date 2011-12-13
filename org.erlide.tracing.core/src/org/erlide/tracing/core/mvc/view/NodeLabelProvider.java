package org.erlide.tracing.core.mvc.view;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.erlide.tracing.core.Activator;
import org.erlide.tracing.core.Images;
import org.erlide.tracing.core.mvc.controller.NodeHelper;
import org.erlide.tracing.core.mvc.model.TracedNode;

/**
 * Provider responsible for displaying nodes in table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class NodeLabelProvider extends LabelProvider implements
        ITableLabelProvider {

    @Override
    public Image getColumnImage(final Object element, final int index) {
        final TracedNode node = (TracedNode) element;

        if (index == NodeColumn.ENABLED.ordinal()) {
            if (node.isEnabled()) {
                return Activator.getDefault().getImageRegistry()
                        .get(Images.CHECKED.toString());
            } else {
                return Activator.getDefault().getImageRegistry()
                        .get(Images.UNCHECKED.toString());
            }
        } else {
            return null;
        }
    }

    @Override
    public String getColumnText(final Object element, final int columnIndex) {
        final TracedNode node = (TracedNode) element;
        switch (NodeColumn.getByIndex(columnIndex)) {
        case COOKIE:
            return node.getCookie();
        case ENABLED:
            break;
        case NODE_NAME:
            return node.getNodeName();
        case TYPE:
            return NodeHelper.isExternal(node.getNodeName()) ? "external"
                    : "erlide";
        }
        return "";
    }
}
