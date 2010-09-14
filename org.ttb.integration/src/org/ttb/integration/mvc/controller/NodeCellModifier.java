package org.ttb.integration.mvc.controller;

import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.swt.widgets.TableItem;
import org.ttb.integration.TraceBackend;
import org.ttb.integration.mvc.model.TracedNode;
import org.ttb.integration.mvc.view.NodeColumn;

public class NodeCellModifier implements ICellModifier {

    public boolean canModify(Object element, String property) {
        return true;
    }

    public Object getValue(Object element, String property) {
        TracedNode node = (TracedNode) element;
        switch (NodeColumn.valueOf(property)) {
        case COOKIE:
            return node.getCookie();
        case ENABLED:
            return new Boolean(node.isEnabled());
        case NODE_NAME:
            return node.getNodeName();
        default:
            return null;
        }
    }

    public void modify(Object element, String property, Object value) {
        TracedNode node = (TracedNode) ((TableItem) element).getData();
        switch (NodeColumn.valueOf(property)) {
        case COOKIE:
            node.setCookie((String) value);
            break;
        case ENABLED:
            node.setEnabled((Boolean) value);
            break;
        case NODE_NAME:
            node.setNodeName((String) value);
            break;
        }
        TraceBackend.getInstance().updateTracedNode(node);
    }
}
