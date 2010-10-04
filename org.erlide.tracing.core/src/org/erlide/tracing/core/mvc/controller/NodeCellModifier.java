package org.erlide.tracing.core.mvc.controller;

import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.TableItem;
import org.erlide.tracing.core.mvc.model.TracedNode;
import org.erlide.tracing.core.mvc.view.NodeColumn;

/**
 * Cell modifier for nodes table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class NodeCellModifier implements ICellModifier {

    private final TableViewer tableViewer;

    /**
     * Creates cell modifier for trace patterns table.
     * 
     * @param tableViewer
     *            table viewer with which this cell modifier is used
     */
    public NodeCellModifier(TableViewer tableViewer) {
        this.tableViewer = tableViewer;
    }

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
        tableViewer.refresh();
    }
}
