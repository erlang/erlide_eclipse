/**
 * 
 */
package org.erlide.ui.internal.search;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.core.model.util.ErlangFunction;

public class ErlangSearchTreeContentProvider extends
        ErlangSearchContentProvider implements ITreeContentProvider {

    private final TreeViewer fTreeViewer;
    private final Map<Object, List<Object>> childMap;
    private final Map<Object, Object> parentMap;
    private final List<String> moduleNames;
    private ErlangSearchResult fResult;

    public ErlangSearchTreeContentProvider(final TreeViewer viewer,
            final ErlangSearchResultPage page) {
        super(page);
        fTreeViewer = viewer;
        childMap = new HashMap<Object, List<Object>>();
        parentMap = new HashMap<Object, Object>();
        moduleNames = new ArrayList<String>();
        // modules = new ArrayList<IErlModule>();
    }

    @Override
    public Object[] getElements(final Object inputElement) {
        return moduleNames.toArray();
    }

    @Override
    public void dispose() {
        // nothing to do
    }

    @Override
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        if (newInput instanceof ErlangSearchResult) {
            fResult = (ErlangSearchResult) newInput;
            initialize(fResult);
        }
    }

    @Override
    protected void initialize(final ErlangSearchResult result) {
        super.initialize(result);
        initialize(result.getResult());
    }

    private void addChild(final Object parent, final Object child) {
        if (!parentMap.containsKey(child)) {
            parentMap.put(child, parent);
        }
        if (childMap.containsKey(parent)) {
            final List<Object> children = childMap.get(parent);
            if (!children.contains(child)) {
                children.add(child);
            }
        } else {
            final List<Object> children = new ArrayList<Object>(1);
            children.add(child);
            childMap.put(parent, children);
        }
    }

    private void removeChild(final Object parent, final Object child) {
        if (parentMap.containsKey(child)) {
            parentMap.remove(child);
        }
        if (childMap.containsKey(parent)) {
            final List<Object> children = childMap.get(parent);
            if (children != null) {
                children.remove(child);
                if (children.isEmpty()) {
                    childMap.remove(parent);
                } else {
                    childMap.put(parent, children);
                }
            }
        }
    }

    protected synchronized void initialize(final List<ErlangSearchElement> eses) {
        if (eses == null) {
            return;
        }
        moduleNames.clear();
        parentMap.clear();
        childMap.clear();
        for (final ErlangSearchElement ese : eses) {
            addElement(ese);
        }

    }

    private void addElement(final ErlangSearchElement ese) {
        final String moduleName = ese.getModuleName();
        if (!moduleNames.contains(moduleName)) {
            moduleNames.add(moduleName);
        }
        if (ese.isSubClause()) {
            final ErlangFunction function = new ErlangFunction(ese.getName(),
                    ese.getArity());
            addChild(moduleName, function);
            addChild(function, ese);
        } else {
            addChild(moduleName, ese);
        }
    }

    private void removeElement(final ErlangSearchElement ese) {
        final String moduleName = ese.getModuleName();
        if (ese.isSubClause()) {
            final ErlangFunction function = new ErlangFunction(ese.getName(),
                    ese.getArity());
            removeChild(moduleName, function);
            removeChild(function, ese);
        } else {
            removeChild(moduleName, ese);
        }
        final List<Object> moduleChildren = childMap.get(moduleName);
        if (moduleChildren == null && moduleNames.contains(moduleName)) {
            moduleNames.remove(moduleName);
        }
    }

    @Override
    public Object[] getChildren(final Object parentElement) {
        final List<Object> l = childMap.get(parentElement);
        if (l == null) {
            return EMPTY_ARR;
        }
        return l.toArray();
    }

    @Override
    public boolean hasChildren(final Object element) {
        return childMap.containsKey(element);
    }

    @Override
    public Object getParent(final Object element) {
        return parentMap.get(element);
    }

    @Override
    public void clear() {
        initialize(fResult.getResult());
        fTreeViewer.refresh();
    }

    @Override
    public void elementsChanged(final Object[] updatedElements) {
        if (getSearchResult() == null) {
            return;
        }
        final TreeViewer viewer = getViewer();
        final int elementLimit = getElementLimit();
        final boolean treeLimited = elementLimit != -1;
        final int n = updatedElements.length;
        final List<Object> toRemove = new ArrayList<Object>(n);
        final List<Object> toAdd = new ArrayList<Object>(n);
        final List<Object> toUpdate = new ArrayList<Object>(n);
        for (final Object element : updatedElements) {
            if (fResult.getMatchCount(element) > 0) {
                if (viewer.testFindItem(element) != null) {
                    toUpdate.add(element);
                } else {
                    if (!treeLimited
                            || viewer.getTree().getItemCount() < elementLimit) {
                        toAdd.add(element);
                    }
                }
            } else {
                toRemove.add(element);
            }
        }
        for (final Object element : toRemove) {
            removeElement((ErlangSearchElement) element);
        }
        for (final Object element : toAdd) {
            addElement((ErlangSearchElement) element);
        }
        viewer.update(toUpdate.toArray(), null);
        viewer.remove(toRemove.toArray());
        viewer.refresh();
    }

    private int getElementLimit() {
        return getPage().getElementLimit().intValue();
    }

    private TreeViewer getViewer() {
        return fTreeViewer;
    }
}
