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
import org.erlide.core.erlang.util.ErlangFunction;

public class ErlangSearchTreeContentProvider extends
		ErlangSearchContentProvider implements ITreeContentProvider {

	private final Object[] EMPTY_ARR = new Object[0];

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
			initialize(fResult.getResult());
		}
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

	public Object[] getChildren(final Object parentElement) {
		final List<Object> l = childMap.get(parentElement);
		if (l == null) {
			return EMPTY_ARR;
		}
		return l.toArray();
	}

	public boolean hasChildren(final Object element) {
		return childMap.containsKey(element);
	}

	// @Override
	// public synchronized void elementsChanged(final Object[]
	// updatedElements) {
	// for (int i = 0; i < updatedElements.length; i++) {
	// if (fResult.getMatchCount(updatedElements[i]) > 0) {
	// insert(updatedElements[i], true);
	// } else {
	// remove(updatedElements[i], true);
	// }
	// }
	// }

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
		final TreeViewer viewer = getViewer();
		final int elementLimit = getElementLimit();
		final boolean treeLimited = elementLimit != -1;
		List<Object> toRemove = new ArrayList<Object>();
		for (Object element : updatedElements) {
			if (fResult.getMatchCount(element) > 0) {
				if (viewer.testFindItem(element) != null) {
					viewer.update(element, null);
				} else {
					if (!treeLimited
							|| viewer.getTree().getItemCount() < elementLimit) {
						addElement((ErlangSearchElement) element);
					}
				}
			} else {
				toRemove.add(element);
			}
		}
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