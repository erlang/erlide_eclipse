/**
 * 
 */
package org.erlide.ui.search;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.core.erlang.util.ErlangFunction;

public class ErlangSearchTreeContentProvider extends
		ErlangSearchContentProvider implements ITreeContentProvider {

	private final Object[] EMPTY_ARR = new Object[0];

	private final AbstractTreeViewer fTreeViewer;
	private final Map<Object, List<Object>> childMap;
	private final Map<Object, Object> parentMap;
	private final List<String> moduleNames;
	private ErlangSearchResult fResult;

	public ErlangSearchTreeContentProvider(final AbstractTreeViewer viewer,
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
			final String moduleName = ese.getModuleName();
			if (!moduleNames.contains(moduleName)) {
				moduleNames.add(moduleName);
			}
			final ErlangFunction function = ese.getFunction();
			if (ese.isSubClause()) {
				addChild(moduleName, function);
				addChild(function, ese);
			} else {
				addChild(moduleName, ese);
			}
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
		// FIXME ska det vara så här? eller ska vi kolla med
		// updatedElements?
		// vi måste nog kolla med updatedElements för att remove ska
		// funka...
		clear();
	}
}