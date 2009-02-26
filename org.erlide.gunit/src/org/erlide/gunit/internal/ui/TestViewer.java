/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.erlide.gunit.internal.ui;

import java.util.AbstractList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.TableItem;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.part.PageBook;

import org.eclipse.debug.core.ILaunchManager;

import org.erlide.gunit.internal.model.TestCaseElement;
import org.erlide.gunit.internal.model.TestElement;
import org.erlide.gunit.internal.model.TestRoot;
import org.erlide.gunit.internal.model.TestRunSession;
import org.erlide.gunit.internal.model.TestSuiteElement;
import org.erlide.gunit.internal.model.TestElement.Status;

public class TestViewer {
	private final class TestSelectionListener implements
			ISelectionChangedListener {
		public void selectionChanged(SelectionChangedEvent event) {
			handleSelected();
		}
	}

	private final class TestOpenListener extends SelectionAdapter {
		@Override
		public void widgetDefaultSelected(SelectionEvent e) {
			handleDefaultSelected();
		}
	}

	private final class FailuresOnlyFilter extends ViewerFilter {
		@Override
		public boolean select(Viewer viewer, Object parentElement,
				Object element) {
			return select(((TestElement) element));
		}

		public boolean select(TestElement testElement) {
			Status status = testElement.getStatus();
			if (status.isErrorOrFailure())
				return true;
			else
				return !fTestRunSession.isRunning() && status == Status.RUNNING; // rerunning
		}
	}

	private static class ReverseList extends AbstractList {
		private final List fList;

		public ReverseList(List list) {
			fList = list;
		}

		@Override
		public Object get(int index) {
			return fList.get(fList.size() - index - 1);
		}

		@Override
		public int size() {
			return fList.size();
		}
	}

	private class ExpandAllAction extends Action {
		public ExpandAllAction() {
			setText(JUnitMessages.ExpandAllAction_text);
			setToolTipText(JUnitMessages.ExpandAllAction_tooltip);
		}

		@Override
		public void run() {
			fTreeViewer.expandAll();
		}
	}

	private final FailuresOnlyFilter fFailuresOnlyFilter = new FailuresOnlyFilter();

	private final TestRunnerViewPart fTestRunnerPart;
	private final Clipboard fClipboard;

	private PageBook fViewerbook;
	private TreeViewer fTreeViewer;
	private TestSessionTreeContentProvider fTreeContentProvider;
	private TestSessionLabelProvider fTreeLabelProvider;
	private TableViewer fTableViewer;
	private TestSessionTableContentProvider fTableContentProvider;
	private TestSessionLabelProvider fTableLabelProvider;
	private SelectionProviderMediator fSelectionProvider;

	private final Image fHierarchyIcon;

	private int fLayoutMode;
	private boolean fTreeHasFilter;
	private boolean fTableHasFilter;

	private TestRunSession fTestRunSession;

	private boolean fTreeNeedsRefresh;
	private boolean fTableNeedsRefresh;
	private HashSet/* <TestElement> */fNeedUpdate;
	private TestCaseElement fAutoScrollTarget;

	private LinkedList/* <TestSuiteElement> */fAutoClose;
	private HashSet/* <TestSuite> */fAutoExpand;

	public TestViewer(Composite parent, Clipboard clipboard,
			TestRunnerViewPart runner) {
		fTestRunnerPart = runner;
		fClipboard = clipboard;

		fHierarchyIcon = TestRunnerViewPart.createImage("obj16/testhier.gif"); //$NON-NLS-1$
		parent.addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent e) {
				disposeIcons();
			}
		});

		fLayoutMode = TestRunnerViewPart.LAYOUT_HIERARCHICAL;

		createTestViewers(parent);

		registerViewersRefresh();

		initContextMenu();
	}

	private void createTestViewers(Composite parent) {
		fViewerbook = new PageBook(parent, SWT.NULL);

		fTreeViewer = new TreeViewer(fViewerbook, SWT.V_SCROLL | SWT.SINGLE);
		ColoredViewersManager.install(fTreeViewer);
		fTreeViewer.setUseHashlookup(true);
		fTreeContentProvider = new TestSessionTreeContentProvider();
		fTreeViewer.setContentProvider(fTreeContentProvider);
		fTreeLabelProvider = new TestSessionLabelProvider(fTestRunnerPart,
				TestRunnerViewPart.LAYOUT_HIERARCHICAL);
		fTreeViewer.setLabelProvider(fTreeLabelProvider);

		fTableViewer = new TableViewer(fViewerbook, SWT.V_SCROLL | SWT.H_SCROLL
				| SWT.SINGLE);
		ColoredViewersManager.install(fTableViewer);
		fTableViewer.setUseHashlookup(true);
		fTableContentProvider = new TestSessionTableContentProvider();
		fTableViewer.setContentProvider(fTableContentProvider);
		fTableLabelProvider = new TestSessionLabelProvider(fTestRunnerPart,
				TestRunnerViewPart.LAYOUT_FLAT);
		fTableViewer.setLabelProvider(fTableLabelProvider);

		fSelectionProvider = new SelectionProviderMediator(
				new StructuredViewer[] { fTreeViewer, fTableViewer },
				fTreeViewer);
		fSelectionProvider
				.addSelectionChangedListener(new TestSelectionListener());
		TestOpenListener testOpenListener = new TestOpenListener();
		fTreeViewer.getTree().addSelectionListener(testOpenListener);
		fTableViewer.getTable().addSelectionListener(testOpenListener);

		fViewerbook.showPage(fTreeViewer.getTree());
	}

	private void initContextMenu() {
		MenuManager menuMgr = new MenuManager("#PopupMenu"); //$NON-NLS-1$
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				handleMenuAboutToShow(manager);
			}
		});
		fTestRunnerPart.getSite().registerContextMenu(menuMgr,
				fSelectionProvider);
		Menu menu = menuMgr.createContextMenu(fViewerbook);
		fTreeViewer.getTree().setMenu(menu);
		fTableViewer.getTable().setMenu(menu);
	}

	void handleMenuAboutToShow(IMenuManager manager) {
		IStructuredSelection selection = (IStructuredSelection) fSelectionProvider
				.getSelection();
		if (!selection.isEmpty()) {
			TestElement testElement = (TestElement) selection.getFirstElement();

			String testLabel = testElement.getTestName();
			String className = testElement.getClassName();
			if (testElement instanceof TestSuiteElement) {
				manager.add(new OpenTestAction(fTestRunnerPart, testLabel));
				manager.add(new Separator());
				if (testClassExists(className)
						&& !fTestRunnerPart.lastLaunchIsKeptAlive()) {
					manager.add(new RerunAction(
							JUnitMessages.RerunAction_label_run,
							fTestRunnerPart, testElement.getId(), className,
							null, ILaunchManager.RUN_MODE));
					manager.add(new RerunAction(
							JUnitMessages.RerunAction_label_debug,
							fTestRunnerPart, testElement.getId(), className,
							null, ILaunchManager.DEBUG_MODE));
				}
			} else {
				TestCaseElement testCaseElement = (TestCaseElement) testElement;
				String testMethodName = testCaseElement.getTestMethodName();
				manager.add(new OpenTestAction(fTestRunnerPart, className,
						testMethodName));
				manager.add(new Separator());
				if (fTestRunnerPart.lastLaunchIsKeptAlive()) {
					manager.add(new RerunAction(
							JUnitMessages.RerunAction_label_rerun,
							fTestRunnerPart, testElement.getId(), className,
							testMethodName, ILaunchManager.RUN_MODE));

				} else {
					manager.add(new RerunAction(
							JUnitMessages.RerunAction_label_run,
							fTestRunnerPart, testElement.getId(), className,
							testMethodName, ILaunchManager.RUN_MODE));
					manager.add(new RerunAction(
							JUnitMessages.RerunAction_label_debug,
							fTestRunnerPart, testElement.getId(), className,
							testMethodName, ILaunchManager.DEBUG_MODE));
				}
			}
			if (fLayoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL) {
				manager.add(new Separator());
				manager.add(new ExpandAllAction());
			}

		}
		if (fTestRunSession != null
				&& fTestRunSession.getFailureCount()
						+ fTestRunSession.getErrorCount() > 0) {
			if (fLayoutMode != TestRunnerViewPart.LAYOUT_HIERARCHICAL)
				manager.add(new Separator());
			manager.add(new CopyFailureListAction(fTestRunnerPart, fClipboard));
		}
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS
				+ "-end")); //$NON-NLS-1$
	}

	private boolean testClassExists(String className) {
		IJavaProject project = fTestRunnerPart.getLaunchedProject();
		if (project == null)
			return false;
		try {
			IType type = project.findType(className);
			return type != null;
		} catch (JavaModelException e) {
			// fall through
		}
		return false;
	}

	public Control getTestViewerControl() {
		return fViewerbook;
	}

	public synchronized void registerActiveSession(TestRunSession testRunSession) {
		fTestRunSession = testRunSession;
		registerAutoScrollTarget(null);
		registerViewersRefresh();
	}

	void handleDefaultSelected() {
		IStructuredSelection selection = (IStructuredSelection) fSelectionProvider
				.getSelection();
		if (selection.size() != 1)
			return;

		TestElement testElement = (TestElement) selection.getFirstElement();

		OpenTestAction action;
		if (testElement instanceof TestSuiteElement) {
			action = new OpenTestAction(fTestRunnerPart, testElement
					.getTestName());
		} else if (testElement instanceof TestCaseElement) {
			TestCaseElement testCase = (TestCaseElement) testElement;
			action = new OpenTestAction(fTestRunnerPart, testCase
					.getClassName(), testCase.getTestMethodName());
		} else {
			throw new IllegalStateException(String.valueOf(testElement));
		}

		if (action.isEnabled())
			action.run();
	}

	private void handleSelected() {
		IStructuredSelection selection = (IStructuredSelection) fSelectionProvider
				.getSelection();
		TestElement testElement = null;
		if (selection.size() == 1) {
			testElement = (TestElement) selection.getFirstElement();
		}
		fTestRunnerPart.handleTestSelected(testElement);
	}

	void disposeIcons() {
		fHierarchyIcon.dispose();
	}

	public synchronized void setShowFailuresOnly(boolean failuresOnly,
			int layoutMode) {
		/*
		 * Management of fTreeViewer and fTableViewer
		 * 
		 * - invisible viewer is updated on registerViewerUpdate unless its
		 * fNeedsRefresh is true - invisible viewer is not refreshed upfront -
		 * on layout change, new viewer is refreshed if necessary - filter only
		 * applies to "current" layout mode / viewer
		 */
		try {
			fViewerbook.setRedraw(false);

			IStructuredSelection selection = null;
			boolean switchLayout = layoutMode != fLayoutMode;
			if (switchLayout) {
				selection = (IStructuredSelection) fSelectionProvider
						.getSelection();
				if (layoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL) {
					if (fTreeNeedsRefresh) {
						clearUpdateAndExpansion();
					}
				} else {
					if (fTableNeedsRefresh) {
						clearUpdateAndExpansion();
					}
				}
				fLayoutMode = layoutMode;
				fViewerbook.showPage(getActiveViewer().getControl());
			}

			// avoid realizing all TableItems, especially in flat mode!
			StructuredViewer viewer = getActiveViewer();
			if (failuresOnly) {
				if (!getActiveViewerHasFilter()) {
					setActiveViewerHasFilter(true);
					if (getActiveViewerNeedsRefresh())
						viewer.setInput(null);
					viewer.addFilter(fFailuresOnlyFilter);
				}

			} else {
				if (getActiveViewerHasFilter()) {
					setActiveViewerHasFilter(false);
					if (getActiveViewerNeedsRefresh())
						viewer.setInput(null);
					viewer.removeFilter(fFailuresOnlyFilter);
				}
			}
			processChangesInUI();

			if (selection != null) {
				// workaround for
				// https://bugs.eclipse.org/bugs/show_bug.cgi?id=125708
				// (ITreeSelection not adapted if TreePaths changed):
				StructuredSelection flatSelection = new StructuredSelection(
						selection.toList());
				fSelectionProvider.setSelection(flatSelection, true);
			}

		} finally {
			fViewerbook.setRedraw(true);
		}
	}

	private boolean getActiveViewerHasFilter() {
		if (fLayoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL)
			return fTreeHasFilter;
		else
			return fTableHasFilter;
	}

	private void setActiveViewerHasFilter(boolean filter) {
		if (fLayoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL)
			fTreeHasFilter = filter;
		else
			fTableHasFilter = filter;
	}

	private StructuredViewer getActiveViewer() {
		if (fLayoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL)
			return fTreeViewer;
		else
			return fTableViewer;
	}

	private boolean getActiveViewerNeedsRefresh() {
		if (fLayoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL)
			return fTreeNeedsRefresh;
		else
			return fTableNeedsRefresh;
	}

	private void setActiveViewerRefreshed() {
		if (fLayoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL)
			fTreeNeedsRefresh = false;
		else
			fTableNeedsRefresh = false;
	}

	/**
	 * To be called periodically by the TestRunnerViewPart (in the UI thread).
	 */
	public void processChangesInUI() {
		TestRoot testRoot;
		if (fTestRunSession == null) {
			registerViewersRefresh();
			fTreeNeedsRefresh = false;
			fTableNeedsRefresh = false;
			fTreeViewer.setInput(null);
			fTableViewer.setInput(null);
			return;
		}

		testRoot = fTestRunSession.getTestRoot();

		StructuredViewer viewer = getActiveViewer();
		if (getActiveViewerNeedsRefresh()) {
			clearUpdateAndExpansion();
			setActiveViewerRefreshed();
			viewer.setInput(testRoot);

		} else {
			Object[] toUpdate;
			synchronized (this) {
				toUpdate = fNeedUpdate.toArray();
				fNeedUpdate.clear();
			}
			if (!fTreeNeedsRefresh && toUpdate.length > 0) {
				if (fTreeHasFilter)
					for (int i = 0; i < toUpdate.length; i++)
						updateElementInTree((TestElement) toUpdate[i]);
				else {
					HashSet toUpdateWithParents = new HashSet();
					toUpdateWithParents.addAll(Arrays.asList(toUpdate));
					for (int i = 0; i < toUpdate.length; i++) {
						TestElement parent = ((TestElement) toUpdate[i])
								.getParent();
						while (parent != null) {
							toUpdateWithParents.add(parent);
							parent = parent.getParent();
						}
					}
					fTreeViewer.update(toUpdateWithParents.toArray(), null);
				}
			}
			if (!fTableNeedsRefresh && toUpdate.length > 0) {
				if (fTableHasFilter)
					for (int i = 0; i < toUpdate.length; i++)
						updateElementInTable((TestElement) toUpdate[i]);
				else
					fTableViewer.update(toUpdate, null);
			}
		}
		autoScrollInUI();
	}

	private void updateElementInTree(final TestElement testElement) {
		if (isShown(testElement)) {
			updateShownElementInTree(testElement);
		} else {
			TestElement current = testElement;
			do {
				if (fTreeViewer.testFindItem(current) != null)
					fTreeViewer.remove(current);
				current = current.getParent();
			} while (!(current instanceof TestRoot) && !isShown(current));

			while (current != null && !(current instanceof TestRoot)) {
				fTreeViewer.update(current, null);
				current = current.getParent();
			}
		}
	}

	private void updateShownElementInTree(TestElement testElement) {
		if (testElement == null || testElement instanceof TestRoot) // paranoia
																	// null
																	// check
			return;

		TestSuiteElement parent = testElement.getParent();
		updateShownElementInTree(parent); // make sure parent is shown and
											// up-to-date

		if (fTreeViewer.testFindItem(testElement) == null) {
			fTreeViewer.add(parent, testElement); // if not yet in tree: add
		} else {
			fTreeViewer.update(testElement, null); // if in tree: update
		}
	}

	private void updateElementInTable(TestElement element) {
		if (isShown(element)) {
			if (fTableViewer.testFindItem(element) == null) {
				TestElement previous = getNextFailure(element, false);
				int insertionIndex = -1;
				if (previous != null) {
					TableItem item = (TableItem) fTableViewer
							.testFindItem(previous);
					if (item != null)
						insertionIndex = fTableViewer.getTable().indexOf(item);
				}
				fTableViewer.insert(element, insertionIndex);
			} else {
				fTableViewer.update(element, null);
			}
		} else {
			fTableViewer.remove(element);
		}
	}

	private boolean isShown(TestElement current) {
		return fFailuresOnlyFilter.select(current);
	}

	private void autoScrollInUI() {
		if (!fTestRunnerPart.isAutoScroll()) {
			clearAutoExpand();
			fAutoClose.clear();
			return;
		}

		if (fLayoutMode == TestRunnerViewPart.LAYOUT_FLAT) {
			if (fAutoScrollTarget != null)
				fTableViewer.reveal(fAutoScrollTarget);
			return;
		}

		synchronized (this) {
			for (Iterator iter = fAutoExpand.iterator(); iter.hasNext();) {
				TestSuiteElement suite = (TestSuiteElement) iter.next();
				fTreeViewer.setExpandedState(suite, true);
			}
			clearAutoExpand();
		}

		TestCaseElement current = fAutoScrollTarget;
		fAutoScrollTarget = null;

		TestSuiteElement parent = current == null ? null
				: (TestSuiteElement) fTreeContentProvider.getParent(current);
		if (fAutoClose.isEmpty() || !fAutoClose.getLast().equals(parent)) {
			// we're in a new branch, so let's close old OK branches:
			for (ListIterator iter = fAutoClose.listIterator(fAutoClose.size()); iter
					.hasPrevious();) {
				TestSuiteElement previousAutoOpened = (TestSuiteElement) iter
						.previous();
				if (previousAutoOpened.equals(parent))
					break;

				if (previousAutoOpened.getStatus() == TestElement.Status.OK) {
					// auto-opened the element, and all children are OK -> auto
					// close
					iter.remove();
					fTreeViewer.collapseToLevel(previousAutoOpened,
							AbstractTreeViewer.ALL_LEVELS);
				}
			}

			while (parent != null
					&& !fTestRunSession.getTestRoot().equals(parent)
					&& fTreeViewer.getExpandedState(parent) == false) {
				fAutoClose.add(parent); // add to auto-opened elements -> close
										// later if STATUS_OK
				parent = (TestSuiteElement) fTreeContentProvider
						.getParent(parent);
			}
		}
		if (current != null)
			fTreeViewer.reveal(current);
	}

	public void selectFirstFailure() {
		TestCaseElement firstFailure = getNextChildFailure(fTestRunSession
				.getTestRoot(), true);
		if (firstFailure != null)
			getActiveViewer().setSelection(
					new StructuredSelection(firstFailure), true);
	}

	public void selectFailure(boolean showNext) {
		IStructuredSelection selection = (IStructuredSelection) getActiveViewer()
				.getSelection();
		TestElement selected = (TestElement) selection.getFirstElement();
		TestElement next;

		if (selected == null) {
			next = getNextChildFailure(fTestRunSession.getTestRoot(), showNext);
		} else {
			next = getNextFailure(selected, showNext);
		}

		if (next != null)
			getActiveViewer().setSelection(new StructuredSelection(next), true);
	}

	private TestElement getNextFailure(TestElement selected, boolean showNext) {
		if (selected instanceof TestSuiteElement) {
			TestElement nextChild = getNextChildFailure(
					(TestSuiteElement) selected, showNext);
			if (nextChild != null)
				return nextChild;
		}
		return getNextFailureSibling(selected, showNext);
	}

	private TestCaseElement getNextFailureSibling(TestElement current,
			boolean showNext) {
		TestSuiteElement parent = current.getParent();
		if (parent == null)
			return null;

		List siblings = Arrays.asList(parent.getChildren());
		if (!showNext)
			siblings = new ReverseList(siblings);

		int nextIndex = siblings.indexOf(current) + 1;
		for (int i = nextIndex; i < siblings.size(); i++) {
			TestElement sibling = (TestElement) siblings.get(i);
			if (sibling.getStatus().isErrorOrFailure()) {
				if (sibling instanceof TestCaseElement) {
					return (TestCaseElement) sibling;
				} else {
					return getNextChildFailure((TestSuiteElement) sibling,
							showNext);
				}
			}
		}
		return getNextFailureSibling(parent, showNext);
	}

	private TestCaseElement getNextChildFailure(TestSuiteElement root,
			boolean showNext) {
		List children = Arrays.asList(root.getChildren());
		if (!showNext)
			children = new ReverseList(children);
		for (int i = 0; i < children.size(); i++) {
			TestElement child = (TestElement) children.get(i);
			if (child.getStatus().isErrorOrFailure()) {
				if (child instanceof TestCaseElement) {
					return (TestCaseElement) child;
				} else {
					return getNextChildFailure((TestSuiteElement) child,
							showNext);
				}
			}
		}
		return null;
	}

	public synchronized void registerViewersRefresh() {
		fTreeNeedsRefresh = true;
		fTableNeedsRefresh = true;
		clearUpdateAndExpansion();
	}

	private void clearUpdateAndExpansion() {
		fNeedUpdate = new LinkedHashSet();
		fAutoClose = new LinkedList();
		fAutoExpand = new HashSet();
	}

	public synchronized void registerTestAdded(TestElement testElement) {
		// TODO: performance: would only need to refresh parent of added element
		fTreeNeedsRefresh = true;
		fTableNeedsRefresh = true;
	}

	public synchronized void registerViewerUpdate(final TestElement testElement) {
		fNeedUpdate.add(testElement);
	}

	private synchronized void clearAutoExpand() {
		fAutoExpand.clear();
	}

	public void registerAutoScrollTarget(TestCaseElement testCaseElement) {
		fAutoScrollTarget = testCaseElement;
	}

	public synchronized void registerFailedForAutoScroll(TestElement testElement) {
		Object parent = fTreeContentProvider.getParent(testElement);
		if (parent != null)
			fAutoExpand.add(parent);
	}

	public void expandFirstLevel() {
		fTreeViewer.expandToLevel(2);
	}

}
