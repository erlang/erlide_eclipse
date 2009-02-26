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

import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
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
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.part.PageBook;
import org.erlide.core.erlang.IErlProject;
import org.erlide.gunit.internal.model.TestCaseElement;
import org.erlide.gunit.internal.model.TestElement;
import org.erlide.gunit.internal.model.TestRoot;
import org.erlide.gunit.internal.model.TestRunSession;
import org.erlide.gunit.internal.model.TestSuiteElement;
import org.erlide.gunit.internal.model.TestElement.Status;
import org.erlide.gunit.model.ITestElement;
import org.erlide.gunit.model.ITestSuiteElement;

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
			if (status.isErrorOrFailure()) {
				return true;
			} else {
				return !TestViewer.this.fTestRunSession.isRunning()
						&& status == Status.RUNNING; // rerunning
			}
		}
	}

	private static class ReverseList<T> extends AbstractList<T> {
		private final List<T> fList;

		public ReverseList(List<T> list) {
			this.fList = list;
		}

		@Override
		public T get(int index) {
			return this.fList.get(this.fList.size() - index - 1);
		}

		@Override
		public int size() {
			return this.fList.size();
		}
	}

	private class ExpandAllAction extends Action {
		public ExpandAllAction() {
			setText(GUnitMessages.ExpandAllAction_text);
			setToolTipText(GUnitMessages.ExpandAllAction_tooltip);
		}

		@Override
		public void run() {
			TestViewer.this.fTreeViewer.expandAll();
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

	private ISelectionProvider fSelectionProvider;

	private final Image fHierarchyIcon;

	private int fLayoutMode;

	private boolean fTreeHasFilter;

	private boolean fTableHasFilter;

	private TestRunSession fTestRunSession;

	private boolean fTreeNeedsRefresh;

	private boolean fTableNeedsRefresh;

	private HashSet<ITestElement> fNeedUpdate;

	private TestCaseElement fAutoScrollTarget;

	private LinkedList<ITestSuiteElement> fAutoClose;

	private HashSet/* <Test> */fAutoExpand;

	public TestViewer(Composite parent, Clipboard clipboard,
			TestRunnerViewPart runner) {
		this.fTestRunnerPart = runner;
		this.fClipboard = clipboard;

		this.fHierarchyIcon = TestRunnerViewPart
				.createImage("obj16/testhier.gif"); //$NON-NLS-1$
		parent.addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent e) {
				disposeIcons();
			}
		});

		this.fLayoutMode = TestRunnerViewPart.LAYOUT_HIERARCHICAL;

		createTestViewers(parent);

		registerViewersRefresh();

		initContextMenu();
	}

	private void createTestViewers(Composite parent) {
		this.fViewerbook = new PageBook(parent, SWT.NULL);

		this.fTreeViewer = new TreeViewer(this.fViewerbook, SWT.V_SCROLL
				| SWT.SINGLE);
		this.fTreeViewer.setUseHashlookup(true);
		this.fTreeContentProvider = new TestSessionTreeContentProvider();
		this.fTreeViewer.setContentProvider(this.fTreeContentProvider);
		this.fTreeLabelProvider = new TestSessionLabelProvider(
				this.fTestRunnerPart, TestRunnerViewPart.LAYOUT_HIERARCHICAL);
		this.fTreeViewer.setLabelProvider(this.fTreeLabelProvider);

		this.fTableViewer = new TableViewer(this.fViewerbook, SWT.V_SCROLL
				| SWT.H_SCROLL | SWT.SINGLE);
		this.fTableViewer.setUseHashlookup(true);
		this.fTableContentProvider = new TestSessionTableContentProvider();
		this.fTableViewer.setContentProvider(this.fTableContentProvider);
		this.fTableLabelProvider = new TestSessionLabelProvider(
				this.fTestRunnerPart, TestRunnerViewPart.LAYOUT_FLAT);
		this.fTableViewer.setLabelProvider(this.fTableLabelProvider);

		// fSelectionProvider = new SelectionProviderMediator(
		// new StructuredViewer[] { fTreeViewer, fTableViewer },
		// fTreeViewer);
		// fSelectionProvider
		// .addSelectionChangedListener(new TestSelectionListener());
		TestOpenListener testOpenListener = new TestOpenListener();
		this.fTreeViewer.getTree().addSelectionListener(testOpenListener);
		this.fTableViewer.getTable().addSelectionListener(testOpenListener);

		this.fViewerbook.showPage(this.fTreeViewer.getTree());
	}

	private void initContextMenu() {
		MenuManager menuMgr = new MenuManager("#PopupMenu"); //$NON-NLS-1$
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				handleMenuAboutToShow(manager);
			}
		});
		this.fTestRunnerPart.getSite().registerContextMenu(menuMgr,
				this.fSelectionProvider);
		Menu menu = menuMgr.createContextMenu(this.fViewerbook);
		this.fTreeViewer.getTree().setMenu(menu);
		this.fTableViewer.getTable().setMenu(menu);
	}

	void handleMenuAboutToShow(IMenuManager manager) {
		IStructuredSelection selection = (IStructuredSelection) this.fSelectionProvider
				.getSelection();
		if (!selection.isEmpty()) {
			TestElement testElement = (TestElement) selection.getFirstElement();

			String testLabel = testElement.getTestName();
			String className = testElement.getClassName();
			if (testElement instanceof TestSuiteElement) {
				manager
						.add(new OpenTestAction(this.fTestRunnerPart, testLabel));
				manager.add(new Separator());
				if (testClassExists(className)
						&& !this.fTestRunnerPart.lastLaunchIsKeptAlive()) {
					manager.add(new RerunAction(
							GUnitMessages.RerunAction_label_run,
							this.fTestRunnerPart, testElement.getId(),
							className, null, ILaunchManager.RUN_MODE));
					manager.add(new RerunAction(
							GUnitMessages.RerunAction_label_debug,
							this.fTestRunnerPart, testElement.getId(),
							className, null, ILaunchManager.DEBUG_MODE));
				}
			} else {
				TestCaseElement testCaseElement = (TestCaseElement) testElement;
				String testMethodName = testCaseElement.getTestMethodName();
				manager.add(new OpenTestAction(this.fTestRunnerPart, className,
						testMethodName));
				manager.add(new Separator());
				if (this.fTestRunnerPart.lastLaunchIsKeptAlive()) {
					manager
							.add(new RerunAction(
									GUnitMessages.RerunAction_label_rerun,
									this.fTestRunnerPart, testElement.getId(),
									className, testMethodName,
									ILaunchManager.RUN_MODE));

				} else {
					manager
							.add(new RerunAction(
									GUnitMessages.RerunAction_label_run,
									this.fTestRunnerPart, testElement.getId(),
									className, testMethodName,
									ILaunchManager.RUN_MODE));
					manager.add(new RerunAction(
							GUnitMessages.RerunAction_label_debug,
							this.fTestRunnerPart, testElement.getId(),
							className, testMethodName,
							ILaunchManager.DEBUG_MODE));
				}
			}
			if (this.fLayoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL) {
				manager.add(new Separator());
				manager.add(new ExpandAllAction());
			}

		}
		if (this.fTestRunSession != null
				&& this.fTestRunSession.getFailureCount()
						+ this.fTestRunSession.getErrorCount() > 0) {
			if (this.fLayoutMode != TestRunnerViewPart.LAYOUT_HIERARCHICAL) {
				manager.add(new Separator());
			}
			manager.add(new CopyFailureListAction(this.fTestRunnerPart,
					this.fClipboard));
		}
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS
				+ "-end")); //$NON-NLS-1$
	}

	private boolean testClassExists(String className) {
		IErlProject project = this.fTestRunnerPart.getLaunchedProject();
		if (project == null) {
			return false;
		}
		// try {
		// IErlModule type = = project.findType(className);
		// return type != null;
		// } catch (ErlModelException e) {
		// // fall through
		// }
		return false;
	}

	public Control getTestViewerControl() {
		return this.fViewerbook;
	}

	public synchronized void registerActiveSession(TestRunSession testRunSession) {
		this.fTestRunSession = testRunSession;
		registerAutoScrollTarget(null);
		registerViewersRefresh();
	}

	void handleDefaultSelected() {
		IStructuredSelection selection = (IStructuredSelection) this.fSelectionProvider
				.getSelection();
		if (selection.size() != 1) {
			return;
		}

		TestElement testElement = (TestElement) selection.getFirstElement();

		OpenTestAction action;
		if (testElement instanceof TestSuiteElement) {
			action = new OpenTestAction(this.fTestRunnerPart, testElement
					.getTestName());
		} else if (testElement instanceof TestCaseElement) {
			TestCaseElement testCase = (TestCaseElement) testElement;
			action = new OpenTestAction(this.fTestRunnerPart, testCase
					.getClassName(), testCase.getTestMethodName());
		} else {
			throw new IllegalStateException(String.valueOf(testElement));
		}

		if (action.isEnabled()) {
			action.run();
		}
	}

	private void handleSelected() {
		IStructuredSelection selection = (IStructuredSelection) this.fSelectionProvider
				.getSelection();
		TestElement testElement = null;
		if (selection.size() == 1) {
			testElement = (TestElement) selection.getFirstElement();
		}
		this.fTestRunnerPart.handleTestSelected(testElement);
	}

	void disposeIcons() {
		this.fHierarchyIcon.dispose();
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
			this.fViewerbook.setRedraw(false);

			IStructuredSelection selection = null;
			boolean switchLayout = layoutMode != this.fLayoutMode;
			if (switchLayout) {
				selection = (IStructuredSelection) this.fSelectionProvider
						.getSelection();
				if (layoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL) {
					if (this.fTreeNeedsRefresh) {
						clearUpdateAndExpansion();
					}
				} else {
					if (this.fTableNeedsRefresh) {
						clearUpdateAndExpansion();
					}
				}
				this.fLayoutMode = layoutMode;
				this.fViewerbook.showPage(getActiveViewer().getControl());
			}

			// avoid realizing all TableItems, especially in flat mode!
			StructuredViewer viewer = getActiveViewer();
			if (failuresOnly) {
				if (!getActiveViewerHasFilter()) {
					setActiveViewerHasFilter(true);
					if (getActiveViewerNeedsRefresh()) {
						viewer.setInput(null);
					}
					viewer.addFilter(this.fFailuresOnlyFilter);
				}

			} else {
				if (getActiveViewerHasFilter()) {
					setActiveViewerHasFilter(false);
					if (getActiveViewerNeedsRefresh()) {
						viewer.setInput(null);
					}
					viewer.removeFilter(this.fFailuresOnlyFilter);
				}
			}
			processChangesInUI();

			if (selection != null) {
				// workaround for
				// https://bugs.eclipse.org/bugs/show_bug.cgi?id=125708
				// (ITreeSelection not adapted if TreePaths changed):
				StructuredSelection flatSelection = new StructuredSelection(
						selection.toList());
				this.fSelectionProvider.setSelection(flatSelection);
			}

		} finally {
			this.fViewerbook.setRedraw(true);
		}
	}

	private boolean getActiveViewerHasFilter() {
		if (this.fLayoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL) {
			return this.fTreeHasFilter;
		} else {
			return this.fTableHasFilter;
		}
	}

	private void setActiveViewerHasFilter(boolean filter) {
		if (this.fLayoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL) {
			this.fTreeHasFilter = filter;
		} else {
			this.fTableHasFilter = filter;
		}
	}

	private StructuredViewer getActiveViewer() {
		if (this.fLayoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL) {
			return this.fTreeViewer;
		} else {
			return this.fTableViewer;
		}
	}

	private boolean getActiveViewerNeedsRefresh() {
		if (this.fLayoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL) {
			return this.fTreeNeedsRefresh;
		} else {
			return this.fTableNeedsRefresh;
		}
	}

	private void setActiveViewerRefreshed() {
		if (this.fLayoutMode == TestRunnerViewPart.LAYOUT_HIERARCHICAL) {
			this.fTreeNeedsRefresh = false;
		} else {
			this.fTableNeedsRefresh = false;
		}
	}

	/**
	 * To be called periodically by the TestRunnerViewPart (in the UI thread).
	 */
	public void processChangesInUI() {
		TestRoot testRoot;
		if (this.fTestRunSession == null) {
			registerViewersRefresh();
			this.fTreeNeedsRefresh = false;
			this.fTableNeedsRefresh = false;
			this.fTreeViewer.setInput(null);
			this.fTableViewer.setInput(null);
			return;
		}

		testRoot = this.fTestRunSession.getTestRoot();

		StructuredViewer viewer = getActiveViewer();
		if (getActiveViewerNeedsRefresh()) {
			clearUpdateAndExpansion();
			setActiveViewerRefreshed();
			viewer.setInput(testRoot);

		} else {
			Object[] toUpdate;
			synchronized (this) {
				toUpdate = this.fNeedUpdate.toArray();
				this.fNeedUpdate.clear();
			}
			if (!this.fTreeNeedsRefresh && toUpdate.length > 0) {
				if (this.fTreeHasFilter) {
					for (int i = 0; i < toUpdate.length; i++) {
						updateElementInTree((TestElement) toUpdate[i]);
					}
				} else {
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
					this.fTreeViewer
							.update(toUpdateWithParents.toArray(), null);
				}
			}
			if (!this.fTableNeedsRefresh && toUpdate.length > 0) {
				if (this.fTableHasFilter) {
					for (int i = 0; i < toUpdate.length; i++) {
						updateElementInTable((TestElement) toUpdate[i]);
					}
				} else {
					this.fTableViewer.update(toUpdate, null);
				}
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
				if (this.fTreeViewer.testFindItem(current) != null) {
					this.fTreeViewer.remove(current);
				}
				current = current.getParent();
			} while (!(current instanceof TestRoot) && !isShown(current));

			while (current != null && !(current instanceof TestRoot)) {
				this.fTreeViewer.update(current, null);
				current = current.getParent();
			}
		}
	}

	private void updateShownElementInTree(TestElement testElement) {
		if (testElement == null || testElement instanceof TestRoot) {
			// null
			// check
			return;
		}

		TestSuiteElement parent = testElement.getParent();
		updateShownElementInTree(parent); // make sure parent is shown and
		// up-to-date

		if (this.fTreeViewer.testFindItem(testElement) == null) {
			this.fTreeViewer.add(parent, testElement); // if not yet in tree:
														// add
		} else {
			this.fTreeViewer.update(testElement, null); // if in tree: update
		}
	}

	private void updateElementInTable(TestElement element) {
		if (isShown(element)) {
			if (this.fTableViewer.testFindItem(element) == null) {
				TestElement previous = getNextFailure(element, false);
				int insertionIndex = -1;
				if (previous != null) {
					TableItem item = (TableItem) this.fTableViewer
							.testFindItem(previous);
					if (item != null) {
						insertionIndex = this.fTableViewer.getTable().indexOf(
								item);
					}
				}
				this.fTableViewer.insert(element, insertionIndex);
			} else {
				this.fTableViewer.update(element, null);
			}
		} else {
			this.fTableViewer.remove(element);
		}
	}

	private boolean isShown(TestElement current) {
		return this.fFailuresOnlyFilter.select(current);
	}

	private void autoScrollInUI() {
		if (!this.fTestRunnerPart.isAutoScroll()) {
			clearAutoExpand();
			this.fAutoClose.clear();
			return;
		}

		if (this.fLayoutMode == TestRunnerViewPart.LAYOUT_FLAT) {
			if (this.fAutoScrollTarget != null) {
				this.fTableViewer.reveal(this.fAutoScrollTarget);
			}
			return;
		}

		synchronized (this) {
			for (Iterator iter = this.fAutoExpand.iterator(); iter.hasNext();) {
				TestSuiteElement suite = (TestSuiteElement) iter.next();
				this.fTreeViewer.setExpandedState(suite, true);
			}
			clearAutoExpand();
		}

		TestCaseElement current = this.fAutoScrollTarget;
		this.fAutoScrollTarget = null;

		TestSuiteElement parent = current == null ? null
				: (TestSuiteElement) this.fTreeContentProvider
						.getParent(current);
		if (this.fAutoClose.isEmpty()
				|| !this.fAutoClose.getLast().equals(parent)) {
			// we're in a new branch, so let's close old OK branches:
			for (ListIterator iter = this.fAutoClose
					.listIterator(this.fAutoClose.size()); iter.hasPrevious();) {
				TestSuiteElement previousAutoOpened = (TestSuiteElement) iter
						.previous();
				if (previousAutoOpened.equals(parent)) {
					break;
				}

				if (previousAutoOpened.getStatus() == TestElement.Status.OK) {
					// auto-opened the element, and all children are OK -> auto
					// close
					iter.remove();
					this.fTreeViewer.collapseToLevel(previousAutoOpened,
							AbstractTreeViewer.ALL_LEVELS);
				}
			}

			while (parent != null
					&& !this.fTestRunSession.getTestRoot().equals(parent)
					&& this.fTreeViewer.getExpandedState(parent) == false) {
				this.fAutoClose.add(parent); // add to auto-opened elements ->
												// close
				// later if STATUS_OK
				parent = (TestSuiteElement) this.fTreeContentProvider
						.getParent(parent);
			}
		}
		if (current != null) {
			this.fTreeViewer.reveal(current);
		}
	}

	public void selectFirstFailure() {
		TestCaseElement firstFailure = getNextChildFailure(this.fTestRunSession
				.getTestRoot(), true);
		if (firstFailure != null) {
			getActiveViewer().setSelection(
					new StructuredSelection(firstFailure), true);
		}
	}

	public void selectFailure(boolean showNext) {
		IStructuredSelection selection = (IStructuredSelection) getActiveViewer()
				.getSelection();
		TestElement selected = (TestElement) selection.getFirstElement();
		TestElement next;

		if (selected == null) {
			next = getNextChildFailure(this.fTestRunSession.getTestRoot(),
					showNext);
		} else {
			next = getNextFailure(selected, showNext);
		}

		if (next != null) {
			getActiveViewer().setSelection(new StructuredSelection(next), true);
		}
	}

	private TestElement getNextFailure(TestElement selected, boolean showNext) {
		if (selected instanceof TestSuiteElement) {
			TestElement nextChild = getNextChildFailure(
					(TestSuiteElement) selected, showNext);
			if (nextChild != null) {
				return nextChild;
			}
		}
		return getNextFailureSibling(selected, showNext);
	}

	private TestCaseElement getNextFailureSibling(TestElement current,
			boolean showNext) {
		TestSuiteElement parent = current.getParent();
		if (parent == null) {
			return null;
		}

		List siblings = Arrays.asList(parent.getChildren());
		if (!showNext) {
			siblings = new ReverseList(siblings);
		}

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
		if (!showNext) {
			children = new ReverseList(children);
		}
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
		this.fTreeNeedsRefresh = true;
		this.fTableNeedsRefresh = true;
		clearUpdateAndExpansion();
	}

	private void clearUpdateAndExpansion() {
		this.fNeedUpdate = new LinkedHashSet();
		this.fAutoClose = new LinkedList();
		this.fAutoExpand = new HashSet();
	}

	public synchronized void registerTestAdded(TestElement testElement) {
		// TODO: performance: would only need to refresh parent of added element
		this.fTreeNeedsRefresh = true;
		this.fTableNeedsRefresh = true;
	}

	public synchronized void registerViewerUpdate(final TestElement testElement) {
		this.fNeedUpdate.add(testElement);
	}

	private synchronized void clearAutoExpand() {
		this.fAutoExpand.clear();
	}

	public void registerAutoScrollTarget(TestCaseElement testCaseElement) {
		this.fAutoScrollTarget = testCaseElement;
	}

	public synchronized void registerFailedForAutoScroll(TestElement testElement) {
		Object parent = this.fTreeContentProvider.getParent(testElement);
		if (parent != null) {
			this.fAutoExpand.add(parent);
		}
	}

	public void expandFirstLevel() {
		this.fTreeViewer.expandToLevel(2);
	}

}
