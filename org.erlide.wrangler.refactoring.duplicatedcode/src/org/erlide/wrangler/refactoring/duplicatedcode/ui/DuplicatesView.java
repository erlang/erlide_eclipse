package org.erlide.wrangler.refactoring.duplicatedcode.ui;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;

public class DuplicatesView extends ViewPart {
	// private final class HighlightAction extends Action {
	// public void run() {
	// ISelection selection = viewer.getSelection();
	// Object obj = ((IStructuredSelection) selection)
	// .getFirstElement();
	// showMessage("Double-click detected on " + obj.toString());
	// }
	// }

	private TreeViewer viewer;
	// private DrillDownAdapter drillDownAdapter;
	// private Action action1;
	// private Action action2;
	Action doubleClickAction;

	/*
	 * The content provider class is responsible for providing objects to the
	 * view. It can wrap existing objects in adapters or simply return objects
	 * as-is. These objects may be sensitive to the current input of the view,
	 * or ignore it and always show the same content (like Task List, for
	 * example).
	 */

	/**
	 * The constructor.
	 */
	public DuplicatesView() {
	}

	public void refresh() {
		viewer.refresh();
	}

	/**
	 * This is a callback that will allow us to create the viewer and initialize
	 * it.
	 */
	@Override
	public void createPartControl(Composite parent) {
		viewer = new TreeViewer(parent, SWT.SINGLE | SWT.H_SCROLL
				| SWT.V_SCROLL);
		// drillDownAdapter = new DrillDownAdapter(viewer);
		viewer.setContentProvider(new DuplicatesViewContentProvider(this));
		viewer.setLabelProvider(new DuplicatesViewLabelProvider());
		// viewer.setSorter(new NameSorter());
		viewer.setInput(getViewSite());
		// makeActions();
		// hookContextMenu();
		hookDoubleClickAction();
		// contributeToActionBars();

	}

	// private void hookContextMenu() {
	// MenuManager menuMgr = new MenuManager("#PopupMenu");
	// menuMgr.setRemoveAllWhenShown(true);
	// menuMgr.addMenuListener(new IMenuListener() {
	// public void menuAboutToShow(IMenuManager manager) {
	// DuplicatedCodeView.this.fillContextMenu(manager);
	// }
	// });
	// Menu menu = menuMgr.createContextMenu(viewer.getControl());
	// viewer.getControl().setMenu(menu);
	// getSite().registerContextMenu(menuMgr, viewer);
	// }
	//
	// private void contributeToActionBars() {
	// IActionBars bars = getViewSite().getActionBars();
	// fillLocalPullDown(bars.getMenuManager());
	// fillLocalToolBar(bars.getToolBarManager());
	// }

	// private void fillLocalPullDown(IMenuManager manager) {
	// manager.add(action1);
	// // manager.add(new Separator());
	// // manager.add(action2);
	// }
	//
	// private void fillContextMenu(IMenuManager manager) {
	// manager.add(action1);
	// // manager.add(action2);
	// manager.add(new Separator());
	// drillDownAdapter.addNavigationActions(manager);
	// // Other plug-ins can contribute there actions here
	// manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	// }
	//
	// private void fillLocalToolBar(IToolBarManager manager) {
	// // manager.add(action1);
	// // manager.add(action2);
	// // manager.add(new Separator());
	// drillDownAdapter.addNavigationActions(manager);
	// }

	// private void makeActions() {
	// action1 = new Action() {
	// public void run() {
	// showMessage("Sample refactoring executed");
	// }
	// };
	// action1.setText("sample refactoring");
	// action1.setToolTipText("sample refactoring");
	// action1.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
	// .getImageDescriptor(ISharedImages.IMG_OBJS_WARN_TSK));
	//
	// action2 = new Action() {
	// public void run() {
	// showMessage("Action 2 executed");
	// }
	// };
	// action2.setText("Action 2");
	// action2.setToolTipText("Action 2 tooltip");
	// action2.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
	// .getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
	// doubleClickAction = new HighlightAction();
	// }

	private void hookDoubleClickAction() {
		viewer.addDoubleClickListener(new DoubleClickListener());
	}

	// private void showMessage(String message) {
	// MessageDialog.openInformation(viewer.getControl().getShell(),
	// "Duplicated codes", message);
	// }

	/**
	 * Passing the focus request to the viewer's control.
	 */
	@Override
	public void setFocus() {
		viewer.getControl().setFocus();
	}
}