/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.views.eval;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.part.ViewPart;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.BackendEvalResult;
import org.erlide.ui.ErlideUIConstants;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.prefs.PreferenceConstants;
import org.erlide.ui.views.SourceViewerInformationControl;

import com.ericsson.otp.erlang.OtpErlangString;

import erlang.ErlideBackend;

/**
 * @author Vlad Dumitrescu
 */
public class LiveExpressionsView extends ViewPart implements
		IResourceChangeListener {

	public static final String ID = "org.erlide.ui.views.eval.LiveExpressionsView";

	private Label label;
	private List<LiveExpr> exprs;
	TableViewer viewer;
	private Action refreshAction;
	private Action fAddAction;
	Action fRemoveAction;
	Backend fBackend;

	private static class LiveExpr {
		String fExpr;

		public LiveExpr(String s) {
			fExpr = s;
		}

		@Override
		public String toString() {
			return fExpr;
		}
	}

	/*
	 * The content provider class is responsible for providing objects to the
	 * view. It can wrap existing objects in adapters or simply return objects
	 * as-is. These objects may be sensitive to the current input of the view,
	 * or ignore it and always show the same content (like Task List, for
	 * example).
	 */
	static class ViewContentProvider implements IStructuredContentProvider {

		private List<?> exprlist;

		public ViewContentProvider() {
			super();
		}

		public void inputChanged(Viewer v, Object oldInput, Object newInput) {
			if (newInput instanceof List<?>) {
				exprlist = (List<?>) newInput;
			}
		}

		public void dispose() {
			exprlist = null;
		}

		public Object[] getElements(Object parent) {
			return exprlist.toArray();
		}

	}

	class ViewLabelProvider extends LabelProvider implements
			ITableLabelProvider {

		public String getColumnText(Object obj, int index) {
			final LiveExpr e = (LiveExpr) obj;
			if (index == 0) {
				return e.fExpr;
			}
			final BackendEvalResult r = ErlideBackend.eval(fBackend, e.fExpr
					+ ".", null);
			if (r.isOk()) {
				return r.getValue().toString();
			}
			return "ERR: " + r.getErrorReason().toString();
		}

		public Image getColumnImage(Object obj, int index) {
			// if (index == 0)
			// return getImage(obj);
			// else
			return null;
		}

		@Override
		public Image getImage(Object obj) {
			return PlatformUI.getWorkbench().getSharedImages().getImage(
					ISharedImages.IMG_OBJ_ELEMENT);
		}
	}

	static class NameSorter extends ViewerSorter {
	}

	/**
	 * The constructor.
	 */
	public LiveExpressionsView() {
		ResourcesPlugin.getWorkspace().addResourceChangeListener(this,
				IResourceChangeEvent.POST_BUILD);

		// TODO make the backend configurable (as for console)
		fBackend = ErlangCore.getBackendManager().getIdeBackend();
	}

	/**
	 * This is a callback that will allow us to create the viewer and initialize
	 * it.
	 */
	@Override
	public void createPartControl(Composite parent) {
		label = new Label(parent, SWT.NULL);
		viewer = new TableViewer(parent, SWT.SINGLE | SWT.V_SCROLL
				| SWT.FULL_SELECTION);
		final Table t = (Table) viewer.getControl();

		final GridData labelLData = new GridData();
		labelLData.verticalAlignment = GridData.BEGINNING;
		labelLData.horizontalAlignment = GridData.FILL;
		labelLData.widthHint = 300;
		labelLData.heightHint = 14;
		labelLData.horizontalIndent = 0;
		labelLData.horizontalSpan = 1;
		labelLData.verticalSpan = 1;
		labelLData.grabExcessHorizontalSpace = true;
		labelLData.grabExcessVerticalSpace = false;
		label.setLayoutData(labelLData);
		label.setSize(new org.eclipse.swt.graphics.Point(319, 14));

		final GridData viewerLData = new GridData();
		viewerLData.verticalAlignment = GridData.FILL;
		viewerLData.horizontalAlignment = GridData.FILL;
		viewerLData.widthHint = 600;
		viewerLData.heightHint = 150;
		viewerLData.horizontalIndent = 0;
		viewerLData.horizontalSpan = 1;
		viewerLData.verticalSpan = 1;
		viewerLData.grabExcessHorizontalSpace = true;
		viewerLData.grabExcessVerticalSpace = true;
		t.setLayoutData(viewerLData);
		t.setSize(new org.eclipse.swt.graphics.Point(600, 200));

		final GridLayout thisLayout = new GridLayout(1, true);
		parent.setLayout(thisLayout);
		thisLayout.marginWidth = 5;
		thisLayout.marginHeight = 5;
		thisLayout.numColumns = 1;
		thisLayout.makeColumnsEqualWidth = false;
		thisLayout.horizontalSpacing = 0;
		thisLayout.verticalSpacing = 1;

		t.setLinesVisible(true);
		t.setHeaderVisible(true);
		// t.setFont();

		final TableColumn colExpr = new TableColumn(t, SWT.LEAD, 0);
		colExpr.setText("Expression");
		colExpr.setWidth(150);

		final TableColumn colValue = new TableColumn(t, SWT.LEAD, 1);
		colValue.setText("Value");
		colValue.setWidth(t.getSize().x - 50);

		viewer.setColumnProperties(new String[] { "expr", "val" });

		viewer.setContentProvider(new ViewContentProvider());
		viewer.setLabelProvider(new ViewLabelProvider());
		// viewer.setSorter(new NameSorter());

		if (!restoreState()) {
			/* Fill LiveExpressions for first time */
			exprs = new ArrayList<LiveExpr>(10);
			addExpr(new LiveExpr("erlang:now()"));
		}
		viewer.setInput(exprs);

		final TextCellEditor e = new TextCellEditor(t);
		viewer.setCellEditors(new CellEditor[] { e, null });
		viewer.setCellModifier(new LiveExprCellModifier(this));

		makeActions();
		hookContextMenu();
		contributeToActionBars();
		hookGlobalActions();

		// //////////////
		// Implement a "fake" tooltip
		// final Listener labelListener = new Listener()
		// {
		//
		// public void handleEvent(Event event)
		// {
		// Label label = (Label) event.widget;
		// Shell shell = label.getShell();
		// switch (event.type)
		// {
		// case SWT.MouseDown :
		// Event e = new Event();
		// e.item = (TableItem) label.getData("_TABLEITEM");
		// // Assuming table is single select, set the selection as
		// // if the mouse down event went through to the table
		// t.setSelection(new TableItem[]{(TableItem) e.item});
		// t.notifyListeners(SWT.Selection, e);
		// // fall through
		// case SWT.MouseExit :
		// shell.dispose();
		// break;
		// }
		// }
		// };

		final Listener tableListener = new Listener() {

			SourceViewerInformationControl info = null;

			public void handleEvent(Event event) {
				switch (event.type) {
				case SWT.Dispose:
				case SWT.KeyDown:
				case SWT.MouseMove: {
					if (info == null) {
						break;
					}
					info.dispose();
					info = null;
					break;
				}
				case SWT.MouseHover: {
					TableItem item = t.getItem(new Point(event.x, event.y));
					if (item != null) {
						String str = "??";
						// try
						// {
						// str = BackendUtil.prettyPrint(bk,
						// item.getText(1));
						// ErlLogger.debug(str);
						BackendEvalResult r = ErlideBackend.eval(fBackend,
								"lists:flatten(io_lib:format(\"~p\", ["
										+ item.getText(0) + "])).", null);
						if (r.isOk()) {
							str = ((OtpErlangString) r.getValue())
									.stringValue();
						} else {
							str = r.getErrorReason().toString();
						}
						// ErlLogger.debug(str);
						// str = item.getText(1);
						// }
						// catch (BackendException e)
						// {
						// e.printStackTrace();
						// str = "???";
						// }

						info = new SourceViewerInformationControl(t.getShell(),
								SWT.ON_TOP | SWT.TOOL | SWT.RESIZE, SWT.MULTI
										| SWT.WRAP,
								PreferenceConstants.EDITOR_TEXT_FONT, null);
						info.setForegroundColor(t.getDisplay().getSystemColor(
								SWT.COLOR_INFO_FOREGROUND));
						info.setBackgroundColor(t.getDisplay().getSystemColor(
								SWT.COLOR_INFO_BACKGROUND));
						info.setInformation(str);

						Rectangle rect = item.getBounds(1);
						int lw = t.getGridLineWidth();
						Point pt = t.toDisplay(rect.x + lw, rect.y + lw);
						info.setLocation(pt);
						info.setSize(rect.width + lw, t.getBounds().height
								- rect.y);
						info.setVisible(true);
					}
				}
					break;
				}
			}
		};
		t.addListener(SWT.Dispose, tableListener);
		t.addListener(SWT.KeyDown, tableListener);
		t.addListener(SWT.MouseMove, tableListener);
		t.addListener(SWT.MouseHover, tableListener);

		// /////////////

	}

	/* We want Live Expressions to be persistant! */
	IMemento memento; // @jve:decl-index=0:

	@Override
	public void init(IViewSite site, IMemento aMemento)
			throws PartInitException {
		init(site);
		memento = aMemento;
	}

	@Override
	public void saveState(IMemento aMemento) {
		if (exprs.isEmpty()) {
			return;
		}
		aMemento = aMemento.createChild("LiveExpressions");
		Iterator<LiveExpr> iter = exprs.iterator();
		while (iter.hasNext()) {
			aMemento.createChild("expression").putTextData(
					iter.next().toString());
		}
	}

	private boolean restoreState() {
		if (memento != null) {
			memento = memento.getChild("LiveExpressions");
		}
		if (memento != null) {
			IMemento expressions[] = memento.getChildren("expression");
			if (expressions.length > 0) {
				exprs = new ArrayList<LiveExpr>(expressions.length);
				for (IMemento element : expressions) {
					exprs.add(new LiveExpr(element.getTextData()));
				}
			}
			return true;
		}
		memento = null;
		return false;
	}

	private static class LiveExprCellModifier implements ICellModifier {

		private final LiveExpressionsView view;

		public LiveExprCellModifier(LiveExpressionsView v) {
			view = v;
		}

		public boolean canModify(Object element, String property) {
			if ("expr".equals(property)) {
				return true;
			}
			return false;
		}

		public Object getValue(Object element, String property) {
			Object result = null;
			final LiveExpr el = (LiveExpr) element;
			result = el.fExpr;
			return result;
		}

		public void modify(Object element, String property, Object value) {
			LiveExpr el;
			// get around bug in TableEditorImpl
			if (element instanceof TableItem) {
				el = (LiveExpr) ((TableItem) element).getData();
			} else {
				el = (LiveExpr) element;
			}
			el.fExpr = (String) value;
			view.updateExpr(el);
		}
	}

	private void hookContextMenu() {
		final MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {

			public void menuAboutToShow(IMenuManager manager) {
				LiveExpressionsView.this.fillContextMenu(manager);
			}
		});
		final Menu menu = menuMgr.createContextMenu(viewer.getControl());
		viewer.getControl().setMenu(menu);
		getSite().registerContextMenu(menuMgr, viewer);
	}

	private void contributeToActionBars() {
		final IActionBars bars = getViewSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
	}

	private void fillLocalPullDown(IMenuManager manager) {
		manager.add(refreshAction);
		manager.add(fAddAction);
		manager.add(fRemoveAction);
		manager.add(new Separator());
	}

	void fillContextMenu(IMenuManager manager) {
		manager.add(refreshAction);
		manager.add(fAddAction);
		manager.add(fRemoveAction);
		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}

	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(refreshAction);
		manager.add(fAddAction);
		manager.add(fRemoveAction);
	}

	private void makeActions() {
		refreshAction = new Action() {

			@Override
			public void run() {
				viewer.refresh();
			}
		};
		refreshAction.setText("Refresh");
		refreshAction.setToolTipText("Refresh expressions");
		refreshAction.setImageDescriptor(ErlideUIPlugin.getDefault()
				.getImageDescriptor(ErlideUIConstants.IMG_REFRESH));

		fAddAction = new Action() {

			@Override
			public void run() {
				addExpr(new LiveExpr("expr"));
			}
		};
		fAddAction.setText("Add expression");
		fAddAction.setToolTipText("Add new expression");
		fAddAction.setImageDescriptor(PlatformUI.getWorkbench()
				.getSharedImages().getImageDescriptor(
						ISharedImages.IMG_TOOL_NEW_WIZARD));

		fRemoveAction = new Action() {

			@Override
			public void run() {
				delExpr();
			}
		};
		fRemoveAction.setText("Delete expression");
		fRemoveAction.setToolTipText("Delete expression");
		fRemoveAction.setImageDescriptor(PlatformUI.getWorkbench()
				.getSharedImages().getImageDescriptor(
						ISharedImages.IMG_TOOL_DELETE));

	}

	private void hookGlobalActions() {
		IActionBars bars = getViewSite().getActionBars();
		bars
				.setGlobalActionHandler(ActionFactory.DELETE.getId(),
						fRemoveAction);
		viewer.getControl().addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent event) {
				if (event.character == SWT.DEL && event.stateMask == 0
						&& fRemoveAction.isEnabled()) {
					fRemoveAction.run();
				}
			}
		});
	};

	void showMessage(String message) {
		MessageDialog.openInformation(viewer.getControl().getShell(),
				"Process list view", message);
	}

	/**
	 * Passing the focus request to the viewer's control.
	 */
	@Override
	public void setFocus() {
		viewer.getControl().setFocus();
	}

	public void resourceChanged(IResourceChangeEvent event) {
		refreshView();
	}

	private void refreshView() {
		if (!viewer.getControl().isDisposed()) {
			final Display display = viewer.getControl().getDisplay();

			if (!display.isDisposed()) {
				display.asyncExec(new Runnable() {

					public void run() {
						if (viewer != null && viewer.getControl().isDisposed()) {
							return;
						}
						viewer.refresh();
					}
				});
			}
		}
	}

	public void addExpr(LiveExpr e) {
		exprs.add(e);
		refreshView();
	}

	public void updateExpr(LiveExpr e) {
		refreshView();
	}

	public void delExpr() {
		IStructuredSelection sel = (IStructuredSelection) viewer.getSelection();
		@SuppressWarnings("unchecked")
		Iterator<LiveExpr> iter = sel.iterator();
		while (iter.hasNext()) {
			exprs.remove(iter.next());
		}
		refreshView();
	}

}
