/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.views.processlist;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.IBackendEventListener;
import org.erlide.runtime.backend.IBackendVisitor;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * 
 * 
 * 
 * @author Vlad Dumitrescu
 */
public class ProcessListView extends ViewPart {

	public static class BackendContentProvider implements
			IStructuredContentProvider {

		public void dispose() {
			// TODO unsubscribe from backend manager

		}

		public void inputChanged(Viewer vwr, Object oldInput, Object newInput) {
			// TODO subscribe to backendmanager events
		}

		public Object[] getElements(Object inputElement) {
			return BackendManager.getDefault().getBackends();
		}
	}

	public static class BackendLabelProvider extends LabelProvider {

		@Override
		public Image getImage(Object element) {
			return null;
		}

		@Override
		public String getText(Object element) {
			final IBackend b = (IBackend) element;
			final String s = b.getLabel();
			if (s == null) {
				return "<default>";
			}
			return s;
		}

	}

	public static final String ID = "org.erlide.ui.views.processlist.ProcessListView";

	private static final String MODULE_NAME = "erlide_proclist";

	private Label label;

	private ComboViewer backends;

	TableViewer viewer;

	private Action refreshAction;

	Action doubleClickAction;

	/*
	 * The content provider class is responsible for providing objects to the
	 * view. It can wrap existing objects in adapters or simply return objects
	 * as-is. These objects may be sensitive to the current input of the view,
	 * or ignore it and always show the same content (like Task List, for
	 * example).
	 */
	class ViewContentProvider implements IStructuredContentProvider,
			IBackendEventListener {

		public void inputChanged(Viewer v, Object oldInput, Object newInput) {
		}

		public void dispose() {
			getBackend().removeEventListener("processlist", this);
		}

		public Object[] getElements(Object parent) {
			final IBackend bk = getBackend();
			bk.addEventListener("processlist", this);

			final OtpErlangList r = getProcessList(bk);
			final OtpErlangObject[] ss = new OtpErlangObject[r.elements().length];

			for (int i = 0; i < r.elements().length; i++) {
				final OtpErlangTuple e = (OtpErlangTuple) r.elementAt(i);
				ss[i] = e;
			}

			return ss;
		}

		/**
		 * @see org.erlide.runtime.backend.IBackendEventListener#eventReceived(com.ericsson.otp.erlang.OtpErlangObject)
		 */
		public void eventReceived(OtpErlangObject event) {
			Display.getDefault().asyncExec(new Runnable() {
				public void run() {
					viewer.refresh();
				}
			});
		}
	}

	static class ViewLabelProvider extends LabelProvider implements
			ITableLabelProvider {

		public String getColumnText(Object obj, int index) {
			final OtpErlangTuple t = (OtpErlangTuple) obj;
			final OtpErlangObject e = t.elementAt(index + 1);
			if (e instanceof OtpErlangString) {
				return ((OtpErlangString) e).stringValue();
			}
			return e.toString();
		}

		public Image getColumnImage(Object obj, int index) {
			if (index == 0) {
				return getImage(obj);
			}
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
	public ProcessListView() {
	}

	/**
	 * This is a callback that will allow us to create the viewer and initialize
	 * it.
	 */
	@Override
	public void createPartControl(Composite parent) {
		label = new Label(parent, SWT.NULL);
		backends = new ComboViewer(parent, SWT.SINGLE | SWT.V_SCROLL);
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

		final GridData comboData = new GridData();
		comboData.verticalAlignment = GridData.BEGINNING;
		comboData.horizontalAlignment = GridData.FILL;
		comboData.widthHint = 300;
		comboData.heightHint = 18;
		comboData.horizontalIndent = 0;
		comboData.horizontalSpan = 1;
		comboData.verticalSpan = 1;
		comboData.grabExcessHorizontalSpace = true;
		comboData.grabExcessVerticalSpace = false;
		backends.getControl().setLayoutData(comboData);
		backends.getControl().setSize(
				new org.eclipse.swt.graphics.Point(319, 18));

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

		final TableColumn colPid = new TableColumn(t, SWT.LEAD);
		colPid.setText("Pid/Name");
		colPid.setWidth(150);

		final TableColumn colStart = new TableColumn(t, SWT.LEAD);
		colStart.setText("Initial call");
		colStart.setWidth(300);

		final TableColumn colReds = new TableColumn(t, SWT.LEAD);
		colReds.setText("Reds");
		colReds.setWidth(80);

		final TableColumn colMsgs = new TableColumn(t, SWT.LEAD);
		colMsgs.setText("Msgs");
		colMsgs.setWidth(60);

		label.setText("Erlang backend node");

		// TODO this is wrong - all backends should be inited
		BackendManager.getDefault().forEachLocal(new IBackendVisitor() {

			public void run(IBackend b) {
				processListInit(b);
			}
		});

		backends.setContentProvider(new BackendContentProvider());
		backends.setLabelProvider(new BackendLabelProvider());
		backends.setInput(BackendManager.getDefault());

		viewer.setContentProvider(new ViewContentProvider());
		viewer.setLabelProvider(new ViewLabelProvider());
		// viewer.setSorter(new NameSorter());
		viewer.setInput(getViewSite());

		makeActions();
		hookContextMenu();
		hookDoubleClickAction();
		contributeToActionBars();
	}

	private void hookContextMenu() {
		final MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {

			public void menuAboutToShow(IMenuManager manager) {
				ProcessListView.this.fillContextMenu(manager);
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
		manager.add(new Separator());
	}

	void fillContextMenu(IMenuManager manager) {
		manager.add(refreshAction);
		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}

	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(refreshAction);
	}

	private void makeActions() {
		refreshAction = new Action() {

			@Override
			public void run() {
				viewer.refresh();
			}
		};
		refreshAction.setText("Refresh");
		refreshAction.setToolTipText("Refresh process list");
		refreshAction.setImageDescriptor(PlatformUI.getWorkbench()
				.getSharedImages().getImageDescriptor(
						ISharedImages.IMG_OBJS_INFO_TSK));

		doubleClickAction = new Action() {

			@Override
			public void run() {
				final ISelection selection = viewer.getSelection();
				final Object obj = ((IStructuredSelection) selection)
						.getFirstElement();

				if (obj == null) {
					return;
				}

				// final ErlangConsole c = ErlangConsole.getDefault();
				// if (c != null) {
				final OtpErlangObject pid = ((OtpErlangTuple) obj).elementAt(0);

				final OtpErlangObject r = getProcessInfo(getBackend(), pid);
				if (r instanceof OtpErlangList) {
					final OtpErlangList l = (OtpErlangList) r;
					String s = "";
					for (int i = 0; i < l.arity(); i++) {
						final OtpErlangTuple e = (OtpErlangTuple) l
								.elementAt(i);
						s += " " + e.elementAt(0).toString() + "\t= "
								+ e.elementAt(1).toString() + "\n";
					}
					showMessage(s);
				} else {
					showMessage("Process "
							+ pid.toString()
							+ " is probably dead.\nPlease refresh process list.");
				}
				// }
			}
		};
	}

	private void hookDoubleClickAction() {
		viewer.addDoubleClickListener(new IDoubleClickListener() {

			public void doubleClick(DoubleClickEvent event) {
				doubleClickAction.run();
			}
		});
	}

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

	public IBackend getBackend() {
		final IStructuredSelection sel = (IStructuredSelection) backends
				.getSelection();
		if (sel.getFirstElement() != null) {
			final IBackend b = (IBackend) sel.getFirstElement();
			return b;
		}
		final IBackend b = BackendManager.getDefault().getIdeBackend();
		backends.setSelection(new StructuredSelection(b));
		return b;
	}

	public static void processListInit(IBackend b) {
		try {
			b.rpc(MODULE_NAME, "process_list_init");
		} catch (final ErlangRpcException e) {
			e.printStackTrace();
		}
	}

	public static OtpErlangList getProcessList(IBackend b) {
		try {
			return (OtpErlangList) b.rpcx(MODULE_NAME, "process_list");
		} catch (final BackendException e) {
			e.printStackTrace();
			return new OtpErlangList();
		}
	}

	public static OtpErlangObject getProcessInfo(IBackend b,
			OtpErlangObject object) {
		try {
			return b.rpcx(MODULE_NAME, "get_process_info", object);
		} catch (final BackendException e) {
			e.printStackTrace();
			return new OtpErlangAtom("error");
		}
	}

}
