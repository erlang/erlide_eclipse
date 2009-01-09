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
import org.erlide.core.erlang.ErlangCore;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.BackendEventListener;
import org.erlide.runtime.backend.BackendVisitor;
import org.erlide.runtime.backend.RuntimeInfo;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideProclist;

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

		public void inputChanged(final Viewer vwr, final Object oldInput,
				final Object newInput) {
			// TODO subscribe to backendmanager events
		}

		public Object[] getElements(final Object inputElement) {
			return ErlangCore.getBackendManager().getAllBackends();
		}
	}

	public static class BackendLabelProvider extends LabelProvider {

		@Override
		public Image getImage(final Object element) {
			return null;
		}

		@Override
		public String getText(final Object element) {
			final Backend b = (Backend) element;
			final RuntimeInfo info = b.getInfo();
			final String s = info.getName();
			// if (s == null) {
			// return "<default>";
			// }
			return s + "  " + info.getNodeName();
		}

	}

	public static final String ID = "org.erlide.ui.views.processlist.ProcessListView";

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
			BackendEventListener {

		public void inputChanged(final Viewer v, final Object oldInput,
				final Object newInput) {
		}

		public void dispose() {
			final Backend backend = getBackend();
			if (backend != null) {
				backend.removeEventListener("processlist", this);
			}
		}

		public Object[] getElements(final Object parent) {
			final Backend bk = getBackend();
			if (bk == null) {
				return new OtpErlangObject[] {};
			}
			bk.addEventListener("processlist", this);

			final OtpErlangList r = ErlideProclist.getProcessList(bk);
			if (r.arity() == 0) {
				return new OtpErlangObject[] {};
			}
			final OtpErlangObject[] ss = new OtpErlangObject[r.elements().length];

			for (int i = 0; i < r.elements().length; i++) {
				final OtpErlangTuple e = (OtpErlangTuple) r.elementAt(i);
				ss[i] = e;
			}

			return ss;
		}

		/**
		 * @see org.erlide.runtime.backend.BackendEventListener#eventReceived(com.ericsson.otp.erlang.OtpErlangObject)
		 */
		public void eventReceived(final OtpErlangObject event) {
			Display.getDefault().asyncExec(new Runnable() {
				public void run() {
					if (!viewer.getControl().isDisposed()) {
						viewer.refresh();
					}
				}
			});
		}
	}

	static class ViewLabelProvider extends LabelProvider implements
			ITableLabelProvider {

		public String getColumnText(final Object obj, final int index) {
			final OtpErlangTuple t = (OtpErlangTuple) obj;
			final OtpErlangObject e = t.elementAt(index + 1);
			if (e instanceof OtpErlangString) {
				return ((OtpErlangString) e).stringValue();
			}
			return e.toString();
		}

		public Image getColumnImage(final Object obj, final int index) {
			if (index == 0) {
				return getImage(obj);
			}
			return null;
		}

		@Override
		public Image getImage(final Object obj) {
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
	public void createPartControl(final Composite parent) {
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
		final Backend ideBackend = ErlangCore.getBackendManager()
				.getIdeBackend();
		if (ideBackend != null) {
			ErlideProclist.processListInit(ideBackend);
		}
		ErlangCore.getBackendManager().forEachProjectBackend(
				new BackendVisitor() {
					public void run(final Backend b) {
						ErlideProclist.processListInit(b);
					}
				});

		backends.setContentProvider(new BackendContentProvider());
		backends.setLabelProvider(new BackendLabelProvider());
		backends.setInput(ErlangCore.getBackendManager());

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

			public void menuAboutToShow(final IMenuManager manager) {
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

	private void fillLocalPullDown(final IMenuManager manager) {
		manager.add(refreshAction);
		manager.add(new Separator());
	}

	void fillContextMenu(final IMenuManager manager) {
		manager.add(refreshAction);
		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}

	private void fillLocalToolBar(final IToolBarManager manager) {
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

				final OtpErlangPid pid = (OtpErlangPid) ((OtpErlangTuple) obj)
						.elementAt(0);

				final OtpErlangObject r = ErlideProclist.getProcessInfo(
						getBackend(), pid);
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
			}
		};
	}

	private void hookDoubleClickAction() {
		viewer.addDoubleClickListener(new IDoubleClickListener() {

			public void doubleClick(final DoubleClickEvent event) {
				doubleClickAction.run();
			}
		});
	}

	void showMessage(final String message) {
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

	public Backend getBackend() {
		final IStructuredSelection sel = (IStructuredSelection) backends
				.getSelection();
		if (sel.getFirstElement() != null) {
			final Backend b = (Backend) sel.getFirstElement();
			return b;
		}
		final Backend b = ErlangCore.getBackendManager().getIdeBackend();
		if (b != null) {
			backends.setSelection(new StructuredSelection(b));
			return b;
		}
		return null;

	}

}
