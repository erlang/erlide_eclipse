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
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
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
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.backend.IErlideBackendVisitor;
import org.erlide.backend.events.ErlangEventHandler;
import org.erlide.ui.views.BackendContentProvider;
import org.erlide.ui.views.BackendLabelProvider;
import org.osgi.service.event.Event;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * 
 * 
 * 
 * @author Vlad Dumitrescu
 */
public class ProcessListView extends ViewPart {

    public static final String ID = "org.erlide.ui.views.processlist.ProcessListView";
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
    class ViewContentProvider implements IStructuredContentProvider {

        private final ProcessEventHandler handler = new ProcessEventHandler(
                getBackend());

        public ViewContentProvider() {
            handler.register();
        }

        @Override
        public void inputChanged(final Viewer v, final Object oldInput,
                final Object newInput) {
        }

        @Override
        public void dispose() {
            final IBackend backend = getBackend();
        }

        @Override
        public Object[] getElements(final Object parent) {
            final IBackend backend = getBackend();
            if (backend == null) {
                return new OtpErlangObject[] {};
            }

            final OtpErlangList r = ErlideProclist.getProcessList(backend);
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

        class ProcessEventHandler extends ErlangEventHandler {

            public ProcessEventHandler(final IBackend backend) {
                super("processlist", backend);
            }

            @Override
            public void handleEvent(final Event event) {
                Display.getDefault().asyncExec(new Runnable() {
                    @Override
                    public void run() {
                        if (!viewer.getControl().isDisposed()) {
                            viewer.refresh();
                        }
                    }
                });
            }
        }
    }

    static class ViewLabelProvider extends LabelProvider implements
            ITableLabelProvider {

        @Override
        public String getColumnText(final Object obj, final int index) {
            final OtpErlangTuple t = (OtpErlangTuple) obj;
            final OtpErlangObject e = t.elementAt(index + 1);
            if (e instanceof OtpErlangString) {
                return ((OtpErlangString) e).stringValue();
            }
            return e.toString();
        }

        @Override
        public Image getColumnImage(final Object obj, final int index) {
            if (index == 0) {
                return getImage(obj);
            }
            return null;
        }

        @Override
        public Image getImage(final Object obj) {
            return PlatformUI.getWorkbench().getSharedImages()
                    .getImage(ISharedImages.IMG_OBJ_ELEMENT);
        }
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
        final Composite container = new Composite(parent, SWT.NONE);
        final GridLayout thisLayout = new GridLayout(2, false);
        container.setLayout(thisLayout);
        thisLayout.marginWidth = 5;
        thisLayout.marginHeight = 5;
        thisLayout.makeColumnsEqualWidth = false;
        thisLayout.verticalSpacing = 1;

        final Label label = new Label(container, SWT.SHADOW_NONE);
        label.setText("Erlang backend node");

        backends = new ComboViewer(container, SWT.SINGLE | SWT.V_SCROLL);
        final Combo combo = backends.getCombo();
        combo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1,
                1));
        backends.getControl().setSize(
                new org.eclipse.swt.graphics.Point(319, 18));
        backends.setContentProvider(new BackendContentProvider());
        backends.setLabelProvider(new BackendLabelProvider());
        backends.setInput(BackendCore.getBackendManager());
        viewer = new TableViewer(container, SWT.SINGLE | SWT.V_SCROLL
                | SWT.FULL_SELECTION);
        final Table table = viewer.getTable();
        final GridData layoutData = new GridData(SWT.FILL, SWT.FILL, false,
                true, 2, 1);
        table.setLayoutData(layoutData);
        final Table t = (Table) viewer.getControl();
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
        viewer.setContentProvider(new ViewContentProvider());
        viewer.setLabelProvider(new ViewLabelProvider());
        // viewer.setSorter(new NameSorter());
        viewer.setInput(getViewSite());
        viewer.addDoubleClickListener(new IDoubleClickListener() {
            @Override
            public void doubleClick(final DoubleClickEvent event) {
                doubleClickAction.run();
            }
        });

        t.setLinesVisible(true);
        t.setHeaderVisible(true);

        // TODO this is wrong - all backends should be inited
        final IBackend ideBackend = BackendCore.getBackendManager()
                .getIdeBackend();
        if (ideBackend != null) {
            ErlideProclist.processListInit(ideBackend);
        }
        BackendCore.getBackendManager().forEachBackend(
                new IErlideBackendVisitor() {
                    @Override
                    public void visit(final IBackend b) {
                        ErlideProclist.processListInit(b);
                    }
                });

        makeActions();
        hookContextMenu();
        hookDoubleClickAction();
        contributeToActionBars();
    }

    private void hookContextMenu() {
        final MenuManager menuMgr = new MenuManager("#PopupMenu");
        final Menu menu = menuMgr.createContextMenu(viewer.getControl());
        viewer.getControl().setMenu(menu);
        getSite().registerContextMenu(menuMgr, viewer);
        menuMgr.setRemoveAllWhenShown(true);
        menuMgr.addMenuListener(new IMenuListener() {

            @Override
            public void menuAboutToShow(final IMenuManager manager) {
                ProcessListView.this.fillContextMenu(manager);
            }
        });
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
                .getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));

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
                    final StringBuilder s = new StringBuilder();
                    for (int i = 0; i < l.arity(); i++) {
                        final OtpErlangTuple e = (OtpErlangTuple) l
                                .elementAt(i);
                        s.append(' ').append(e.elementAt(0).toString())
                                .append("\t= ")
                                .append(e.elementAt(1).toString()).append('\n');
                    }
                    showMessage(s.toString());
                } else {
                    showMessage("Process "
                            + pid.toString()
                            + " is probably dead.\nPlease refresh process list.");
                }
            }
        };
    }

    private void hookDoubleClickAction() {
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

    public IBackend getBackend() {
        final IStructuredSelection sel = (IStructuredSelection) backends
                .getSelection();
        if (sel.getFirstElement() != null) {
            final IBackend b = (IBackend) sel.getFirstElement();
            return b;
        }
        final IBackend b = BackendCore.getBackendManager().getIdeBackend();
        if (b != null) {
            backends.setSelection(new StructuredSelection(b));
            return b;
        }
        return null;

    }

}
