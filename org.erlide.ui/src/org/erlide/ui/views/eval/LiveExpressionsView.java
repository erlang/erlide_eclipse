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
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
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
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.launch.debug.BackendEvalResult;
import org.erlide.launch.debug.DebugHelper;
import org.erlide.ui.ErlideUIConstants;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.prefs.PreferenceConstants;
import org.erlide.ui.views.SourceViewerInformationControl;
import org.erlide.utils.ErlUtils;

/**
 * @author Vlad Dumitrescu
 */
public class LiveExpressionsView extends ViewPart implements
        IResourceChangeListener {

    public static final String ID = "org.erlide.ui.views.eval.LiveExpressionsView";

    private Label label;
    private List<LiveExpr> exprs;
    CheckboxTableViewer viewer;
    private Action refreshAction;
    private Action fAddAction;
    Action fRemoveAction;

    private static class LiveExpr {
        String fExpr;
        private String cachedValue = "";
        boolean doEval = false;

        public LiveExpr(final String s) {
            fExpr = s;
        }

        public void setDoEval(final boolean eval) {
            doEval = eval;
        }

        public String getValue() {
            if (doEval) {
                cachedValue = evaluate();
            }
            return cachedValue;
        }

        private String evaluate() {
            final IBackend b = BackendCore.getBackendManager().getIdeBackend();
            final BackendEvalResult r = DebugHelper.eval(b, fExpr + ".", null);
            if (r.isOk()) {
                return r.getValue().toString();
            }
            return "ERR: " + r.getErrorReason().toString();
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

        private List<LiveExpr> exprlist;

        public ViewContentProvider() {
            super();
        }

        @Override
        @SuppressWarnings("unchecked")
        public void inputChanged(final Viewer v, final Object oldInput,
                final Object newInput) {
            if (newInput instanceof List<?>) {
                exprlist = (List<LiveExpr>) newInput;
            }
        }

        @Override
        public void dispose() {
            exprlist = null;
        }

        @Override
        public Object[] getElements(final Object parent) {
            return exprlist.toArray();
        }

    }

    class ViewLabelProvider extends LabelProvider implements
            ITableLabelProvider {

        @Override
        public String getColumnText(final Object obj, final int index) {
            final LiveExpr e = (LiveExpr) obj;
            if (index == 0) {
                return e.fExpr;
            }
            if (index == 1) {
                e.setDoEval(viewer.getChecked(e));
                return e.getValue();
            }
            return null;
        }

        @Override
        public Image getColumnImage(final Object obj, final int index) {
            // if (index == 0)
            // return getImage(obj);
            // else
            return null;
        }

        @Override
        public Image getImage(final Object obj) {
            return PlatformUI.getWorkbench().getSharedImages()
                    .getImage(ISharedImages.IMG_OBJ_ELEMENT);
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
    }

    /**
     * This is a callback that will allow us to create the viewer and initialize
     * it.
     */
    @Override
    public void createPartControl(final Composite parent) {
        label = new Label(parent, SWT.NULL);
        final Table t = new Table(parent, SWT.SINGLE | SWT.V_SCROLL
                | SWT.FULL_SELECTION | SWT.CHECK);
        viewer = new CheckboxTableViewer(t);

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

        final Listener tableListener = new Listener() {

            SourceViewerInformationControl info = null;

            @Override
            public void handleEvent(final Event event) {
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
                    final TableItem item = t
                            .getItem(new Point(event.x, event.y));
                    if (item != null) {
                        String str = item.getText(1);
                        if (str.length() > 0) {
                            // ErlLogger.debug(str);
                            final BackendEvalResult r = DebugHelper.eval(
                                    BackendCore.getBackendManager()
                                            .getIdeBackend(),
                                    "lists:flatten(io_lib:format(\"~p\", ["
                                            + item.getText(1) + "])).", null);
                            if (r.isOk()) {
                                str = ErlUtils.asString(r.getValue());
                            } else {
                                str = r.getErrorReason().toString();
                            }
                            info = new SourceViewerInformationControl(
                                    t.getShell(), SWT.ON_TOP | SWT.TOOL
                                            | SWT.RESIZE, SWT.MULTI | SWT.WRAP,
                                    PreferenceConstants.EDITOR_TEXT_FONT, null);
                            info.setForegroundColor(t.getDisplay()
                                    .getSystemColor(SWT.COLOR_INFO_FOREGROUND));
                            info.setBackgroundColor(t.getDisplay()
                                    .getSystemColor(SWT.COLOR_INFO_BACKGROUND));
                            info.setInformation(str);

                            final Rectangle rect = item.getBounds(1);
                            final int lw = t.getGridLineWidth();
                            final Point pt = t.toDisplay(rect.x + lw, rect.y
                                    + lw);
                            info.setLocation(pt);
                            info.setSize(rect.width + lw, t.getBounds().height
                                    - rect.y);
                            info.setVisible(true);
                        }
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
    public void init(final IViewSite site, final IMemento aMemento)
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
        final Iterator<LiveExpr> iter = exprs.iterator();
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
            final IMemento expressions[] = memento.getChildren("expression");
            if (expressions.length > 0) {
                exprs = new ArrayList<LiveExpr>(expressions.length);
                for (final IMemento element : expressions) {
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

        public LiveExprCellModifier(final LiveExpressionsView v) {
            view = v;
        }

        @Override
        public boolean canModify(final Object element, final String property) {
            if ("expr".equals(property)) {
                return true;
            }
            return false;
        }

        @Override
        public Object getValue(final Object element, final String property) {
            Object result = null;
            final LiveExpr el = (LiveExpr) element;
            result = el.fExpr;
            return result;
        }

        @Override
        public void modify(final Object element, final String property,
                final Object value) {
            LiveExpr el;
            // get around bug in TableEditorImpl
            if (element instanceof TableItem) {
                el = (LiveExpr) ((TableItem) element).getData();
            } else {
                el = (LiveExpr) element;
            }
            el.fExpr = (String) value;
            el.cachedValue = "";
            view.updateExpr(el);
        }
    }

    private void hookContextMenu() {
        final MenuManager menuMgr = new MenuManager("#PopupMenu");
        menuMgr.setRemoveAllWhenShown(true);
        menuMgr.addMenuListener(new IMenuListener() {

            @Override
            public void menuAboutToShow(final IMenuManager manager) {
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

    private void fillLocalPullDown(final IMenuManager manager) {
        manager.add(refreshAction);
        manager.add(fAddAction);
        manager.add(fRemoveAction);
        manager.add(new Separator());
    }

    void fillContextMenu(final IMenuManager manager) {
        manager.add(refreshAction);
        manager.add(fAddAction);
        manager.add(fRemoveAction);
        // Other plug-ins can contribute there actions here
        manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
    }

    private void fillLocalToolBar(final IToolBarManager manager) {
        manager.add(refreshAction);
        manager.add(fAddAction);
        manager.add(fRemoveAction);
    }

    private void makeActions() {
        refreshAction = new Action() {

            @Override
            public void run() {
                refreshView();
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
                .getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_TOOL_NEW_WIZARD));

        fRemoveAction = new Action() {

            @Override
            public void run() {
                delExpr();
            }
        };
        fRemoveAction.setText("Delete expression");
        fRemoveAction.setToolTipText("Delete expression");
        fRemoveAction.setImageDescriptor(PlatformUI.getWorkbench()
                .getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_TOOL_DELETE));

    }

    private void hookGlobalActions() {
        final IActionBars bars = getViewSite().getActionBars();
        bars.setGlobalActionHandler(ActionFactory.DELETE.getId(), fRemoveAction);
        viewer.getControl().addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(final KeyEvent event) {
                if (event.character == SWT.DEL && event.stateMask == 0
                        && fRemoveAction.isEnabled()) {
                    fRemoveAction.run();
                }
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

    @Override
    public void resourceChanged(final IResourceChangeEvent event) {
        refreshView();
    }

    private void refreshView() {
        if (!viewer.getControl().isDisposed()) {
            final Display display = viewer.getControl().getDisplay();

            if (!display.isDisposed()) {
                display.asyncExec(new Runnable() {

                    @Override
                    public void run() {
                        if (viewer == null) {
                            return;
                        }
                        if (viewer.getControl().isDisposed()) {
                            return;
                        }
                        viewer.refresh();
                    }
                });
            }
        }
    }

    public void addExpr(final LiveExpr e) {
        exprs.add(e);
        refreshView();
    }

    public void updateExpr(final LiveExpr e) {
        refreshView();
    }

    public void delExpr() {
        final IStructuredSelection sel = (IStructuredSelection) viewer
                .getSelection();
        @SuppressWarnings("unchecked")
        final Iterator<LiveExpr> iter = sel.iterator();
        while (iter.hasNext()) {
            exprs.remove(iter.next());
        }
        refreshView();
    }

}
