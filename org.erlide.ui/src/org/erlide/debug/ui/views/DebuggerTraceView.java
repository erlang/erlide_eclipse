package org.erlide.debug.ui.views;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IDebugEventSetListener;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.ui.AbstractDebugView;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.CellLabelProvider;
import org.eclipse.jface.viewers.ColumnPixelData;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElementLocator;
import org.erlide.debug.ui.tracing.DebugTraceEvent;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.debug.model.ErlangDebugTarget;
import org.erlide.launch.debug.model.ErlangDebugTarget.TraceChangedEventData;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.ui.internal.ErlideUIPlugin;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class DebuggerTraceView extends AbstractDebugView implements
        IDebugEventSetListener {

    // Tree:
    // Launch
    // Node
    // Process
    // Tuples with ieval (or trace info)

    // -record(ieval,
    // {level = 1, % Current call level
    // line = -1, % Current source code line (of module)
    // module, % MFA which called the currently
    // function, % interpreted function
    // arguments, %
    // last_call = false % True if current expression is
    // }). % the VERY last to be evaluated
    // % (ie at all, not only in a clause)

    private final class TreeContentProvider implements ITreeContentProvider {

        @Override
        public void dispose() {

        }

        // public void updateChildCount(Object element, int
        // currentChildCount) {
        //
        // int length;
        // if (element instanceof MarkerItem)
        // length = ((MarkerItem) element).getChildren().length;
        // else
        // // If it is not a MarkerItem it is the root
        // length = ((CachedMarkerBuilder) element).getElements().length;
        //
        // int markerLimit = MarkerSupportInternalUtilities
        // .getMarkerLimit();
        // length = markerLimit > 0 ? Math.min(length, markerLimit)
        // : length;
        // if (currentChildCount == length)
        // return;
        // viewer.setChildCount(element, length);
        //
        // }

        // public void updateElement(Object parent, int index) {
        // MarkerItem newItem;
        //
        // if (parent instanceof MarkerItem)
        // newItem = ((MarkerItem) parent).getChildren()[index];
        // else
        // newItem = ((CachedMarkerBuilder) parent).getElements()[index];
        //
        // viewer.replace(parent, index, newItem);
        // updateChildCount(newItem, -1);
        //
        // if (!newItem.isConcrete()
        // && categoriesToExpand
        // .contains(((MarkerCategory) newItem).getName())) {
        // viewer.expandToLevel(newItem, 1);
        // categoriesToExpand.remove(newItem);
        // }
        //
        // }

        @Override
        public Object[] getChildren(final Object parentElement) {
            if (parentElement instanceof ILaunch) {
                final ILaunch launch = (ILaunch) parentElement;
                final List<IDebugTarget> nodes = nodeMap.get(launch);
                return nodes.toArray();
            } else if (parentElement instanceof IDebugTarget) {
                final IDebugTarget node = (IDebugTarget) parentElement;
                final List<DebugTraceEvent> events = eventMap.get(node);
                return events.toArray();
            }
            return NO_CHILDREN;
        }

        @Override
        public Object[] getElements(final Object inputElement) {
            // if (debugTarget == null) {
            // return NO_CHILDREN;
            // }
            return launches.toArray();
            // final List<OtpErlangTuple> traceList =
            // debugTarget.getTraceList();
            // if (traceList == null) {
            // return NO_CHILDREN;
            // }
            // return traceList.toArray(new Object[traceList.size()]);
        }

        @Override
        public Object getParent(final Object element) {
            return parentMap.get(element);
        }

        @Override
        public boolean hasChildren(final Object element) {
            if (element instanceof DebugTraceEvent) {
                return false;
            }
            return true;
        }

        @Override
        public void inputChanged(final Viewer theViewer, final Object oldInput,
                final Object newInput) {
        }
    }

    public static class ColumnLabelProvider extends CellLabelProvider {

        @Override
        public void update(final ViewerCell cell) {
            final Object element = cell.getElement();
            final int columnIndex = cell.getColumnIndex();
            String s = null;
            if (element instanceof ILaunch) {
                switch (columnIndex) {
                case 0:
                    s = "launch";
                    break;
                case 1:
                    final ILaunch l = (ILaunch) element;
                    s = l.toString();
                    break;
                default:
                    s = "";
                    break;
                }
            }
            if (element instanceof IDebugTarget) {
                switch (columnIndex) {
                case 0:
                    s = "node";
                    break;
                case 1:
                    final IDebugTarget target = (IDebugTarget) element;
                    s = target.toString();
                    break;
                default:
                    s = "";
                    break;
                }
            }
            if (element instanceof DebugTraceEvent) {
                final DebugTraceEvent dt = (DebugTraceEvent) element;
                final OtpErlangTuple t = dt.getTuple();
                final OtpErlangTuple t2 = (OtpErlangTuple) t.elementAt(1);
                switch (columnIndex) {
                case 0:
                    final OtpErlangAtom w = (OtpErlangAtom) t.elementAt(0);
                    final String what = w.atomValue();
                    s = what;
                    break;
                case 1:
                    final OtpErlangTuple ieval = (OtpErlangTuple) t2
                            .elementAt(0);
                    final OtpErlangAtom mod = (OtpErlangAtom) ieval
                            .elementAt(3);
                    final String module = mod.atomValue();
                    final OtpErlangLong lin = (OtpErlangLong) ieval
                            .elementAt(2);
                    s = module;
                    try {
                        final int line = lin.intValue();
                        s += ":" + line; //$NON-NLS-1$
                    } catch (final OtpErlangRangeException e) {
                    }
                    break;
                case 2:
                default:
                    final OtpErlangObject o = t2.elementAt(1);
                    s = o.toString();
                    break;
                }
            }
            if (s != null) {
                cell.setText(s);
            }
        }

    }

    protected static final Object[] NO_CHILDREN = new Object[0];
    //	private static final String DEBUG_TRACE_AS_LAUNCH = "DebugTraceAsLaunch"; //$NON-NLS-1$
    private TreeViewer viewer;
    private final List<ILaunch> launches = new ArrayList<ILaunch>();
    private final Map<ILaunch, List<IDebugTarget>> nodeMap = new HashMap<ILaunch, List<IDebugTarget>>();
    private final Map<IDebugTarget, List<DebugTraceEvent>> eventMap = new HashMap<IDebugTarget, List<DebugTraceEvent>>();
    private final Map<Object, Object> parentMap = new HashMap<Object, Object>();

    // private ErlangDebugTarget debugTarget;
    public DebuggerTraceView() {
    }

    @Override
    protected Viewer createViewer(final Composite parent) {
        viewer = new TreeViewer(new Tree(parent, SWT.H_SCROLL | SWT.V_SCROLL
                | SWT.MULTI | SWT.FULL_SELECTION));
        // setViewer(viewer);
        // super.createPartControl(parent);
        // parent.setLayout(new FillLayout());

        viewer.getTree().setLinesVisible(true);
        viewer.setUseHashlookup(true);

        createColumns();

        viewer.setContentProvider(getContentProvider());
        viewer.setLabelProvider(new ColumnLabelProvider());
        getSite().setSelectionProvider(viewer);

        viewer.setInput(this);
        DebugPlugin.getDefault().addDebugEventListener(this);

        // viewer.getTree().addTreeListener(new TreeAdapter() {

        // @Override
        // public void treeCollapsed(final TreeEvent e) {
        // removeExpandedCategory((MarkerCategory) e.item.getData());
        // }
        //
        // @Override
        // public void treeExpanded(final TreeEvent e) {
        // addExpandedCategory((MarkerCategory) e.item.getData());
        // }
        // });

        // // Set help on the view itself
        // viewer.getControl().addHelpListener(new HelpListener() {

        // public void helpRequested(HelpEvent e) {
        // Object provider = getAdapter(IContextProvider.class);
        // if (provider == null) {
        // return;
        // }
        //
        // IContext context = ((IContextProvider) provider)
        // .getContext(viewer.getControl());
        // PlatformUI.getWorkbench().getHelpSystem().displayHelp(context);
        // }
        //
        // });

        viewer.getTree().addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                final Object o = getSelectedInTree();
                final String msg = o == null ? "" : o.toString(); //$NON-NLS-1$
                getViewSite().getActionBars().getStatusLineManager()
                        .setMessage(msg);

            }
        });

        viewer.getTree().addMouseListener(new MouseListener() {

            @Override
            public void mouseDoubleClick(final MouseEvent e) {
                final Object o = getSelectedInTree();
                if (o instanceof OtpErlangTuple) {
                    final OtpErlangTuple t = (OtpErlangTuple) o;
                    final OtpErlangTuple t2 = (OtpErlangTuple) t.elementAt(1);
                    final OtpErlangTuple ieval = (OtpErlangTuple) t2
                            .elementAt(0);
                    final OtpErlangAtom mod = (OtpErlangAtom) ieval
                            .elementAt(3);
                    final String module = mod.atomValue();
                    final OtpErlangLong lin = (OtpErlangLong) ieval
                            .elementAt(2);
                    try {
                        final int line = lin.intValue();
                        gotoModuleLine(module, line);
                    } catch (final OtpErlangRangeException e1) {
                    }

                }
            }

            @Override
            public void mouseDown(final MouseEvent e) {
            }

            @Override
            public void mouseUp(final MouseEvent e) {
            }

        });
        // PlatformUI.getWorkbench().getWorkingSetManager()
        // .addPropertyChangeListener(getWorkingSetListener());
        return viewer;
        // registerContextMenu();
        // initDragAndDrop();
    }

    // @Override
    // public void createPartControl(final Composite parent) {
    // viewer = new TreeViewer(new Tree(parent, SWT.H_SCROLL | SWT.V_SCROLL
    // | SWT.MULTI | SWT.FULL_SELECTION));
    // setViewer(viewer);
    // super.createPartControl(parent);
    // // parent.setLayout(new FillLayout());
    //
    // viewer.getTree().setLinesVisible(true);
    // viewer.setUseHashlookup(true);
    //
    // createColumns();
    //
    // viewer.setContentProvider(getContentProvider());
    // viewer.setLabelProvider(new ColumnLabelProvider());
    // getSite().setSelectionProvider(viewer);
    //
    // viewer.setInput(this);
    // DebugPlugin.getDefault().addDebugEventListener(this);
    //
    // // viewer.getTree().addTreeListener(new TreeAdapter() {

    // // @Override
    // // public void treeCollapsed(final TreeEvent e) {
    // // removeExpandedCategory((MarkerCategory) e.item.getData());
    // // }
    // //
    // // @Override
    // // public void treeExpanded(final TreeEvent e) {
    // // addExpandedCategory((MarkerCategory) e.item.getData());
    // // }
    // // });
    //
    // // // Set help on the view itself
    // // viewer.getControl().addHelpListener(new HelpListener() {
    // // public void helpRequested(HelpEvent e) {
    // // Object provider = getAdapter(IContextProvider.class);
    // // if (provider == null) {
    // // return;
    // // }
    // //
    // // IContext context = ((IContextProvider) provider)
    // // .getContext(viewer.getControl());
    // // PlatformUI.getWorkbench().getHelpSystem().displayHelp(context);
    // // }
    // //
    // // });
    //
    // viewer.getTree().addSelectionListener(new SelectionAdapter() {
    // @Override
    // public void widgetSelected(final SelectionEvent e) {
    // final Object o = getSelectedInTree();
    //				final String msg = o == null ? "" : o.toString(); //$NON-NLS-1$
    // getViewSite().getActionBars().getStatusLineManager()
    // .setMessage(msg);
    //
    // }
    // });
    //
    // viewer.getTree().addMouseListener(new MouseListener() {
    //
    // public void mouseDoubleClick(final MouseEvent e) {
    // final Object o = getSelectedInTree();
    // if (o instanceof OtpErlangTuple) {
    // final OtpErlangTuple t = (OtpErlangTuple) o;
    // final OtpErlangTuple t2 = (OtpErlangTuple) t.elementAt(1);
    // final OtpErlangTuple ieval = (OtpErlangTuple) t2
    // .elementAt(0);
    // final OtpErlangAtom mod = (OtpErlangAtom) ieval
    // .elementAt(3);
    // final String module = mod.atomValue();
    // final OtpErlangLong lin = (OtpErlangLong) ieval
    // .elementAt(2);
    // try {
    // final int line = lin.intValue();
    // gotoModuleLine(module, line);
    // } catch (final OtpErlangRangeException e1) {
    // }
    //
    // }
    // }
    //
    // public void mouseDown(final MouseEvent e) {
    // }
    //
    // public void mouseUp(final MouseEvent e) {
    // }
    //
    // });
    // // PlatformUI.getWorkbench().getWorkingSetManager()
    // // .addPropertyChangeListener(getWorkingSetListener());
    //
    // // registerContextMenu();
    // // initDragAndDrop();
    //
    // }

    protected void gotoModuleLine(final String moduleName, final int line) {
        final IWorkbenchWindow window = ErlideUIPlugin
                .getActiveWorkbenchWindow();
        if (window == null) {
            return;
        }
        final IWorkbenchPage page = window.getActivePage();
        if (page == null) {
            return;
        }

        IEditorPart part = null;
        final IErlElementLocator model = ErlModelManager.getErlangModel();
        IErlModule module;
        try {
            module = model.findModule(moduleName);
        } catch (final ErlModelException e) {
            ErlLogger.error(e);
            return;
        }
        IEditorInput input = null;
        input = EditorUtility.getEditorInput(module);
        if (input != null) {
            final String editorId = EditorUtility.getEditorID(input, module);
            if (editorId != null) {
                try {
                    part = page.openEditor(input, editorId);
                } catch (final PartInitException e) {
                    ErlideUIPlugin.errorDialog(window.getShell(), "Go to File",
                            "Exception occurred", e); //
                }
            }
        }
        if (part instanceof ErlangEditor) {
            part.setFocus();
            final ErlangEditor ee = (ErlangEditor) part;
            final IDocument d = ee.getDocument();
            int lineStart, lineLength;
            try {
                lineStart = d.getLineOffset(line - 1);
                lineLength = d.getLineLength(line - 1);
                EditorUtility.revealInEditor(ee, lineStart, lineLength - 1);
            } catch (final BadLocationException e) {
                e.printStackTrace();
            }
        }
    }

    private IContentProvider getContentProvider() {
        return new TreeContentProvider();
    }

    private void createColumns() {

        final Tree tree = viewer.getTree();
        final TableLayout layout = new TableLayout();
        TreeViewerColumn column;
        final String[] names = {
                "Kind", "Function", org.erlide.debug.ui.views.ActionMessages.getString("DebuggerTraceView.5") }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        for (final String name : names) {
            column = new TreeViewerColumn(viewer, SWT.NONE);
            final TreeColumn treeColumn = column.getColumn();
            treeColumn.setResizable(true);
            treeColumn.setMoveable(true);
            treeColumn.addSelectionListener(new SelectionListener() {

                @Override
                public void widgetDefaultSelected(final SelectionEvent e) {
                }

                @Override
                public void widgetSelected(final SelectionEvent e) {
                }
            });

            // column.getColumn().setData(MARKER_FIELD, markerField);
            // Show the help in the first column
            column.setLabelProvider(new ColumnLabelProvider());
            treeColumn.setText(name);
            treeColumn.setToolTipText(name);
        }
        // column = new TreeViewerColumn(viewer, SWT.NONE);
        // treeColumn = column.getColumn();
        // treeColumn.setResizable(true);
        // treeColumn.setMoveable(true);
        // column.setLabelProvider(new ColumnLabelProvider());
        // column.getColumn().setImage(markerField.getColumnHeaderImage());

        // final EditingSupport support = markerField
        // .getEditingSupport(viewer);
        // if (support != null) {
        // column.setEditingSupport(support);
        // }

        // if (builder.getPrimarySortField().equals(markerField)) {
        // updateDirectionIndicator(column.getColumn(), markerField);
        // }

        int columnWidth = -1;

        for (int i = 0; i < names.length; ++i) {
            if (i == 0) {
                // Compute and store a font metric
                final GC gc = new GC(tree);
                gc.setFont(tree.getFont());
                final FontMetrics fontMetrics = gc.getFontMetrics();
                gc.dispose();
                columnWidth = Math.max(100,
                        fontMetrics.getAverageCharWidth() * 20);
            }

            // if (columnWidths != null) {
            // final Integer value = columnWidths.getInteger(getFieldId(column
            // .getColumn()));
            //
            // // Make sure we get a useful value
            // if (value != null && value.intValue() > 0) {
            // columnWidth = value.intValue();
            // }
            // }

            // // Take trim into account if we are using the default value, but
            // not
            // // if it is restored.
            // if (columnWidth < 0) {
            // layout.addColumnData(new ColumnPixelData(markerField
            // .getDefaultColumnWidth(tree), true, true));
            // } else {
            layout.addColumnData(new ColumnPixelData(columnWidth, true));
            // }
        }
        // }

        // Remove extra columns
        // if (currentColumns.length > fields.length) {
        // for (int i = fields.length; i < currentColumns.length; i++) {
        // currentColumns[i].dispose();
        //
        // }
        // }

        viewer.getTree().setLayout(layout);
        tree.setLinesVisible(true);
        tree.setHeaderVisible(true);
        tree.layout(true);

    }

    @Override
    public void setFocus() {
    }

    @Override
    public void handleDebugEvents(final DebugEvent[] events) {
        for (final DebugEvent event : events) {
            if (event.getKind() == DebugEvent.MODEL_SPECIFIC
                    && event.getDetail() == ErlangDebugTarget.TRACE_CHANGED) {
                final Object source = event.getSource();
                if (source instanceof ErlangDebugTarget) {
                    final TraceChangedEventData data = (TraceChangedEventData) event
                            .getData();
                    traceChanged(data, source);
                }
            }
        }
    }

    private void traceChanged(final TraceChangedEventData data,
            final Object source) {
        if (viewer == null || viewer.getControl().isDisposed()) {
            return;
        }
        final Display display = viewer.getControl().getDisplay();
        if (!display.isDisposed()) {
            display.asyncExec(new Runnable() {
                @Override
                public void run() {
                    if (viewer == null || viewer.getControl().isDisposed()) {
                        return;
                    }
                    // if (viewer.getInput() != source) {
                    // viewer.setInput(source);
                    // viewer.refresh();
                    // } else {
                    if (data.getWhat() == TraceChangedEventData.ADDED) {
                        final ILaunch launch = data.getLaunch();
                        if (!launches.contains(launch)) {
                            launches.add(launch);
                            viewer.add(viewer.getInput(), launch);
                            parentMap.put(launch, viewer.getInput());
                        }
                        List<IDebugTarget> nodes = nodeMap.get(launch);
                        if (nodes == null) {
                            nodes = new ArrayList<IDebugTarget>(1);
                            nodeMap.put(launch, nodes);
                        }
                        final IDebugTarget node = data.getNode();
                        if (!nodes.contains(node)) {
                            nodes.add(node);
                            viewer.add(launch, node);
                            parentMap.put(node, launch);
                        }
                        List<DebugTraceEvent> events = eventMap.get(node);
                        if (events == null) {
                            events = new ArrayList<DebugTraceEvent>(data
                                    .getEvents().length);
                            eventMap.put(node, events);
                        }
                        final OtpErlangPid pid = data.getPid();
                        for (final OtpErlangTuple t : data.getEvents()) {
                            events.add(new DebugTraceEvent(pid, t));
                            parentMap.put(t, node);
                        }
                        viewer.add(node, data.getEvents());
                    }
                    // }
                }
            });
        }
    }

    Object getSelectedInTree() {
        final ISelection selection = viewer.getSelection();
        if (selection instanceof IStructuredSelection) {
            final IStructuredSelection ss = (IStructuredSelection) selection;
            if (ss.size() == 1) {
                return ss.getFirstElement();
            }
        }
        return null;
    }

    @Override
    protected void configureToolBar(final IToolBarManager tbm) {
    }

    @Override
    protected void createActions() {
    }

    @Override
    protected void fillContextMenu(final IMenuManager menu) {
    }

    @Override
    protected String getHelpContextId() {
        return null;
    }

    public List<DebugTraceEvent> getEventsForLaunch(final IDebugTarget target) {
        return eventMap.get(target);
    }

}
