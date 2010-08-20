package org.ttb.integration.views;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.util.ErlModelUtils;
import org.ttb.integration.TtbBackend;
import org.ttb.integration.mvc.controller.CollectedTracesContentProvider;
import org.ttb.integration.mvc.model.CollectedDataList;
import org.ttb.integration.mvc.model.ITraceNodeObserver;
import org.ttb.integration.mvc.model.TracePattern;
import org.ttb.integration.mvc.model.treenodes.FunctionNode;
import org.ttb.integration.mvc.model.treenodes.ITreeNode;
import org.ttb.integration.mvc.model.treenodes.ModuleNode;
import org.ttb.integration.mvc.view.CollectedTracesLabelProvider;

/**
 * Sequence diagram which shows tracing results.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TreeViewerView extends ViewPart implements ITraceNodeObserver {

    private TreeViewer treeViewer;
    private Button clearButton;
    private Button loadButton;

    public TreeViewerView() {
        TtbBackend.getInstance().addListener(this);
    }

    @Override
    public void dispose() {
        TtbBackend.getInstance().removeListener(this);
        super.dispose();
    }

    @Override
    public void createPartControl(Composite parent) {
        // layout
        final GridLayout containerLayout = new GridLayout(1, false);
        containerLayout.marginWidth = 0;
        containerLayout.marginHeight = 0;
        containerLayout.verticalSpacing = 3;
        parent.setLayout(containerLayout);

        // children
        createButtonPanel(parent);
        createTreeViewerPanel(parent);
    }

    private void createButtonPanel(Composite parent) {
        final Composite container = new Composite(parent, SWT.NONE);
        container.setLayout(new RowLayout());

        clearButton = new Button(container, SWT.PUSH | SWT.CENTER);
        clearButton.setText("Clear");
        clearButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                CollectedDataList.getInstance().clear();
                treeViewer.setInput(CollectedDataList.getInstance());
            }
        });

        loadButton = new Button(container, SWT.PUSH | SWT.CENTER);
        loadButton.setText("Load...");
        loadButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                // TODO add support for multiple selection
                FileDialog dialog = new FileDialog(PlatformUI.getWorkbench().getDisplay().getActiveShell(), SWT.OPEN);
                // dialog.setFilterExtensions(new String[] { "*.*" });
                dialog.setText("Load trace data...");
                String selected = dialog.open();
                if (selected != null)
                    TtbBackend.getInstance().loadData(selected);
            }
        });
    }

    private void createTreeViewerPanel(Composite parent) {
        final Composite container = new Composite(parent, SWT.NONE);
        container.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        container.setLayout(new GridLayout());

        // treeViewer = new TreeViewer(container, SWT.VIRTUAL);
        treeViewer = new TreeViewer(container);
        treeViewer.getTree().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        // providers
        treeViewer.setContentProvider(new CollectedTracesContentProvider(treeViewer));
        treeViewer.setLabelProvider(new CollectedTracesLabelProvider());

        // listener
        treeViewer.addDoubleClickListener(new IDoubleClickListener() {

            public void doubleClick(DoubleClickEvent event) {
                doDoubleClick(event);
            }
        });
    }

    private void doDoubleClick(DoubleClickEvent event) {
        IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        ITreeNode treeNode = (ITreeNode) selection.getFirstElement();
        try {
            if (treeNode instanceof FunctionNode) {
                FunctionNode functionNode = (FunctionNode) treeNode;
                ErlangFunction erlangFunction = new ErlangFunction(functionNode.getFunctionName(), functionNode.getArity());
                ErlModelUtils.openExternalFunction(functionNode.getModuleName(), erlangFunction, null, null, null, true);
            } else if (treeNode instanceof ModuleNode) {
                ModuleNode moduleNode = (ModuleNode) treeNode;
                ErlModelUtils.openExternalType(moduleNode.getModuleName(), moduleNode.getModuleName(), null, null, null, true);
            }
        } catch (CoreException e) {
            ErlLogger.error(e);
        }
    }

    @Override
    public void setFocus() {
    }

    public void addPattern(TracePattern tracePattern) {
    }

    public void removePattern(TracePattern tracePattern) {
    }

    public void updatePattern(TracePattern tracePattern) {
    }

    public void startTracing() {
        loadButton.setEnabled(false);
        clearButton.setEnabled(false);
        treeViewer.setInput(CollectedDataList.getInstance());
    }

    public void stopTracing() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                treeViewer.setInput(CollectedDataList.getInstance());
                loadButton.setEnabled(true);
                clearButton.setEnabled(true);
            }
        });
    }

    public void receivedTraceData() {
        // treeViewer.setInput(CollectedDataList.getInstance());
    }

    public void startLoading() {
        loadButton.setEnabled(false);
        clearButton.setEnabled(false);
        treeViewer.setInput(CollectedDataList.getInstance());
    }

    public void stopLoading() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                treeViewer.setInput(CollectedDataList.getInstance());
                loadButton.setEnabled(true);
                clearButton.setEnabled(true);
            }
        });
    }
}
