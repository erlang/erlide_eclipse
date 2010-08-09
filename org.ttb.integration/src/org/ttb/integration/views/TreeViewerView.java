package org.ttb.integration.views;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.ttb.integration.TtbBackend;
import org.ttb.integration.mvc.controller.CollectedTracesContentProvider;
import org.ttb.integration.mvc.model.CollectedDataList;
import org.ttb.integration.mvc.model.ITraceNodeObserver;
import org.ttb.integration.mvc.model.TracePattern;
import org.ttb.integration.mvc.view.CollectedTracesLabelProvider;

/**
 * Sequence diagram which shows tracing results.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TreeViewerView extends ViewPart implements ITraceNodeObserver {

    private TreeViewer treeViewer;

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

        Button button = new Button(container, SWT.PUSH | SWT.CENTER);
        button.setText("Clear");
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                CollectedDataList.getInstance().clear();
                treeViewer.setInput(CollectedDataList.getInstance());
            }
        });
    }

    private void createTreeViewerPanel(Composite parent) {
        final Composite container = new Composite(parent, SWT.NONE);
        container.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        container.setLayout(new GridLayout());

        treeViewer = new TreeViewer(container);
        treeViewer.getTree().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        // providers
        treeViewer.setContentProvider(new CollectedTracesContentProvider());
        treeViewer.setLabelProvider(new CollectedTracesLabelProvider());
    }

    @Override
    public void setFocus() {
    }

    @Override
    public void addPattern(TracePattern tracePattern) {
    }

    @Override
    public void removePattern(TracePattern tracePattern) {
    }

    @Override
    public void updatePattern(TracePattern tracePattern) {
    }

    @Override
    public void startTracing() {
    }

    @Override
    public void stopTracing() {
        treeViewer.setInput(CollectedDataList.getInstance());
    }
}
