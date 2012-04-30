package org.erlide.cover.ui.views;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.part.ViewPart;
import org.erlide.cover.core.CoverBackend;
import org.erlide.cover.core.IEUnitObserver;
import org.erlide.cover.ui.views.helpers.TestViewContentProvider;
import org.erlide.cover.ui.views.helpers.TestViewLabelProvider;
import org.erlide.cover.views.model.TestTreeModel;

/**
 * Simple view to show EUnit test results
 * 
 * @author Aleksandra Lipiec
 * 
 */
public class EUnitView extends ViewPart implements IEUnitObserver {

    /**
     * The ID of the view as specified by the extension.
     */
    public static final String ID = "org.erlide.eunit.ui.EUnitView";

    private TreeViewer viewer;
    private Label resultsLabel;
    private final CoverBackend backend;

    public EUnitView() {
        backend = CoverBackend.getInstance();
        backend.addEUnitListener(this);
    }

    @Override
    public void createPartControl(final Composite parent) {
        // layout
        final GridLayout containerLayout = new GridLayout(1, false);
        containerLayout.marginWidth = 0;
        containerLayout.marginHeight = 0;
        containerLayout.verticalSpacing = 3;
        parent.setLayout(containerLayout);

        viewer = new TreeViewer(parent, SWT.SINGLE | SWT.H_SCROLL
                | SWT.V_SCROLL);

        viewer.setContentProvider(new TestViewContentProvider(getViewSite()));
        viewer.setLabelProvider(new TestViewLabelProvider());
        viewer.getTree().setLayoutData(
                new GridData(SWT.FILL, SWT.FILL, true, true));
        viewer.getTree().setLinesVisible(true);

        viewer.setInput(TestTreeModel.getInstance());

        resultsLabel = new Label(parent, SWT.SINGLE);
        resultsLabel
                .setText("Passed: 0    Failed: 0   Skipped: 0   Canceled: 0");
        final FontData font = new FontData();
        font.setStyle(SWT.BOLD);
        resultsLabel.setFont(new Font(Display.getCurrent(), font));

    }

    @Override
    public void setFocus() {
        viewer.getControl().setFocus();
    }

    @Override
    public void treeChanged() {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                viewer.refresh();
            }
        });
    }

    @Override
    public void labelChanged() {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                final TestTreeModel model = TestTreeModel.getInstance();
                resultsLabel.setText(String
                        .format("Passed: %d    Failed: %d   Skipped: %d   Canceled: %d",
                                model.getPass(), model.getFail(),
                                model.getSkip(), model.getCancel()));
            }
        });
    }

}
