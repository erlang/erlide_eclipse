package org.erlide.test_support.ui.suites;

import java.util.List;

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.part.ViewPart;

import com.google.common.collect.Lists;

public class RegressionResultsView extends ViewPart {
    public static final String VIEW_ID = "org.erlide.test_support.views.regressionresults";

    private Composite control;
    private TableViewer tableViewer;
    private Label label;

    private final List<String> data;

    public RegressionResultsView() {
        data = Lists.newArrayList();
    }

    @Override
    public void createPartControl(final Composite parent) {
        control = new Composite(parent, SWT.NONE);
        control.setLayout(new GridLayout(1, false));

        label = new Label(control, SWT.NONE);
        label.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1,
                1));
        label.setText("");

        tableViewer = new TableViewer(control, SWT.H_SCROLL | SWT.V_SCROLL);
        final Table list = tableViewer.getTable();
        list.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
        tableViewer.setLabelProvider(new RegressionResultsLabelProvider());
        tableViewer.setContentProvider(new RegressionResultsContentProvider());
        tableViewer.setInput(data);

        initToolbar();
    }

    private void initToolbar() {
        // final IActionBars actionBars = getViewSite().getActionBars();
        // final IMenuManager dropDownMenu = actionBars.getMenuManager();
        // final IToolBarManager toolBar = actionBars.getToolBarManager();

        // final Action action = new ClearTestResultsAction(tableViewer,
        // events);
        // dropDownMenu.add(action);
        // toolBar.add(action);
    }

    @Override
    public void setFocus() {
        tableViewer.getTable().setFocus();
    }

    public void clear() {

    }

    public void setMessage(final String string) {
        label.setText(string);
        label.update();
    }

    public void addLine(final String line) {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                data.add("" + data.size() + ": " + line);
                tableViewer.refresh();
                tableViewer.reveal(data.get(data.size() - 1));
                // tableViewer.refresh();
                control.update();
            }

        });
    }

}
