package org.ttb.integration.views;

import java.util.Collection;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.part.ViewPart;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.runtime.backend.ErlideBackend;
import org.ttb.integration.TtbBackend;
import org.ttb.integration.mvc.controller.CellModifier;
import org.ttb.integration.mvc.controller.TracePatternContentProvider;
import org.ttb.integration.mvc.model.ITracePatternListObserver;
import org.ttb.integration.mvc.model.TracePattern;
import org.ttb.integration.mvc.model.TracePatternList;
import org.ttb.integration.mvc.view.Columns;
import org.ttb.integration.mvc.view.TracePatternLabelProvider;

/**
 * A control panel for tracing.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class ControlPanelView extends ViewPart implements ITracePatternListObserver {

    private TableViewer tableViewer;
    private Text projectName;
    private final TtbBackend ttbBackend = new TtbBackend();

    public ControlPanelView() {
        TracePatternList.getInstance().addListener(this);
    }

    @Override
    public void dispose() {
        TracePatternList.getInstance().removeListener(this);
        super.dispose();
    }

    @Override
    public void createPartControl(Composite parent) {
        addChildren(parent);
    }

    private void addChildren(Composite parent) {
        // Create a composite to hold the children
        GridData gridData = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.FILL_BOTH);
        parent.setLayoutData(gridData);

        // Set numColumns to 3 for the buttons
        GridLayout layout = new GridLayout(3, false);
        layout.marginWidth = 4;
        parent.setLayout(layout);

        createButtons(parent);
        projectName = new Text(parent, SWT.BORDER);
        createTable(parent);
    }

    private void createButtons(Composite parent) {
        // "Add" button
        Button button = new Button(parent, SWT.PUSH | SWT.CENTER);
        button.setText("Add");
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                TracePatternList.getInstance().addPattern(new TracePattern());
            }
        });

        // "Remove" button
        button = new Button(parent, SWT.PUSH | SWT.CENTER);
        button.setText("Remove");
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                TracePattern tracePattern = (TracePattern) ((IStructuredSelection) tableViewer.getSelection()).getFirstElement();
                if (tracePattern != null) {
                    TracePatternList.getInstance().removePattern(tracePattern);
                }
            }
        });

        // "Start/Stop" button
        final Button startButton = new Button(parent, SWT.PUSH | SWT.CENTER);
        // startButton.setText(TtbBackend.getInstance().isStarted() ? "Stop" :
        // "Start");
        startButton.setText(ttbBackend.isStarted() ? "Stop" : "Start");
        startButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                Collection<ErlideBackend> backends = ErlangCore.getBackendManager().getAllBackends();
                for (ErlideBackend erlideBackend : backends) {
                    System.out.println(erlideBackend.getName());
                }
                // if (TtbBackend.getInstance().isStarted()) {
                // TtbBackend.getInstance().stop();
                // startButton.setText("Start");
                // } else if (TtbBackend.getInstance().start()) {
                // startButton.setText("Stop");
                // }
                if (ttbBackend.isStarted()) {
                    ttbBackend.stop();
                    startButton.setText("Start");
                } else {
                    ttbBackend.setBackend(ErlangCore.getBackendManager().getByName(projectName.getText()));
                    if (ttbBackend.start()) {
                        startButton.setText("Stop");
                        for (Object o : TracePatternList.getInstance().toArray()) {
                            TracePattern tracePattern = (TracePattern) o;
                            System.out.println("adding: " + tracePattern.getFunctionName());
                            ttbBackend.addTracePattern(tracePattern);
                        }
                    }
                }
            }
        });
    }

    private void createTable(Composite parent) {
        int style = SWT.SINGLE | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.HIDE_SELECTION;
        GridData gridData = new GridData(GridData.FILL_BOTH);
        gridData.grabExcessVerticalSpace = true;
        gridData.horizontalSpan = 3;

        tableViewer = new TableViewer(parent, style);
        tableViewer.setUseHashlookup(true);

        // table
        Table table = tableViewer.getTable();
        table.setLayoutData(gridData);
        table.setHeaderVisible(true);
        table.setLinesVisible(true);

        // columns
        String columnProperties[] = new String[Columns.values().length];
        for (Columns column : Columns.values()) {
            TableColumn tableColumn = new TableColumn(table, SWT.CENTER, column.ordinal());
            tableColumn.setResizable(true);
            tableColumn.setMoveable(true);
            tableColumn.setWidth(column.getWidth());
            tableColumn.setText(column.getName());
            columnProperties[column.ordinal()] = column.name();
        }
        tableViewer.setColumnProperties(columnProperties);

        // providers
        tableViewer.setLabelProvider(new TracePatternLabelProvider());
        tableViewer.setContentProvider(new TracePatternContentProvider());

        // editors
        CellEditor[] editors = new CellEditor[Columns.values().length];
        editors[Columns.ENABLED.ordinal()] = new CheckboxCellEditor(table);
        editors[Columns.MODULE_NAME.ordinal()] = new TextCellEditor(table);
        editors[Columns.FUNCTION_NAME.ordinal()] = new TextCellEditor(table);
        tableViewer.setCellEditors(editors);
        tableViewer.setCellModifier(new CellModifier());

        // input
        tableViewer.setInput(TracePatternList.getInstance());
    }

    @Override
    public void setFocus() {
        // TODO Auto-generated method stub
    }

    @Override
    public void addPattern(TracePattern tracePattern) {
        tableViewer.add(tracePattern);
        // TtbBackend.getInstance().addTracePattern(tracePattern);
    }

    @Override
    public void removePattern(TracePattern tracePattern) {
        tableViewer.remove(tracePattern);
        // TtbBackend.getInstance().removeTracePattern(tracePattern);
    }

    @Override
    public void updatePattern(TracePattern tracePattern) {
        tableViewer.update(tracePattern, null);
    }
}
