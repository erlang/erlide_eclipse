package org.erlide.cover.ui.views;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.DrillDownAdapter;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.statushandlers.StatusManager;
import org.erlide.cover.core.CoverBackend;
import org.erlide.cover.core.ICoverEvent;
import org.erlide.cover.core.ICoverObserver;
import org.erlide.cover.core.Logger;
import org.erlide.cover.ui.Activator;
import org.erlide.cover.ui.actions.ClearCoverageAction;
import org.erlide.cover.ui.actions.EksportReports;
import org.erlide.cover.ui.actions.HideCoverageAction;
import org.erlide.cover.ui.actions.HtmlReportAction;
import org.erlide.cover.ui.actions.OpenItemAction;
import org.erlide.cover.ui.actions.RestoreAction;
import org.erlide.cover.ui.actions.SaveAction;
import org.erlide.cover.ui.actions.ShowCoverageAction;
import org.erlide.cover.ui.annotations.EditorTracker;
import org.erlide.cover.ui.views.helpers.StatsNameSorter;
import org.erlide.cover.ui.views.helpers.StatsViewContentProvider;
import org.erlide.cover.ui.views.helpers.StatsViewLabelProvider;
import org.erlide.cover.views.model.ICoverageObject;
import org.erlide.cover.views.model.StatsTreeModel;

/**
 * View for coverage statistics
 * 
 * Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 */
public class CoverStatsView extends ViewPart implements ICoverObserver {

    /**
     * The ID of the view as specified by the extension.
     */
    public static final String ID = "org.erlide.eunit.core.views.TestResultView";

    private TreeViewer viewer;
    private DrillDownAdapter drillDownAdapter;

    private Action openItem;
    private Action showHtml;
    private Action save;
    private Action clear;
    private Action eksportHTML;
    private Action restore;
    private Action doubleClickAction;
    private Action showCoverage;
    private Action hideCoverage;

    private final CoverBackend backend;
    private TreeColumn colName;
    private TreeColumn colLines;
    private TreeColumn colCovered;
    private TreeColumn colPercentage;

    private final Logger log; // logger

    private final ISelectionChangedListener viewerSelectionChanged = new ISelectionChangedListener() {

        @Override
        public void selectionChanged(final SelectionChangedEvent event) {
            event.getSelection();
            final ISelection selection = viewer.getSelection();

            if (!(selection instanceof ITreeSelection)) {
                final IStatus executionStatus = new Status(IStatus.ERROR,
                        Activator.PLUGIN_ID,
                        "Internall error occured: bad sellection type", null);
                StatusManager.getManager().handle(executionStatus,
                        StatusManager.SHOW);
                return;
            }

            final ITreeSelection treeSelection = (ITreeSelection) selection;
            final ICoverageObject obj = (ICoverageObject) treeSelection
                    .getFirstElement();

            if (obj == null) {
                return;
            }

            switch (obj.getType()) {
            case FUNCTION:
                showHtml.setEnabled(false);
                showCoverage.setEnabled(true);
                hideCoverage.setEnabled(true);
                openItem.setEnabled(true);
                break;
            case MODULE:
                showHtml.setEnabled(true);
                showCoverage.setEnabled(true);
                hideCoverage.setEnabled(true);
                openItem.setEnabled(true);
                break;
            case FOLDER:
                showHtml.setEnabled(true);
                showCoverage.setEnabled(false);
                hideCoverage.setEnabled(false);
                openItem.setEnabled(false);
                break;
            case PROJECT:
                showHtml.setEnabled(true);
                showCoverage.setEnabled(true);
                hideCoverage.setEnabled(true);
                openItem.setEnabled(false);
                break;
            }

            if (StatsTreeModel.getInstance().isChanged()) {
                showHtml.setEnabled(false);
            }
        }

    };

    /**
     * The constructor.
     */
    public CoverStatsView() {
        backend = CoverBackend.getInstance();
        backend.addListener(this);

        backend.addAnnotationMaker(EditorTracker.getInstance());
        backend.getAnnotationMaker().addAnnotations();
        log = Activator.getDefault();
    }

    /**
     * This is a callback that will allow us to create the viewer and initialize
     * it.
     */
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
        drillDownAdapter = new DrillDownAdapter(viewer);
        viewer.setContentProvider(new StatsViewContentProvider(getViewSite()));
        viewer.setLabelProvider(new StatsViewLabelProvider());
        viewer.setSorter(new StatsNameSorter());
        viewer.setInput(getViewSite());
        viewer.getTree().setLayoutData(
                new GridData(SWT.FILL, SWT.FILL, true, true));
        viewer.addSelectionChangedListener(viewerSelectionChanged);

        createTableTree();
        viewer.setInput(StatsTreeModel.getInstance());

        // Create the help context id for the viewer's control
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(viewer.getControl(), "org.erlide.eunit.core.viewer");

        makeActions();

        hookContextMenu();
        hookDoubleClickAction();
        contributeToActionBars();
    }

    private void createTableTree() {

        final Tree tree = viewer.getTree();

        tree.setLinesVisible(true);
        tree.setHeaderVisible(true);

        colName = new TreeColumn(tree, SWT.LEFT);
        colName.setText("Name");
        colName.setWidth(540);

        colLines = new TreeColumn(tree, SWT.RIGHT);
        colLines.setText("Total Lines");
        colLines.setWidth(150);

        colCovered = new TreeColumn(tree, SWT.RIGHT);
        colCovered.setText("Covered Lines");
        colCovered.setWidth(150);

        colPercentage = new TreeColumn(tree, SWT.RIGHT);
        colPercentage.setText("Coverage %");
        colPercentage.setWidth(150);

    }

    private void hookContextMenu() {
        final MenuManager menuMgr = new MenuManager("#PopupMenu");
        menuMgr.setRemoveAllWhenShown(true);
        menuMgr.addMenuListener(new IMenuListener() {
            @Override
            public void menuAboutToShow(final IMenuManager manager) {
                CoverStatsView.this.fillContextMenu(manager);
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
        manager.add(clear);
        manager.add(eksportHTML);
        manager.add(new Separator());
        manager.add(restore);
        manager.add(save);

    }

    private void fillContextMenu(final IMenuManager manager) {
        manager.add(openItem);
        manager.add(showHtml);
        manager.add(showCoverage);
        manager.add(hideCoverage);
        manager.add(new Separator());
        drillDownAdapter.addNavigationActions(manager);
        // Other plug-ins can contribute there actions here
        manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
    }

    private void fillLocalToolBar(final IToolBarManager manager) {
        manager.add(clear);
        manager.add(eksportHTML);
        manager.add(new Separator());
        manager.add(restore);
        manager.add(save);
        manager.add(new Separator());
        drillDownAdapter.addNavigationActions(manager);
    }

    private void makeActions() {
        makeClearAction();
        makeDoubleClickAction();
        makeOpenItemAction();
        makeRestoreAction();
        makeSaveAction();
        makeShowHtmlAction();
        makeRefreshAction();
        makeShowCoverageAction();
        makeHideCoverageAction();
    }

    private void makeShowCoverageAction() {
        showCoverage = new ShowCoverageAction(viewer);
        showCoverage.setText("Show coverage");
        showCoverage.setToolTipText("Shows item's coverage");
        showCoverage.setImageDescriptor(PlatformUI.getWorkbench()
                .getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
    }

    private void makeHideCoverageAction() {
        hideCoverage = new HideCoverageAction(viewer);
        hideCoverage.setText("Hide coverage");
        hideCoverage.setToolTipText("Hides item's coverage");
        hideCoverage.setImageDescriptor(PlatformUI.getWorkbench()
                .getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
    }

    private void makeOpenItemAction() {
        openItem = new OpenItemAction(viewer);
        openItem.setText("Open in editor");
        openItem.setToolTipText("Opens the including file in editor");
        openItem.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
    }

    private void makeShowHtmlAction() {
        log.info(viewer.getSelection());
        showHtml = new HtmlReportAction(viewer);
        showHtml.setText("Show html report");
        showHtml.setToolTipText("Shows generated html report");
        showHtml.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
    }

    private void makeClearAction() {
        clear = new ClearCoverageAction();
        clear.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_ETOOL_CLEAR));
        clear.setToolTipText("Clear coverage marking from editor");
    }

    private void makeRestoreAction() {
        restore = new RestoreAction(viewer);
        restore.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_OBJ_FOLDER));
        restore.setToolTipText("Restore previous results");
    }

    private void makeSaveAction() {
        save = new SaveAction(viewer.getControl().getShell());
        save.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_ETOOL_SAVE_EDIT));
        save.setToolTipText("Save coverage results");
    }

    private void makeRefreshAction() {
        eksportHTML = new EksportReports(viewer.getControl().getShell());
        // TODO change image
        eksportHTML.setImageDescriptor(PlatformUI.getWorkbench()
                .getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_TOOL_UP));
        eksportHTML.setToolTipText("Eksport coverage reports in HTML format.");
    }

    private void makeDoubleClickAction() {
        doubleClickAction = new OpenItemAction(viewer);
        doubleClickAction.setEnabled(false);
    }

    private void hookDoubleClickAction() {
        viewer.addDoubleClickListener(new IDoubleClickListener() {
            @Override
            public void doubleClick(final DoubleClickEvent event) {
                doubleClickAction.run();
            }
        });
    }

    /**
     * Passing the focus request to the viewer's control.
     */
    @Override
    public void setFocus() {
        viewer.getControl().setFocus();
    }

    @Override
    public void eventOccured(final ICoverEvent e) {

        switch (e.getType()) {
        case UPDATE:

            Display.getDefault().asyncExec(new Runnable() {
                @Override
                public void run() {
                    viewer.refresh();
                }
            });
            break;
        case ERROR:
            final IStatus executionStatus = new Status(IStatus.ERROR,
                    Activator.PLUGIN_ID, e.getInfo(), null);
            StatusManager.getManager().handle(executionStatus,
                    StatusManager.SHOW);
            break;
        default:
            break;
        }
    }

}
