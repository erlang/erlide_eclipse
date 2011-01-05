package org.erlide.cover.ui.views;

import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.DrillDownAdapter;
import org.eclipse.ui.part.ViewPart;
import org.erlide.cover.core.CoverBackend;
import org.erlide.cover.core.ICoverObserver;
import org.erlide.cover.ui.actions.HtmlReportAction;
import org.erlide.cover.ui.views.helpers.StatsNameSorter;
import org.erlide.cover.ui.views.helpers.StatsViewContentProvider;
import org.erlide.cover.ui.views.helpers.StatsViewLabelProvider;
import org.erlide.cover.views.model.StatsTreeModel;


/**
 * This sample class demonstrates how to plug-in a new
 * workbench view. The view shows data obtained from the
 * model. The sample creates a dummy model on the fly,
 * but a real implementation would connect to the model
 * available either in this or another plug-in (e.g. the workspace).
 * The view is connected to the model using a content provider.
 * <p>
 * The view uses a label provider to define how model
 * objects should be presented in the view. Each
 * view can present the same model objects using
 * different labels and icons, if needed. Alternatively,
 * a single label provider can be shared between views
 * in order to ensure that objects of the same type are
 * presented in the same way everywhere.
 * <p>
 */

public class CoverStatsView extends ViewPart implements ICoverObserver{

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
	private Action refresh;
	private Action restore;
	private Action doubleClickAction;
	
	private CoverBackend backend;
	
	private TreeColumn colName;
	private TreeColumn colLines;
	private TreeColumn colCovered;
	private TreeColumn colPercentage;
	
	
	/**
	 * The constructor.
	 */
	public CoverStatsView(){
		backend = CoverBackend.getInstance();
		backend.addListener(this);
	}

	/**
	 * This is a callback that will allow us
	 * to create the viewer and initialize it.
	 */
	public void createPartControl(Composite parent) {
		//layout
		final GridLayout containerLayout = new GridLayout(1, false);
		containerLayout.marginWidth = 0;
		containerLayout.marginHeight = 0;
		containerLayout.verticalSpacing = 3;
		parent.setLayout(containerLayout);
		
		
		viewer = new TreeViewer(parent, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL);
		drillDownAdapter = new DrillDownAdapter(viewer);
		viewer.setContentProvider(new StatsViewContentProvider(getViewSite()));
		viewer.setLabelProvider(new StatsViewLabelProvider());
		viewer.setSorter(new StatsNameSorter());
		viewer.setInput(getViewSite());
		viewer.getTree().setLayoutData(
				new GridData(SWT.FILL, SWT.FILL, true, true));
		
		
		createTableTree();
		viewer.setInput(StatsTreeModel.getInstance());
		
		viewer.refresh();

		// Create the help context id for the viewer's control
		PlatformUI.getWorkbench().getHelpSystem().setHelp(viewer.getControl(), "org.erlide.eunit.core.viewer");
		
		
		makeActions();
		
		hookContextMenu();
		hookDoubleClickAction();
		contributeToActionBars();
	}
	
	private void createTableTree() {
		
		Tree tree = viewer.getTree();
		
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
		colPercentage.setText("Coverage");
		colPercentage.setWidth(150);
		
	}

	private void hookContextMenu() {
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				CoverStatsView.this.fillContextMenu(manager);
			}
		});
		Menu menu = menuMgr.createContextMenu(viewer.getControl());
		viewer.getControl().setMenu(menu);
		getSite().registerContextMenu(menuMgr, viewer);
	}

	private void contributeToActionBars() {
		IActionBars bars = getViewSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
	}

	private void fillLocalPullDown(IMenuManager manager) {
		manager.add(clear);
		manager.add(refresh);
		manager.add(new Separator());
		manager.add(restore);
		manager.add(save);
	}

	private void fillContextMenu(IMenuManager manager) {
		manager.add(openItem);
		manager.add(showHtml);
		manager.add(new Separator());
		drillDownAdapter.addNavigationActions(manager);
		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}
	
	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(clear);
		manager.add(refresh);
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
	}
	
	private void makeOpenItemAction() {
		openItem = new Action() {
			public void run() {
				showMessage("Action open item");
			}
		};
		openItem.setText("Open in editor");
		openItem.setToolTipText("Opens the including file in editor");
		openItem.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().
			getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
	}
	
	private void makeShowHtmlAction() {
	    System.out.println(viewer.getSelection());
		showHtml = new HtmlReportAction(viewer);
		showHtml.setText("Show html report");
		showHtml.setToolTipText("Shows generated html report");
		showHtml.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().
				getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
	}
	
	private void makeClearAction() {
		clear = new Action() {
            
            public void run() {
            	showMessage("Action clear");
            }
        };
        clear.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
        		.getImageDescriptor(ISharedImages.IMG_ETOOL_CLEAR));
        clear.setToolTipText("Clear results");
	}
	
	private void makeRestoreAction() {
		restore = new Action() {
            
            public void run() {
            	showMessage("Action save");
            }
        };
        restore.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
        		.getImageDescriptor(ISharedImages.IMG_OBJ_FOLDER));
        restore.setToolTipText("Restore previous results");
	}	
	
	private void makeSaveAction() {
		save = new Action() {
            
            public void run() {
            	showMessage("Action save");
            }
        };
        save.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
        		.getImageDescriptor(ISharedImages.IMG_ETOOL_SAVE_EDIT));
        save.setToolTipText("Save coverage results");
	}
	
	private void makeRefreshAction() {
		refresh = new Action() {
            
            public void run() {
            	showMessage("Action refresh");
            }
        };
        //TODO change image 
        refresh.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
        		.getImageDescriptor(ISharedImages.IMG_TOOL_REDO));
        refresh.setToolTipText("Refresh coverage statistics view");
	}
	
	private void makeDoubleClickAction() {
		//TODO: use it
		doubleClickAction = new Action() {
			public void run() {
				ISelection selection = viewer.getSelection();
				Object obj = ((IStructuredSelection)selection).getFirstElement();
				showMessage("Double-click detected on "+obj.toString());
			}
		};
	}

	private void hookDoubleClickAction() {
		viewer.addDoubleClickListener(new IDoubleClickListener() {
			public void doubleClick(DoubleClickEvent event) {
				doubleClickAction.run();
			}
		});
	}
	private void showMessage(String message) {
		MessageDialog.openInformation(
			viewer.getControl().getShell(),
			"Coverage statistics",
			message);
	}

	/**
	 * Passing the focus request to the viewer's control.
	 */
	public void setFocus() {
		viewer.getControl().setFocus();
	}

	public void updateViewer() {
		System.out.println("Updating viewer");
		
		System.out.println(StatsTreeModel.getInstance().getRoot());
		
		Display.getDefault().asyncExec(new Runnable() {
            public void run() {
            	System.out.println("refreshing viewer");
                viewer.refresh();
            }
        });
	}

    public void showError(String place, String type, String info) {
        
        Shell activeShell = PlatformUI.getWorkbench().
                getDisplay().getActiveShell();
        
        
        ErrorDialog dial = new ErrorDialog(activeShell,
                "Coverage error", 
                String.format("Error at %s while %s: %s\n",
                        place, type, info),
                Status.OK_STATUS,
                Status.ERROR);
       
    }
	
	
}