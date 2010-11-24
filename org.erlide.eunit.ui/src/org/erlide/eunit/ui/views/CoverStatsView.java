package org.erlide.eunit.ui.views;

import java.util.Map;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.DrillDownAdapter;
import org.eclipse.ui.part.ViewPart;
import org.erlide.eunit.core.CoverResults;
import org.erlide.eunit.core.EUnitBackend;
import org.erlide.eunit.core.IEUnitObserver;
import org.erlide.eunit.ui.views.helpers.StatsNameSorter;
import org.erlide.eunit.ui.views.helpers.StatsViewContentProvider;
import org.erlide.eunit.ui.views.helpers.StatsViewLabelProvider;


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

public class CoverStatsView extends ViewPart implements IEUnitObserver{

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "org.erlide.eunit.core.views.TestResultView";

	private TreeViewer viewer;
	private DrillDownAdapter drillDownAdapter;
	private Action action1;
	private Action action2;
	private Action doubleClickAction;
	
	private EUnitBackend backend;
	
	
	/**
	 * The constructor.
	 */
	public CoverStatsView(){
		backend = EUnitBackend.getInstance();
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
		
		TreeColumn column1 = new TreeColumn(tree, SWT.LEFT);
		column1.setText("Name");
		column1.setWidth(540);
		TreeColumn column2 = new TreeColumn(tree, SWT.RIGHT);
		column2.setText("Lines");
		column2.setWidth(150);
		TreeColumn column3 = new TreeColumn(tree, SWT.RIGHT);
		column3.setText("Total");
		column3.setWidth(150);
		TreeColumn column4 = new TreeColumn(tree, SWT.RIGHT);
		column4.setText("%");
		column4.setWidth(150);
		
		
	/*	TreeItem total = new TreeItem(tree, SWT.NONE);
		total.setText(new String[] {"total", "1000", "250", "25"});
		
		for (int i = 0; i < 6; i++) {
		      TreeItem item = new TreeItem(total, SWT.NONE);
		      item.setText(new String[] { "module" + i, "100", "12", "12" });
		      for (int j = 0; j < 4; j++) {
		        TreeItem subItem = new TreeItem(item, SWT.NONE);
		        subItem
		            .setText(new String[] { "function" + j, "20", "3", "3" });
		      }
		}*/
		
		
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
		manager.add(action1);
		manager.add(new Separator());
		manager.add(action2);
	}

	private void fillContextMenu(IMenuManager manager) {
		manager.add(action1);
		manager.add(action2);
		manager.add(new Separator());
		drillDownAdapter.addNavigationActions(manager);
		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}
	
	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(action1);
		manager.add(action2);
		manager.add(new Separator());
		drillDownAdapter.addNavigationActions(manager);
	}

	private void makeActions() {
		action1 = new Action() {
			public void run() {
				showMessage("Action 1 executed");
			}
		};
		action1.setText("Open in editor");
		action1.setToolTipText("Opens the including file in editor");
		action1.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().
			getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
		
		action2 = new Action() {
			public void run() {
				showMessage("Action 2 executed");
			}
		};
		action2.setText("Show html report");
		action2.setToolTipText("Shows generated html report");
		action2.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().
				getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
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

	public void finishCovering() {
		System.out.println("Updating viewer");
		Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                updateTable();
            }
        });
	}
	
	private void updateTable(){
		
		System.out.println("updating....");
		
		int totalNum = backend.getHandler().getTotal();
		Map<String, CoverResults> results = backend.getHandler().getResults();
		
		Tree tree = viewer.getTree();
		tree.clearAll(true);
		
		TreeItem total = new TreeItem(tree, SWT.NONE);
		total.setText(new String[] {"total", "", "", Integer.toString(totalNum)});
		
		for(String res : results.keySet()){
			TreeItem subItem = new TreeItem(total, SWT.NONE);
			String linesNum = Integer.toString(results.get(res).linesTotal);
			String linesCov = Integer.toString(results.get(res).linesCovered);
			String percent = Double.toString(results.get(res).percent);
			
			System.out.format("%s, %s, %s, %s\n", res, linesNum, linesCov, percent);
			
			subItem.setText(new String[] {res, linesNum, linesCov, percent});
		}
	}
	
}