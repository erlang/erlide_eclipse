package org.erlide.ui.views;

import java.util.ArrayList;

import org.eclipse.core.runtime.IAdaptable;
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
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.DrillDownAdapter;
import org.eclipse.ui.part.ViewPart;
import org.erlide.core.model.erlang.IErlModule;

/**
 * This sample class demonstrates how to plug-in a new workbench view. The view
 * shows data obtained from the model. The sample creates a dummy model on the
 * fly, but a real implementation would connect to the model available either in
 * this or another plug-in (e.g. the workspace). The view is connected to the
 * model using a content provider.
 * <p>
 * The view uses a label provider to define how model objects should be
 * presented in the view. Each view can present the same model objects using
 * different labels and icons, if needed. Alternatively, a single label provider
 * can be shared between views in order to ensure that objects of the same type
 * are presented in the same way everywhere.
 * <p>
 */

public class TraceLogView extends ViewPart {

    TreeViewer viewer;

    private DrillDownAdapter drillDownAdapter;

    private Action action1;

    private Action action2;

    Action doubleClickAction;

    /*
     * The content provider class is responsible for providing objects to the
     * view. It can wrap existing objects in adapters or simply return objects
     * as-is. These objects may be sensitive to the current input of the view,
     * or ignore it and always show the same content (like Task List, for
     * example).
     */

    class TreeObject implements IAdaptable {

        private final String name;

        private TreeParent parent;

        public TreeObject(final String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        public void setParent(final TreeParent parent) {
            this.parent = parent;
        }

        public TreeParent getParent() {
            return parent;
        }

        @Override
        public String toString() {
            return getName();
        }

        @Override
        public Object getAdapter(@SuppressWarnings("rawtypes") final Class key) {
            return null;
        }
    }

    class TreeParent extends TreeObject {

        private final ArrayList<TreeObject> children;

        public TreeParent(final String name) {
            super(name);
            children = new ArrayList<TreeObject>();
        }

        public void addChild(final TreeObject child) {
            children.add(child);
            child.setParent(this);
        }

        public void removeChild(final TreeObject child) {
            children.remove(child);
            child.setParent(null);
        }

        public TreeObject[] getChildren() {
            return children.toArray(new TreeObject[children.size()]);
        }

        public boolean hasChildren() {
            return children.size() > 0;
        }
    }

    class ViewContentProvider implements IStructuredContentProvider,
            ITreeContentProvider {

        private TreeParent invisibleRoot;

        @Override
        public void inputChanged(final Viewer v, final Object oldInput,
                final Object newInput) {
            if (newInput instanceof IErlModule[]) {
                // TreeObject to1 = new TreeObject("Leaf 1");
                final IErlModule[] x = (IErlModule[]) newInput;
                invisibleRoot = new TreeParent("");
                for (final IErlModule module : x) {
                    invisibleRoot.addChild(new TreeObject(module.getName()));
                }
            }
        }

        @Override
        public void dispose() {
        }

        @Override
        public Object[] getElements(final Object parent) {
            if (parent.equals(getViewSite())) {
                if (invisibleRoot == null) {
                    initialize();
                }
                return getChildren(invisibleRoot);
            }
            return getChildren(parent);
        }

        @Override
        public Object getParent(final Object child) {
            if (child instanceof TreeObject) {
                return ((TreeObject) child).getParent();
            }
            return null;
        }

        @Override
        public Object[] getChildren(final Object parent) {
            if (parent instanceof TreeParent) {
                return ((TreeParent) parent).getChildren();
            }
            return new Object[0];
        }

        @Override
        public boolean hasChildren(final Object parent) {
            if (parent instanceof TreeParent) {
                return ((TreeParent) parent).hasChildren();
            }
            return false;
        }

        /*
         * We will set up a dummy model to initialize tree heararchy. In a real
         * code, you will connect to a real model and expose its hierarchy.
         */
        private void initialize() {
            final TreeObject to1 = new TreeObject("Leaf 1");
            final TreeObject to2 = new TreeObject("Leaf 2");
            final TreeObject to3 = new TreeObject("Leaf 3");
            final TreeParent p1 = new TreeParent("Parent 1");
            p1.addChild(to1);
            p1.addChild(to2);
            p1.addChild(to3);

            final TreeObject to4 = new TreeObject("Leaf 4");
            final TreeParent p2 = new TreeParent("Parent 2");
            p2.addChild(to4);

            final TreeParent root = new TreeParent("Root");
            root.addChild(p1);
            root.addChild(p2);

            invisibleRoot = new TreeParent("");
            invisibleRoot.addChild(root);
        }
    }

    static class ViewLabelProvider extends LabelProvider {

        @Override
        public String getText(final Object obj) {
            return obj.toString();
        }

        @Override
        public Image getImage(final Object obj) {
            String imageKey = ISharedImages.IMG_OBJ_ELEMENT;
            if (obj instanceof TreeParent) {
                imageKey = ISharedImages.IMG_OBJ_FOLDER;
            }
            return PlatformUI.getWorkbench().getSharedImages()
                    .getImage(imageKey);
        }
    }

    static class NameSorter extends ViewerSorter {
    }

    /**
     * The constructor.
     */
    public TraceLogView() {
    }

    /**
     * This is a callback that will allow us to create the viewer and initialize
     * it.
     */
    @Override
    public void createPartControl(final Composite parent) {
        viewer = new TreeViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
        drillDownAdapter = new DrillDownAdapter(viewer);
        viewer.setContentProvider(new ViewContentProvider());
        viewer.setLabelProvider(new ViewLabelProvider());
        viewer.setSorter(new NameSorter());
        viewer.setInput(getViewSite());
        makeActions();
        hookContextMenu();
        hookDoubleClickAction();
        contributeToActionBars();
    }

    private void hookContextMenu() {
        final MenuManager menuMgr = new MenuManager("#PopupMenu");
        menuMgr.setRemoveAllWhenShown(true);
        menuMgr.addMenuListener(new IMenuListener() {

            @Override
            public void menuAboutToShow(final IMenuManager manager) {
                TraceLogView.this.fillContextMenu(manager);
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
        manager.add(action1);
        manager.add(new Separator());
        manager.add(action2);
    }

    void fillContextMenu(final IMenuManager manager) {
        manager.add(action1);
        manager.add(action2);
        manager.add(new Separator());
        drillDownAdapter.addNavigationActions(manager);
        // Other plug-ins can contribute there actions here
        manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
    }

    private void fillLocalToolBar(final IToolBarManager manager) {
        manager.add(action1);
        manager.add(action2);
        manager.add(new Separator());
        drillDownAdapter.addNavigationActions(manager);
    }

    private void makeActions() {
        action1 = new Action() {

            @Override
            public void run() {
                showMessage("Action 1 executed");
            }
        };
        action1.setText("Action 1");
        action1.setToolTipText("Action 1 tooltip");
        action1.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));

        action2 = new Action() {

            @Override
            public void run() {
                // testing
                // final IErlModel mdl = ErlangCore.getModel();
                // final List<IErlModule> ms = mdl.findModule("test", ".*");
                // ErlLogger.debug("found(\"*\") " + ms.size());
                // for (IErlModule element : ms) {
                // ErlLogger.debug(" " + element.getName());
                // }
                //
                // final List<IErlFunction> fs = mdl.findFunction("test", ".*",
                // "st.*", IErlModel.UNKNOWN_ARITY);
                // ErlLogger.debug("found(\"*\") " + fs.size());
                // for (IErlFunction element : fs) {
                // ErlLogger.debug(" " + element.getName() + "/"
                // + element.getArity());
                // }
                //
                // viewer.setInput(ms);
                //

                showMessage("Action 2 executed");
            }
        };
        action2.setText("Action 2");
        action2.setToolTipText("Action 2 tooltip");
        action2.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
        doubleClickAction = new Action() {

            @Override
            public void run() {
                final ISelection selection = viewer.getSelection();
                final Object obj = ((IStructuredSelection) selection)
                        .getFirstElement();
                showMessage("Double-click detected on " + obj.toString());
            }
        };
    }

    private void hookDoubleClickAction() {
        viewer.addDoubleClickListener(new IDoubleClickListener() {

            @Override
            public void doubleClick(final DoubleClickEvent event) {
                doubleClickAction.run();
            }
        });
    }

    void showMessage(final String message) {
        MessageDialog.openInformation(viewer.getControl().getShell(),
                "Trace Log View", message);
    }

    /**
     * Passing the focus request to the viewer's control.
     */
    @Override
    public void setFocus() {
        viewer.getControl().setFocus();
    }
}
