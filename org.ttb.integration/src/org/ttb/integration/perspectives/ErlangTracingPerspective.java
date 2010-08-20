package org.ttb.integration.perspectives;

import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.console.IConsoleConstants;
import org.erlide.ui.ErlideUIConstants;

/**
 * Perspective for tracing Erlang applications.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class ErlangTracingPerspective implements IPerspectiveFactory {

    public static final String CONTROL_PANEL_VIEW_ID = "org.ttb.integration.views.ControlPanelView";
    public static final String TREE_VIEWER_VIEW_ID = "org.ttb.integration.views.TreeViewerView";
    private static final String TOP_LEFT = "top left";
    private static final String BOTTOM_LEFT = "bottom left";
    private static final String BOTTOM_CENTER = "bottom center";
    private static final String RIGHT = "right";

    public void createInitialLayout(IPageLayout pageLayout) {
        defineActions(pageLayout);
        defineLayout(pageLayout);
    }

    private void defineActions(IPageLayout pageLayout) {
        pageLayout.addActionSet(IDebugUIConstants.LAUNCH_ACTION_SET);
        pageLayout.addActionSet(IDebugUIConstants.DEBUG_ACTION_SET);
        pageLayout.addActionSet(IPageLayout.ID_NAVIGATE_ACTION_SET);
    }

    private void defineLayout(IPageLayout pageLayout) {
        // top left
        IFolderLayout topLeft = pageLayout.createFolder(TOP_LEFT, IPageLayout.LEFT, 0.30f, pageLayout.getEditorArea());
        topLeft.addView(ErlideUIConstants.NAVIGATOR_VIEW_ID);

        // bottom left
        IFolderLayout bottomLeft = pageLayout.createFolder(BOTTOM_LEFT, IPageLayout.BOTTOM, 0.40f, TOP_LEFT);
        bottomLeft.addView(TREE_VIEWER_VIEW_ID);

        // bottom center
        IFolderLayout bottomCenter = pageLayout.createFolder(BOTTOM_CENTER, IPageLayout.BOTTOM, 0.60f, pageLayout.getEditorArea());
        bottomCenter.addView(IPageLayout.ID_PROBLEM_VIEW);
        bottomCenter.addView(IConsoleConstants.ID_CONSOLE_VIEW);
        bottomCenter.addView(CONTROL_PANEL_VIEW_ID);

        // right
        IFolderLayout right = pageLayout.createFolder(RIGHT, IPageLayout.RIGHT, 0.75f, pageLayout.getEditorArea());
        right.addView(IPageLayout.ID_OUTLINE);
    }
}
