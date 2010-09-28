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
    public static final String BROWSER_VIEW_ID = "org.ttb.integration.views.TraceBrowserView";
    private static final String TOP_LEFT = "top left";
    private static final String BOTTOM_LEFT = "bottom left";
    private static final String CENTER_LEFT = "center left";
    private static final String BOTTOM_CENTER = "bottom center";
    private static final String RIGHT_CENTER = "right center";
    private static final String RIGHT_BOTTOM = "right bottom";

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

        // center left
        IFolderLayout centerLeft = pageLayout.createFolder(CENTER_LEFT, IPageLayout.BOTTOM, 0.20f, TOP_LEFT);
        centerLeft.addView(BROWSER_VIEW_ID);

        // bottom left
        IFolderLayout bottomLeft = pageLayout.createFolder(BOTTOM_LEFT, IPageLayout.BOTTOM, 0.30f, CENTER_LEFT);
        bottomLeft.addView(TREE_VIEWER_VIEW_ID);

        // bottom center
        IFolderLayout bottomCenter = pageLayout.createFolder(BOTTOM_CENTER, IPageLayout.BOTTOM, 0.60f, pageLayout.getEditorArea());
        bottomCenter.addView(IPageLayout.ID_PROBLEM_VIEW);
        bottomCenter.addView(CONTROL_PANEL_VIEW_ID);

        // right center
        IFolderLayout rightCenter = pageLayout.createFolder(RIGHT_CENTER, IPageLayout.RIGHT, 0.75f, pageLayout.getEditorArea());
        rightCenter.addView(IPageLayout.ID_OUTLINE);

        // right bottom
        IFolderLayout rightBottom = pageLayout.createFolder(RIGHT_BOTTOM, IPageLayout.RIGHT, 0.70f, BOTTOM_CENTER);
        rightBottom.addView(IConsoleConstants.ID_CONSOLE_VIEW);
    }
}
