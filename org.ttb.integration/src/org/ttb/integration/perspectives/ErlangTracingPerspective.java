package org.ttb.integration.perspectives;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

/**
 * Perspective for tracing Erlang applications.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class ErlangTracingPerspective implements IPerspectiveFactory {

    public static final String VIEW_ID = "org.ttb.integration.views.ControlPanelView123";
    private static final String BOTTOM = "bottom";

    @Override
    public void createInitialLayout(IPageLayout pageLayout) {

        pageLayout.addView(IPageLayout.ID_OUTLINE, IPageLayout.LEFT, 0.30f, pageLayout.getEditorArea());
        // pageLayout.addView(IPageLayout.ID_OUTLINE, IPageLayout.LEFT, 0.30f,
        // VIEW_ID);

        IFolderLayout bot = pageLayout.createFolder(BOTTOM, IPageLayout.BOTTOM, 0.76f, pageLayout.getEditorArea());
        bot.addView(VIEW_ID);
    }
}
