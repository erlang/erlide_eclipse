package org.erlide.wrangler.refactoring.codeinspection;

import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.erlide.wrangler.refactoring.codeinspection.view.GraphImageView;

/**
 * 
 * @author Gyorgy Orosz
 * 
 */
public class GraphViewManager {
	static public void setImage(String path) {
		GraphImageView view = (GraphImageView) showView();
		view.setImage(path);
	}

	/**
	 * Shows the Graph view.
	 * 
	 * @return view which is shown
	 */
	static public IViewPart showView() {

		IWorkbench workbench = PlatformUI.getWorkbench();

		IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
		try {
			IViewPart view = window
					.getActivePage()
					.showView(
							org.erlide.wrangler.refactoring.codeinspection.view.GraphImageView.VIEW_ID);
			return view;
		} catch (PartInitException e) {
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * Hides the graph view.
	 */
	static public void hideView() {
		IWorkbench workbench = PlatformUI.getWorkbench();

		IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
		IViewPart view;
		try {
			view = window
					.getActivePage()
					.showView(
							org.erlide.wrangler.refactoring.codeinspection.view.GraphImageView.VIEW_ID);
			window.getActivePage().hideView(view);
		} catch (PartInitException e) {
			e.printStackTrace();
		}

	}

}
