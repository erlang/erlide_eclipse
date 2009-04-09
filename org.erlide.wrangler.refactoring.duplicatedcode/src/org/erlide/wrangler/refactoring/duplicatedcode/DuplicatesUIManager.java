package org.erlide.wrangler.refactoring.duplicatedcode;

import java.util.List;

import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.IDuplicatedCodeResultDisplayer;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;

public class DuplicatesUIManager {
	// private static List<DuplicatedCode> result;
	private static IDuplicatedCodeResultDisplayer dupDisplayer;

	public static void setDuplicatedCodeResultDisplayer(
			IDuplicatedCodeResultDisplayer displayer) {
		dupDisplayer = displayer;
	}

	public static void setRefactoringResults(List<DuplicatedCodeElement> root) {
		dupDisplayer.showResult(root);
	}

	public static void showDuplicatesView() {
		IWorkbench workbench = PlatformUI.getWorkbench();

		IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
		try {

			IViewPart view = window
					.getActivePage()
					.showView(
							"org.erlide.wrangler.refactoring.duplicatedcode.views.DuplicatedCodeView");

		} catch (PartInitException e) {
			e.printStackTrace();
		}
	}
}
