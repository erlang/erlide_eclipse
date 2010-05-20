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
	public static final String duplicatedView = "org.erlide.wrangler.refactoring.duplicatedcode.views.DuplicatedCodeView";
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

			@SuppressWarnings("unused")
			IViewPart view = window.getActivePage().showView(duplicatedView);

		} catch (PartInitException e) {
			e.printStackTrace();
		}
	}

	public static void closeDuplicatesView() {
		IWorkbench workbench = PlatformUI.getWorkbench();

		IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
		IViewPart view;
		try {
			view = window.getActivePage().showView(duplicatedView);
			window.getActivePage().hideView(view);
		} catch (PartInitException e) {
			e.printStackTrace();
		}

	}
}
