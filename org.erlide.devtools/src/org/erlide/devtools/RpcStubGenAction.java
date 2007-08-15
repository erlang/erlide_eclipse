package org.erlide.devtools;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.erlide.jinterface.StubGenerator;

public class RpcStubGenAction implements IWorkbenchWindowActionDelegate {

	private TextSelection fSelection;

	public void dispose() {
	}

	public void init(IWorkbenchWindow window) {
	}

	public void run(IAction action) {
		if (fSelection == null) {
			return;
		}
		System.out.println("@@" + fSelection.getText());

		Class target;
		try {
			target = Class.forName(fSelection.getText());

			String module = StubGenerator.module(target);
			System.out.println("generate stub!!! " + module);

			// StubGenerator.tofile(target, null);

			System.out.println(StubGenerator.generate(target));

		} catch (ClassNotFoundException e) {
			System.out.format("class %s not found", fSelection.getText());
		}

	}

	public void selectionChanged(IAction action, ISelection selection) {
		if (selection instanceof TextSelection) {
			fSelection = (TextSelection) selection;
		}
	}

}
