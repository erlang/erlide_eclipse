package org.erlide.ui;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFolder;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;

public class DialyzeAction implements IObjectActionDelegate {

	private final List<IErlModule> modules;

	public DialyzeAction() {
		modules = new ArrayList<IErlModule>();
	}

	public void setActivePart(final IAction action,
			final IWorkbenchPart targetPart) {
		// TODO Auto-generated method stub

	}

	public void run(final IAction action) {
		final StringBuilder b = new StringBuilder();
		for (final IErlModule i : modules) {
			b.append(i.getName()).append("  ");
		}
		MessageDialog.openConfirm(null, "Modules", b.toString());
	}

	public void selectionChanged(final IAction action,
			final ISelection selection) {
		modules.clear();
		if (selection instanceof IStructuredSelection) {
			final IErlModel model = ErlangCore.getModel();
			final IStructuredSelection ss = (IStructuredSelection) selection;
			for (final Object i : ss.toList()) {
				try {
					model.open(null);
					if (i instanceof IResource) {
						final IResource r = (IResource) i;
						final IErlElement e = model.findElement(r, true);
						if (e instanceof IErlFolder) {
							final IErlFolder f = (IErlFolder) e;
							modules.addAll(f.getModules());
						} else if (e instanceof IErlModule) {
							final IErlModule m = (IErlModule) e;
							modules.add(m);
						}
					}
				} catch (final ErlModelException e) {
					e.printStackTrace();
				}
			}
		}
		action.setEnabled(!modules.isEmpty());
	}

}
