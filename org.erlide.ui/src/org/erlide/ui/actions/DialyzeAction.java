package org.erlide.ui.actions;

import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.erlide.core.builder.DialyzerPreferences;
import org.erlide.core.builder.DialyzerUtils;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.jinterface.util.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;

public class DialyzeAction implements IObjectActionDelegate {

	private final class DialyzeOperation implements IRunnableWithProgress {

		public void run(final IProgressMonitor monitor)
				throws InvocationTargetException, InterruptedException {
			final Set<IErlProject> keySet = modules.keySet();
			monitor.beginTask("Dialyzing", keySet.size());
			try {
				prefs.load();
			} catch (final BackingStoreException e) {
				ErlLogger.error(e);
			}
			DialyzerUtils.doDialyze(monitor, modules, prefs);
		}
	}

	private final Map<IErlProject, Set<IErlModule>> modules;
	private Shell shell;
	private final DialyzerPreferences prefs;

	public DialyzeAction() {
		modules = new HashMap<IErlProject, Set<IErlModule>>();
		prefs = new DialyzerPreferences();
	}

	public void setActivePart(final IAction action,
			final IWorkbenchPart targetPart) {
		shell = targetPart.getSite().getShell();
	}

	public void run(final IAction action) {
		final IRunnableWithProgress op = new DialyzeOperation();
		try {
			final IWorkbench wb = PlatformUI.getWorkbench();
			wb.getProgressService().run(true, true, op);
		} catch (final InvocationTargetException e) {
			final Throwable t = e.getCause();
			Display.getDefault().asyncExec(new Runnable() {
				public void run() {
					MessageDialog.openError(getShell(), "Dialyzer error", t
							.getMessage());
				}
			});
		} catch (final InterruptedException e) {
			ErlLogger.error(e);
		}

	}

	private Shell getShell() {
		return shell;
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
						DialyzerUtils.addModulesFromResource(model,
								(IResource) i, modules);
					}
				} catch (final ErlModelException e) {
					e.printStackTrace();
				}
			}
		}
		action.setEnabled(!modules.isEmpty());
	}
}
