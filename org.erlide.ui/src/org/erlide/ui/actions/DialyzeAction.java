package org.erlide.ui.actions;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
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
import org.erlide.core.builder.DialyzerUtils;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFolder;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideDialyze;

public class DialyzeAction implements IObjectActionDelegate {

	private final Map<IErlProject, Set<IErlModule>> modules;
	private Shell shell;

	public DialyzeAction() {
		modules = new HashMap<IErlProject, Set<IErlModule>>();
	}

	public void setActivePart(final IAction action,
			final IWorkbenchPart targetPart) {
		shell = targetPart.getSite().getShell();

	}

	public void run(final IAction action) {
		final String plt = System.getProperty("erlide.plt");
		final IRunnableWithProgress op = new IRunnableWithProgress() {

			public void run(final IProgressMonitor monitor)
					throws InvocationTargetException, InterruptedException {
				final Set<IErlProject> keySet = modules.keySet();
				monitor.beginTask("Dialyzing", keySet.size());
				for (final IErlProject p : keySet) {
					monitor.subTask("Dialyzing " + p.getName());
					final IProject project = p.getProject();
					DialyzerUtils.removeDialyzerWarningMarkers(p);
					try {
						final Backend backend = ErlangCore.getBackendManager()
								.getBuildBackend(project);
						final List<String> files = new ArrayList<String>();
						for (final IErlModule m : modules.get(p)) {
							files.add(m.getResource().getLocation()
									.toPortableString());
						}
						final List<String> includeDirs = new ArrayList<String>();
						for (final String i : p.getProperties()
								.getIncludeDirs()) {
							final IPath path = new Path(i);
							project.getFile(path);
							final String s = project.getLocation().append(i)
									.toPortableString();
							includeDirs.add(s);
						}
						final OtpErlangObject result = ErlideDialyze.dialyze(
								backend, files, plt, includeDirs);
						if (dialyzeError(result)) {
							break;
						}
						DialyzerUtils.addDialyzeWarningMarkersFromResult(p,
								backend, (OtpErlangList) result);
					} catch (final BackendException e) {
						ErlLogger.error(e);
					}
					monitor.worked(1);
				}
			}
		};
		try {
			final IWorkbench wb = PlatformUI.getWorkbench();
			wb.getProgressService().run(true, true, op);
		} catch (final InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		// for (final IErlProject p : modules.keySet()) {
		// final IProject project = p.getProject();
		// Backend backend;
		// try {
		// backend = ErlangCore.getBackendManager().getBuildBackend(
		// project);
		// final List<String> files = new ArrayList<String>();
		// for (final IErlModule m : modules.get(p)) {
		// files.add(m.getResource().getLocation().toPortableString());
		// }
		// final List<String> includeDirs = new ArrayList<String>();
		// for (final String i : p.getProperties().getIncludeDirs()) {
		// final String s = project.getLocation().append(i)
		// .toPortableString();
		// includeDirs.add(s);
		// }
		// final OtpErlangObject result = ErlideDialyze.dialyze(backend,
		// files, plt, includeDirs);
		// addDialyzeWarningMarkersFromResult(p, result);
		// } catch (final BackendException e) {
		// ErlLogger.error(e);
		// }
		// }

	}

	protected boolean dialyzeError(final OtpErlangObject result) {
		if (result instanceof OtpErlangTuple) {
			OtpErlangTuple t = (OtpErlangTuple) result;
			if (t.elementAt(1) instanceof OtpErlangTuple) {
				t = (OtpErlangTuple) t.elementAt(1);
			}
			final String s = Util.stringValue(t.elementAt(1));
			Display.getDefault().asyncExec(new Runnable() {

				public void run() {
					MessageDialog.openError(getShell(), "Dialyzer error", s);
				}
			});
			return true;
		}
		return false;
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
						final IResource r = (IResource) i;
						final IErlElement e = model.findElement(r, true);
						if (e instanceof IErlFolder) {
							final IErlFolder f = (IErlFolder) e;
							final IErlProject p = f.getErlProject();
							Set<IErlModule> ms = modules.get(p);
							if (ms == null) {
								ms = new HashSet<IErlModule>();
							}
							ms.addAll(f.getModules());
							modules.put(p, ms);
						} else if (e instanceof IErlModule) {
							final IErlModule m = (IErlModule) e;
							final IErlProject p = m.getErlProject();
							Set<IErlModule> ms = modules.get(p);
							if (ms == null) {
								ms = new HashSet<IErlModule>();
							}
							ms.add(m);
							modules.put(p, ms);
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
