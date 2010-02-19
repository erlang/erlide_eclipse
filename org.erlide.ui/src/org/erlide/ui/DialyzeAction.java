package org.erlide.ui;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFolder;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;

import erlang.ErlideDialyze;

public class DialyzeAction implements IObjectActionDelegate {

	private final Map<IErlProject, Set<IErlModule>> modules;

	public DialyzeAction() {
		modules = new HashMap<IErlProject, Set<IErlModule>>();
	}

	public void setActivePart(final IAction action,
			final IWorkbenchPart targetPart) {
		// TODO Auto-generated method stub

	}

	public static final String DIALYZE_WARNING_MARKER = ErlangPlugin.PLUGIN_ID
			+ ".dialyzewarningmarker";

	public void run(final IAction action) {
		final String plt = System.getProperty("erlide.plt");
		new IRunnableWithProgress() {

			public void run(final IProgressMonitor monitor)
					throws InvocationTargetException, InterruptedException {
				for (final IErlProject p : modules.keySet()) {
					final IProject project = p.getProject();
					Backend backend;
					try {
						backend = ErlangCore.getBackendManager()
								.getBuildBackend(project);
						final List<String> files = new ArrayList<String>();
						for (final IErlModule m : modules.get(p)) {
							files.add(m.getResource().getFullPath()
									.toPortableString());
						}
						final OtpErlangObject result = ErlideDialyze.dialyze(
								backend, files, plt);
						addDialyzeWarningMarkersFromResult(result);
					} catch (final BackendException e) {
						ErlLogger.error(e);
					}
				}
			}
		};
	}

	private void addDialyzeWarningMarkersFromResult(final OtpErlangObject result) {
		;
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
