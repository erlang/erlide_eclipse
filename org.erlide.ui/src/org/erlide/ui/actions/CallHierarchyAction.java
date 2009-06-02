/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.actions;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IErlModule;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcFuture;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.views.CallHierarchyView;

import erlang.ErlangXref;
import erlang.FunctionRef;

public class CallHierarchyAction extends Action {

	private ErlangEditor editor;
	private IErlModule module;

	public CallHierarchyAction(ErlangEditor erlangEditor, IErlModule module) {
		super("Call hierarchy");
		editor = erlangEditor;
		this.module = module;
	}

	@Override
	public void run() {
		IErlElement el = editor.getElementAt(editor.getViewer()
				.getSelectedRange().x, false);
		IErlFunction f = null;
		if (el instanceof IErlFunction) {
			f = (IErlFunction) el;
		} else if (el instanceof IErlFunctionClause) {
			f = (IErlFunction) el.getParent();
		}
		if (f == null) {
			return;
		}
		String name = module.getName();
		int i = name.lastIndexOf('.');
		if (i > 0) {
			name = name.substring(0, i);
		}
		final FunctionRef ref = new FunctionRef(name, f.getFunctionName(), f
				.getArity());

		IWorkbenchWindow dw = PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow();
		final IWorkbenchPage page = dw.getActivePage();

		Backend b = ErlangCore.getBackendManager().getIdeBackend();
		try {
			IViewPart p = page.showView("org.erlide.ui.callhierarchy");
			CallHierarchyView cvh = (CallHierarchyView) p
					.getAdapter(CallHierarchyView.class);

			cvh.setMessage("<computing... project "
					+ module.getProject().getName() + ">");
		} catch (PartInitException e) {
			ErlLogger.error("could not open Call hierarchy view: ", e
					.getMessage());
			return;
		}
		final RpcFuture result = ErlangXref.addProject(b, module.getProject());

		Job job = new UIJob("call hierarchy updater") {
			@Override
			public IStatus runInUIThread(IProgressMonitor monitor) {
				try {
					if (result.get(1) == null) {
						schedule(100);
					}
				} catch (RpcException e1) {
					e1.printStackTrace();
				}
				IViewPart p;
				try {
					p = page.showView("org.erlide.ui.callhierarchy");
					final CallHierarchyView cvh = (CallHierarchyView) p
							.getAdapter(CallHierarchyView.class);
					cvh.setRoot(module.getModel().findFunction(ref));
				} catch (PartInitException e) {
					e.printStackTrace();
				}
				return new Status(IStatus.OK, ErlideUIPlugin.PLUGIN_ID, "done");
			}
		};
		job.schedule(100);
	}
}
