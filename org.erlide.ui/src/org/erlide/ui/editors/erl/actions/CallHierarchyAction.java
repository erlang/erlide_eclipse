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
package org.erlide.ui.editors.erl.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IErlModule;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.rpc.RpcFuture;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.jinterface.AsyncCaller;
import org.erlide.ui.views.CallHierarchyView;

import erlang.ErlangXref;
import erlang.FunctionRef;

public class CallHierarchyAction extends Action {

    private final ErlangEditor editor;
    IErlModule module;

    public CallHierarchyAction(final ErlangEditor erlangEditor,
            final IErlModule module) {
        super("Call hierarchy");
        editor = erlangEditor;
        this.module = module;
    }

    @Override
    public void run() {
        if (module == null) {
            return;
        }
        final IErlElement el = editor.getElementAt(editor.getViewer()
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
        final int i = name.lastIndexOf('.');
        if (i > 0) {
            name = name.substring(0, i);
        }
        final FunctionRef ref = new FunctionRef(name, f.getFunctionName(),
                f.getArity());

        final IWorkbenchWindow dw = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow();
        final IWorkbenchPage page = dw.getActivePage();

        final AsyncCaller<CallHierarchyView> ac = new AsyncCaller<CallHierarchyView>(
                100) {

            @Override
            protected CallHierarchyView prepare() {
                try {
                    final IViewPart p = page
                            .showView("org.erlide.ui.callhierarchy");
                    final CallHierarchyView cvh = (CallHierarchyView) p
                            .getAdapter(CallHierarchyView.class);

                    cvh.setMessage("<searching... project "
                            + module.getProject().getName() + ">");
                    return cvh;
                } catch (final PartInitException e) {
                    ErlLogger.error("could not open Call hierarchy view: ",
                            e.getMessage());
                    return null;
                }
            }

            @Override
            protected RpcFuture call() throws BackendException {
                final Backend b = ErlangCore.getBackendManager()
                        .getIdeBackend();
                final RpcFuture result = ErlangXref.addProject(b,
                        module.getProject());
                return result;
            }

            @Override
            protected void handleResult(final CallHierarchyView context,
                    final RpcFuture result) {
                page.activate(context);
                context.setRoot(module.getModel().findFunction(ref));
            }
        };
        ac.run();
    }
}
