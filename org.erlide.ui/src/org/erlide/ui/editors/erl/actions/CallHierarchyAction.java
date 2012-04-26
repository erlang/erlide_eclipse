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
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendException;
import org.erlide.backend.IBackend;
import org.erlide.core.model.erlang.FunctionRef;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.services.search.ErlangXref;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.IRpcFuture;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.jinterface.AsyncCaller;
import org.erlide.ui.views.CallHierarchyView;

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
            protected IRpcFuture call() throws BackendException {
                final IBackend b = BackendCore.getBackendManager()
                        .getIdeBackend();
                final IRpcFuture result = ErlangXref.addProject(b,
                        module.getProject());
                return result;
            }

            @Override
            protected void handleResult(final CallHierarchyView context,
                    final IRpcFuture result) {
                page.activate(context);
                try {
                    context.setRoot(module.getModel().findFunction(ref));
                } catch (final ErlModelException e) {
                    ErlLogger.error(e);
                }
            }
        };
        ac.run();
    }
}
