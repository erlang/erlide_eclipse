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
import org.erlide.backend.api.BackendException;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.FunctionRef;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlFunctionClause;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.services.search.XrefService;
import org.erlide.runtime.rpc.RpcFuture;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.jinterface.AsyncCaller;
import org.erlide.ui.views.CallHierarchyView;
import org.erlide.util.ErlLogger;

public class CallHierarchyAction extends Action {

    private final ErlangEditor editor;
    IErlModule module;
    private final XrefService xrefService;

    public CallHierarchyAction(final ErlangEditor erlangEditor, final IErlModule module,
            final XrefService xrefService) {
        super("Call hierarchy");
        editor = erlangEditor;
        this.module = module;
        this.xrefService = xrefService;
    }

    @Override
    public void run() {
        if (module == null) {
            return;
        }
        final IErlElement el = editor.getElementAt(
                editor.getViewer().getSelectedRange().x, false);
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
        final FunctionRef ref = new FunctionRef(name, f.getFunctionName(), f.getArity());

        final IWorkbenchWindow dw = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
        final IWorkbenchPage page = dw.getActivePage();

        final AsyncCaller<CallHierarchyView> ac = new AsyncCaller<CallHierarchyView>(100) {

            @Override
            protected CallHierarchyView prepare() {
                try {
                    final IViewPart p = page.showView("org.erlide.ui.callhierarchy");
                    final CallHierarchyView cvh = (CallHierarchyView) p
                            .getAdapter(CallHierarchyView.class);

                    cvh.setMessage("<searching... project "
                            + ErlangEngine.getInstance().getModelUtilService()
                                    .getProject(module).getName() + ">");
                    return cvh;
                } catch (final PartInitException e) {
                    ErlLogger.error("could not open Call hierarchy view: ",
                            e.getMessage());
                    return null;
                }
            }

            @Override
            protected RpcFuture call() throws BackendException {
                final RpcFuture result = xrefService.addProject(ErlangEngine
                        .getInstance().getModelUtilService().getProject(module));
                return result;
            }

            @Override
            protected void handleResult(final CallHierarchyView context,
                    final RpcFuture result) {
                page.activate(context);
                try {
                    context.setRoot(ErlangEngine.getInstance().getModel()
                            .findFunction(ref));
                } catch (final ErlModelException e) {
                    ErlLogger.error(e);
                }
            }
        };
        ac.run();
    }
}
