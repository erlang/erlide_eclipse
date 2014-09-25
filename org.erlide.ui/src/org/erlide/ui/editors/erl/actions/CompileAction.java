/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl.actions;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.action.Action;
import org.eclipse.ui.IWorkbenchSite;
import org.erlide.backend.BackendCore;
import org.erlide.core.builder.BuildResource;
import org.erlide.core.builder.BuilderHelper;
import org.erlide.core.builder.CompilerOptions;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.editors.erl.ErlEditorActionBarContributor;

import com.ericsson.otp.erlang.OtpErlangList;

public class CompileAction extends Action {

    private final IWorkbenchSite site;
    private final BuilderHelper helper = new BuilderHelper();

    public CompileAction(final IWorkbenchSite site) {
        super("Compile file");
        this.site = site;
    }

    @Override
    public void run() {
        final AbstractErlangEditor editor = (AbstractErlangEditor) getSite().getPage()
                .getActiveEditor();
        final IErlModule module = editor.getModule();
        if (module == null) {
            return;
        }

        final IResource resource = module.getResource();
        final IProject project = resource.getProject();
        if (project == null) {
            return;
        }
        final IErlProject eproject = ErlangEngine.getInstance().getModelUtilService()
                .getProject(module);
        if (eproject == null) {
            return;
        }
        final IOtpRpc b = BackendCore.getBuildBackend(eproject);

        final BuildResource bres = new BuildResource(resource);
        final CompilerOptions prefs = new CompilerOptions(project);
        prefs.load();
        final OtpErlangList compilerOptions = prefs.export();
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(project);

        if ("erl".equals(resource.getFileExtension())) {
            helper.compileErl(project, bres, erlProject.getProperties().getOutputDir()
                    .toString(), b, compilerOptions);
        }
        if ("yrl".equals(resource.getFileExtension())) {
            helper.compileYrl(project, bres, b, compilerOptions);
        }
        final ErlEditorActionBarContributor status = (ErlEditorActionBarContributor) editor
                .getEditorSite().getActionBarContributor();
        status.displayMessage(String.format("File '%s' was compiled.", resource.getName()));
    }

    public IWorkbenchSite getSite() {
        return site;
    }
}
