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
import org.erlide.backend.IBackend;
import org.erlide.core.MessageReporter;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.services.builder.BuildResource;
import org.erlide.core.services.builder.BuilderHelper;
import org.erlide.core.services.builder.CompilerOptions;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.osgi.service.prefs.BackingStoreException;

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
        final ErlangEditor editor = (ErlangEditor) getSite().getPage()
                .getActiveEditor();
        final IErlModule module = editor.getModule();
        if (module == null) {
            return;
        }
        final IBackend b = BackendCore.getBackendManager().getIdeBackend();

        final IResource resource = module.getResource();
        final IProject project = resource.getProject();
        final BuildResource bres = new BuildResource(resource);
        final CompilerOptions prefs = new CompilerOptions(project);
        try {
            prefs.load();
        } catch (final BackingStoreException e1) {
            e1.printStackTrace();
        }
        final OtpErlangList compilerOptions = prefs.export();
        final IErlProject erlProject = ErlModelManager.getErlangModel()
                .getErlangProject(project);

        if ("erl".equals(resource.getFileExtension())) {
            helper.compileErl(project, bres, erlProject.getOutputLocation()
                    .toString(), b, compilerOptions);
        }
        if ("yrl".equals(resource.getFileExtension())) {
            helper.compileYrl(project, bres, b, compilerOptions);
        }
        MessageReporter.showInfo(String.format("File '%s' was compiled.",
                resource.getName()));
    }

    public IWorkbenchSite getSite() {
        return site;
    }
}
