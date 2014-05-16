package org.erlide.ui.editors.erl.correction.fixes;

import java.io.IOException;
import java.util.Iterator;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.ModelUtilService;
import org.erlide.ui.editors.erl.correction.MarkerQuickFixExecutor;
import org.erlide.util.ErlLogger;

public class CreateHeaderQuickFix extends MarkerQuickFixExecutor {

    @Override
    public void run() {
        final ModelUtilService svc = ErlangEngine.getInstance().getModelUtilService();

        final String name = getQuickFix().getArgs().get(0);
        final IErlProject project = svc.getProject(module);

        final Iterator<IPath> iterator = project.getProperties().getIncludeDirs()
                .iterator();
        final IPath inc;
        if (!iterator.hasNext()) {
            inc = module.getResource().getParent().getProjectRelativePath();
        } else {
            inc = iterator.next();
            // if there are multiple include dirs, let user move file manually
        }
        final IProject wproject = project.getWorkspaceProject();
        final IFolder folder = wproject.getFolder(inc);
        final IFile header = folder.getFile(name);
        try {
            final EmptyInputStream source = new EmptyInputStream();
            try {
                header.create(source, true, null);
            } finally {
                try {
                    source.close();
                } catch (final IOException e) {
                }
            }
            folder.refreshLocal(IResource.DEPTH_ONE, null);

            final IFile fileToOpen = header;
            final IWorkbenchPage page = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getActivePage();
            IDE.openEditor(page, fileToOpen);
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }

    }
}
