package org.erlide.ui.editors.erl.correction;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.ModelUtilService;
import org.erlide.util.ErlLogger;

public class CreateHeaderQuickFix extends ErlangQuickFix {

    private static final String INCLUDE_NO_HEADER_LABEL = "create header file \"%s\"";
    private static final String INCLUDE_NO_HEADER_DESCRIPTION = "description...";
    private final IErlModule module;
    private final String name;

    public CreateHeaderQuickFix(final IErlModule module, final String name) {
        super(String.format(INCLUDE_NO_HEADER_LABEL, name),
                INCLUDE_NO_HEADER_DESCRIPTION, null);
        this.module = module;
        this.name = name;
    }

    @Override
    public void run(final IMarker marker) {
        final ModelUtilService svc = ErlangEngine.getInstance().getModelUtilService();
        final IErlProject project = svc.getProject(module);

        // TODO what if there are multiple include dirs?

        final IPath inc = project.getProperties().getIncludeDirs().iterator().next();
        final IProject wproject = project.getWorkspaceProject();
        final IFile header = wproject.getFolder(inc).getFile(name);
        try {
            header.create(new EmptyInputStream(), true, null);
            wproject.refreshLocal(IResource.DEPTH_INFINITE, null);

            final IFile fileToOpen = header;
            final IWorkbenchPage page = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getActivePage();
            IDE.openEditor(page, fileToOpen);
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }

    }
}
