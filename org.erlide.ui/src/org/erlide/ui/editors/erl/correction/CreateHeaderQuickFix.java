package org.erlide.ui.editors.erl.correction;

import java.io.IOException;
import java.util.Collection;
import java.util.Iterator;
import java.util.regex.Pattern;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
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
import org.erlide.engine.services.correction.MessageMatcher;
import org.erlide.engine.services.search.ModelUtilService;
import org.erlide.util.ErlLogger;

public class CreateHeaderQuickFix extends ErlangQuickFix {

    private static final Pattern PATTERN = Pattern
            .compile("can't find include file \"(.+?)\"");
    private static final String LABEL = "Create header file \"%s\"";
    private static final String DESCRIPTION = "description...";

    private final IErlModule module;
    private final String name;

    public CreateHeaderQuickFix(final IErlModule module, final Collection<String> matches) {
        super(String.format(LABEL, matches.iterator().next()), DESCRIPTION, null);
        this.module = module;
        this.name = matches.iterator().next();
    }

    @Override
    public void run(final IMarker marker) {
        final ModelUtilService svc = ErlangEngine.getInstance().getModelUtilService();
        final IErlProject project = svc.getProject(module);

        final Iterator<IPath> iterator = project.getProperties().getIncludeDirs()
                .iterator();
        final IPath inc;
        if (!iterator.hasNext()) {
            inc = module.getResource().getParent().getProjectRelativePath();
        } else {
            inc = iterator.next();
            // TODO what if there are multiple include dirs?
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

    public static Collection<String> matches(final String message) {
        final MessageMatcher matcher = new MessageMatcher();
        final Collection<String> match = matcher.matchMessage(message, PATTERN);
        return match;
    }
}
