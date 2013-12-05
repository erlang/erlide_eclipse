package org.erlide.ui.editors.scratchpad;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.Charset;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

public class NewErlangScratchPadWizard extends Wizard implements INewWizard {

    private IStructuredSelection fSelection;
    private NewErlangScratchPadWizardPage fPage;

    @Override
    public void addPages() {
        fPage = new NewErlangScratchPadWizardPage(fSelection);
        addPage(fPage);
    }

    // FIXME copied a lot from ErlangFileWizard, we should refactor to common
    // base class...

    @Override
    public boolean performFinish() {
        final IPath containerFullPath = fPage.getContainerFullPath();
        final String fileName = fPage.getFileName();
        final IRunnableWithProgress op = new IRunnableWithProgress() {

            @Override
            public void run(final IProgressMonitor monitor)
                    throws InvocationTargetException {
                try {
                    doFinish(containerFullPath, fileName, monitor);
                } catch (final CoreException e) {
                    throw new InvocationTargetException(e);
                } finally {
                    monitor.done();
                }
            }
        };
        try {
            getContainer().run(true, false, op);
        } catch (final InterruptedException e) {
            return false;
        } catch (final InvocationTargetException e) {
            final Throwable realException = e.getTargetException();
            MessageDialog.openError(getShell(), "Error", realException.getMessage());
            return false;
        }
        return true;
    }

    /**
     * The worker method. It will find the container, create the file if missing
     * or just replace its contents, and open the editor on the newly created
     * file.
     */
    public void doFinish(final IPath containerFullPath, final String fileName,
            final IProgressMonitor monitor) throws CoreException {

        // ErlLogger.debug("Generating a file with skeleton: "+skeleton);

        // create a sample file
        monitor.beginTask("Creating " + fileName, 2);
        final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        final IResource resource = root.findMember(containerFullPath);
        if (!resource.exists() || !(resource instanceof IContainer)) {
            throwCoreException("Container \"" + containerFullPath + "\" does not exist.");
        }
        final IContainer container = (IContainer) resource;
        IPath path = new Path(fileName);
        final String ext = path.getFileExtension();
        if (!"erlScratchPad".equals(ext)) {
            path = path.addFileExtension("erlScratchPad");
        }
        final IFile file = container.getFile(path);
        try {
            final InputStream stream = openContentStream();
            if (file.exists()) {
                file.setContents(stream, true, true, monitor);
            } else {
                file.create(stream, true, monitor);
            }
            stream.close();
        } catch (final IOException e) {
        }
        monitor.worked(1);

        // ErlangCore.getModelManager().create(file, null);

        monitor.setTaskName("Opening file for editing...");
        getShell().getDisplay().asyncExec(new Runnable() {

            @Override
            public void run() {
                final IWorkbenchPage page = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage();
                try {
                    IDE.openEditor(page, file, true);
                } catch (final PartInitException e) {
                }
            }
        });
        monitor.worked(1);
    }

    /**
     * We will initialize file contents with a sample text.
     */

    private InputStream openContentStream() {
        final String s = "%% This is a scratch pad for running erlang code\n%% Everything is saved";
        return new ByteArrayInputStream(s.getBytes(Charset.forName("UTF8")));
    }

    private void throwCoreException(final String message) throws CoreException {
        final IStatus status = new Status(IStatus.ERROR, "Erlang_Wizard", IStatus.OK,
                message, null);
        throw new CoreException(status);
    }

    @Override
    public void init(final IWorkbench workbench, final IStructuredSelection selection) {
        fSelection = selection;
        setNeedsProgressMonitor(true);
    }

}
