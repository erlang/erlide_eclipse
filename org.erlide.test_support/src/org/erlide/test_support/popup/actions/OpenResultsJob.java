/**
 * 
 */
package org.erlide.test_support.popup.actions;

import java.net.MalformedURLException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.progress.UIJob;

public class OpenResultsJob extends UIJob {
    private final IFile report;
    private final IFile trace;

    public OpenResultsJob(final String name, final IFile report,
            final IFile trace) {
        super(name);
        this.report = report;
        this.trace = trace;
    }

    @Override
    public IStatus runInUIThread(final IProgressMonitor mon) {
        openInBrowser(report);
        openInEditor(trace);
        return Status.OK_STATUS;
    }

    private void openInEditor(final IFile file) {
        if (file == null) {
            return;
        }
        try {
            file.refreshLocal(IResource.DEPTH_ZERO, null);
        } catch (final CoreException e) {
        }

        final IWorkbench wbench = PlatformUI.getWorkbench();
        final IWorkbenchPage page = wbench.getActiveWorkbenchWindow()
                .getActivePage();
        final IEditorDescriptor desc = PlatformUI.getWorkbench()
                .getEditorRegistry().getDefaultEditor(file.getName());
        try {
            page.openEditor(new FileEditorInput(file), desc.getId());
        } catch (final PartInitException e) {
            e.printStackTrace();
        }
    }

    private void openInBrowser(final IFile file) {
        if (file == null) {
            return;
        }
        final IWorkbench wbench = PlatformUI.getWorkbench();
        final IWorkbenchBrowserSupport browserSupport = wbench
                .getBrowserSupport();
        try {
            file.refreshLocal(IResource.DEPTH_ZERO, null);
        } catch (final CoreException e1) {
        }
        if (file.exists() && file.isAccessible()) {
            try {
                final int style = IWorkbenchBrowserSupport.NAVIGATION_BAR
                        | IWorkbenchBrowserSupport.AS_EDITOR;
                final String name = file.getName();
                final String browserId = getName() + "_" + name;
                IWebBrowser browser;
                browser = browserSupport.createBrowser(style, browserId, name,
                        null);
                try {
                    browser.openURL(file.getRawLocationURI().toURL());
                } catch (final MalformedURLException e) {
                    // should not happen
                    e.printStackTrace();
                }
            } catch (final PartInitException e) {
                e.printStackTrace();
            }
        }
    }

}
