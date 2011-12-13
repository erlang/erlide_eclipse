package org.erlide.ui.handlers;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.commands.IHandlerListener;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.dialogs.OpenModuleDialog;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;

public final class OpenModuleHandler extends Action implements IHandler,
        IWorkbenchWindowActionDelegate {

    /**
     * The identifier of the parameter storing the file path.
     */
    private static final String PARAM_ID_FILE_PATH = "filePath"; //$NON-NLS-1$

    /**
     * A collection of objects listening to changes to this manager. This
     * collection is <code>null</code> if there are no listeners.
     */
    private transient ListenerList listenerList = null;

    /**
     * Creates a new instance of the class.
     */
    public OpenModuleHandler() {
        super();
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(this, IErlangHelpContextIds.OPEN_MODULE_ACTION);
    }

    @Override
    public final void addHandlerListener(final IHandlerListener listener) {
        if (listenerList == null) {
            listenerList = new ListenerList(ListenerList.IDENTITY);
        }

        listenerList.add(listener);
    }

    @Override
    public final void dispose() {
        listenerList = null;
    }

    @Override
    public final Object execute(final ExecutionEvent event)
            throws ExecutionException {
        final List<IFile> files = new ArrayList<IFile>();

        if (event.getParameter(PARAM_ID_FILE_PATH) == null) {
            // Prompt the user for the resource to open.
            final Object[] result = queryFileResource();
            promptForFiles(files, result);

        } else {
            // Use the given parameter.
            final IResource resource = (IResource) event
                    .getObjectParameterForExecution(PARAM_ID_FILE_PATH);
            if (!(resource instanceof IFile)) {
                throw new ExecutionException(
                        "filePath parameter must identify a file"); //$NON-NLS-1$
            }
            files.add((IFile) resource);
        }

        if (files.size() > 0) {

            final IWorkbenchWindow window = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow();
            if (window == null) {
                throw new ExecutionException("no active workbench window"); //$NON-NLS-1$
            }

            final IWorkbenchPage page = window.getActivePage();
            if (page == null) {
                throw new ExecutionException("no active workbench page"); //$NON-NLS-1$
            }

            try {
                for (final IFile file : files) {
                    IDE.openEditor(page, file, true);
                }
            } catch (final PartInitException e) {
                throw new ExecutionException("error opening file in editor", e); //$NON-NLS-1$
            }
        }

        return null;
    }

    private void promptForFiles(final List<IFile> files, final Object[] result) {
        if (result != null) {
            for (int i = 0; i < result.length; i++) {
                if (result[i] instanceof IFile) {
                    files.add((IFile) result[i]);
                } else if (result[i] instanceof String) {
                    try {
                        final String path = (String) result[i];
                        final IFile[] cons = ResourcesPlugin.getWorkspace()
                                .getRoot()
                                .findFilesForLocationURI(new URI(path));
                        for (final IFile con : cons) {
                            if (cons.length == 1) {
                                files.add(con);
                            }
                        }
                        if (files.size() == 0) {
                            // final IFile file = ModelUtils.openExternal(null,
                            // path);
                            // files.add(file);
                        }
                        // } catch (final CoreException e) {
                    } catch (final URISyntaxException e) {
                        ErlLogger.error(e);
                    }
                }
            }
        }
    }

    @Override
    public final void init(final IWorkbenchWindow window) {
        // Do nothing.
    }

    /**
     * Query the user for the resources that should be opened
     * 
     * @return the resource that should be opened.
     */
    private final Object[] queryFileResource() {
        final IWorkbenchWindow window = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow();
        if (window == null) {
            return null;
        }
        final Shell parent = window.getShell();
        final IContainer input = ResourcesPlugin.getWorkspace().getRoot();

        final OpenModuleDialog dialog = new OpenModuleDialog(parent, input);
        final int resultCode = dialog.open();
        if (resultCode != IDialogConstants.OK_ID) {
            return null;
        }

        final Object[] result = dialog.getResult();
        return result;
    }

    @Override
    public final void removeHandlerListener(final IHandlerListener listener) {
        if (listenerList != null) {
            listenerList.remove(listener);

            if (listenerList.isEmpty()) {
                listenerList = null;
            }
        }
    }

    @Override
    public final void run(final IAction action) {
        try {
            execute(new ExecutionEvent());
        } catch (final ExecutionException e) {
            // Do nothing. Maybe show error message?
        }
    }

    @Override
    public final void selectionChanged(final IAction action,
            final ISelection selection) {
        // Do nothing.
    }
}
