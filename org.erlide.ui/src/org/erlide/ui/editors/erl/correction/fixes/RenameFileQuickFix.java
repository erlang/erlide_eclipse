package org.erlide.ui.editors.erl.correction.fixes;

import java.util.List;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.dialogs.MessageDialog;
import org.erlide.ui.editors.erl.correction.MarkerQuickFixExecutor;

public class RenameFileQuickFix extends MarkerQuickFixExecutor {

    @Override
    public void run() throws CoreException {
        final IMarker marker = getMarker();
        final List<String> margs = getQuickFix().getArgs();
        final IResource file = marker.getResource();
        final IPath path = file.getFullPath();
        final IPath newPath = path.removeLastSegments(1).append(margs.get(0) + ".erl");
        file.move(newPath, true, null);
    }

    @Override
    public void handleException(final Throwable exception) {
        if (exception instanceof CoreException) {
            MessageDialog.openInformation(null, "Rename file quickfix",
                    "Could not finish action due to error: " + exception.getMessage());
        }
        super.handleException(exception);
    }
}
