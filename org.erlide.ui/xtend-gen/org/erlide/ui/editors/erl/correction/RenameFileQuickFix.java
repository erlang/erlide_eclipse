package org.erlide.ui.editors.erl.correction;

import java.util.List;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

public class RenameFileQuickFix extends ErlangQuickFixRunnable {

    @Override
    public void run() throws CoreException {
        final IMarker marker = getMarker();
        final List<String> margs = getQuickFix().getArgs();
        final IResource file = marker.getResource();
        final IPath path = file.getFullPath();
        final IPath newPath = path.removeLastSegments(1).append(margs.get(0) + ".erl");
        file.move(newPath, true, null);
    }
}
