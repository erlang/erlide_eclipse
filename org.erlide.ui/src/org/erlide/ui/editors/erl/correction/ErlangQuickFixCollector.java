package org.erlide.ui.editors.erl.correction;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.ui.IMarkerResolution;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.erlang.IErlModule;

public class ErlangQuickFixCollector {

    public IMarkerResolution[] getFixes(final IResource resource, final int line,
            final String message) {
        final IErlModel model = ErlangEngine.getInstance().getModel();
        IErlModule module;

        if (resource == null) {
            return new IMarkerResolution[0];
        }
        if (resource instanceof IProject) {
            return getFixesForProject((IProject) resource, line, message);

        }
        if (resource instanceof IFile) {
            module = model.findModule((IFile) resource);
            if (module != null) {
                return getFixesForModule(module, line, message);
            }
        }

        return new IMarkerResolution[0];
    }

    private IMarkerResolution[] getFixesForProject(final IProject project,
            final int line, final String message) {

        // TODO

        return new IMarkerResolution[] { new ErlangQuickFix(message,
                "PROJECT some<br>description <b>here</b>!", null) };
    }

    private IMarkerResolution[] getFixesForModule(final IErlModule module,
            final int line, final String message) {

        // TODO

        return new IMarkerResolution[] { new ErlangQuickFix(message + " @" + line,
                "MODULE some<br>description <b>here</b>!", null) };
    }

}
