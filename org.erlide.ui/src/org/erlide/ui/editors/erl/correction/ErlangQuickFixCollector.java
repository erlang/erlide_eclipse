package org.erlide.ui.editors.erl.correction;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.ui.IMarkerResolution;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.erlang.IErlModule;

import com.google.common.collect.Lists;

//TODO make this an engine service?

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

        // TODO handle multiple project quickfixes

        return new IMarkerResolution[0];
    }

    private IMarkerResolution[] getFixesForModule(final IErlModule module,
            final int line, final String message) {

        // TODO handle multiple quickfixes

        final List<IMarkerResolution> result = Lists.newArrayList();
        final Collection<String> match = CreateHeaderQuickFix.matches(message);
        if (match != null) {
            final ErlangQuickFix fix = new CreateHeaderQuickFix(module, match);
            result.add(fix);
        }

        return result.toArray(new IMarkerResolution[result.size()]);
    }
}
