package org.erlide.ui.editors.erl.correction;

import java.util.Collection;
import java.util.regex.Pattern;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.ui.IMarkerResolution;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.services.correction.MessageMatcher;

//TODO make this an engine service?

public class ErlangQuickFixCollector {

    private static final Pattern INCLUDE_NO_HEADER_PATTERN = Pattern
            .compile("can't find include file \"(.+?)\"");

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
        final MessageMatcher matcher = new MessageMatcher();

        return new IMarkerResolution[0];
    }

    private IMarkerResolution[] getFixesForModule(final IErlModule module,
            final int line, final String message) {

        // TODO

        final MessageMatcher matcher = new MessageMatcher();
        final Collection<String> match = matcher.matchMessage(message,
                INCLUDE_NO_HEADER_PATTERN);
        if (match != null) {
            final ErlangQuickFix fix = new CreateHeaderQuickFix(module, match.iterator()
                    .next());
            return new IMarkerResolution[] { fix };
        }

        return new IMarkerResolution[0];
    }

}
