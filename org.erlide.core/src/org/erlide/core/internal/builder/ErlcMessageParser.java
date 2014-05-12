package org.erlide.core.internal.builder;

import java.util.Iterator;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.erlide.engine.model.builder.MarkerUtils;

import com.google.common.base.Splitter;

public class ErlcMessageParser implements IMessageParser {

    private final IProject project;

    public ErlcMessageParser(final IProject project) {
        this.project = project;
    }

    /**
     * Parses messages from erlc and creates the markers on the appropriate
     * resource. Returns true if any marker was created.
     *
     * Format is filename:line: message
     *
     */
    @Override
    public boolean createMarkers(final String msg) {
        boolean result = false;
        final Iterable<String> pars = Splitter.on(':').limit(3).split(msg);
        if (pars.iterator().next().equals("ERROR")) {
            MarkerUtils.createProblemMarker(project, null, pars.iterator().next(), -1,
                    IMarker.SEVERITY_ERROR);
            result = true;
        } else {
            final Iterator<String> iterator = pars.iterator();
            try {
                final String filename = iterator.next();
                final int line = Integer.parseInt(iterator.next());
                final String rawmessage = iterator.next().trim();
                String message;
                int severity;
                if (rawmessage.startsWith("Warning: ")) {
                    message = rawmessage.substring("Warning: ".length());
                    severity = IMarker.SEVERITY_WARNING;
                } else {
                    message = rawmessage;
                    severity = IMarker.SEVERITY_ERROR;
                }
                final IResource resource = project.findMember(filename);
                if (resource != null) {
                    MarkerUtils.createProblemMarker(resource, null, message, line,
                            severity);
                    result = true;
                }
            } catch (final Exception e) {
            }
        }
        return result;
    }
}
