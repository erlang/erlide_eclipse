package org.erlide.core.internal.builder;

import java.util.Iterator;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.erlide.core.builder.MarkerUtils;

import com.google.common.base.Splitter;

public class ErlcMessageParser implements IMessageParser {

    private final IProject project;

    public ErlcMessageParser(final IProject project) {
        this.project = project;
    }

    /**
     * Parses messages from erlc and creates the markers on the appropriate
     * resource.
     * 
     * Format is filename:line: message
     * 
     */
    @Override
    public void createMarkers(final String msg) {
        final Iterable<String> pars = Splitter.on(':').limit(3).split(msg);
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
                MarkerUtils.addProblemMarker(resource, null, null, message, line,
                        severity);
            }

        } catch (final Exception e) {
        }
    }

}
