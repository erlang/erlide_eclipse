package org.erlide.core.internal.builder;

import java.util.Iterator;
import java.util.NoSuchElementException;

import org.erlide.util.ErlLogger;

import com.google.common.base.Splitter;

public class ErlcMessageParser implements IMessageParser {

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
            final String line = iterator.next();
            final String message = iterator.next();

        } catch (final NoSuchElementException e) {
            ErlLogger.warn("Weird erlc message: '" + msg + "'");
        }
    }

}
