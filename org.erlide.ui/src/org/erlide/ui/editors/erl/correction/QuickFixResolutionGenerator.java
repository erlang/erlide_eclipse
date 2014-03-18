package org.erlide.ui.editors.erl.correction;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolutionGenerator;
import org.erlide.engine.model.builder.MarkerUtils;

public class QuickFixResolutionGenerator implements IMarkerResolutionGenerator {

    @Override
    public IMarkerResolution[] getResolutions(final IMarker marker) {

        try {
            if (!marker.getType().equals(MarkerUtils.PROBLEM_MARKER)) {
                return new IMarkerResolution[0];
            }

            final IResource resource = marker.getResource();
            final int line = marker.getAttribute(IMarker.LINE_NUMBER, -1);
            final String message = marker.getAttribute(IMarker.MESSAGE, "");

            final ErlangQuickFixCollector collector = new ErlangQuickFixCollector();
            final IMarkerResolution[] result = collector
                    .getFixes(resource, line, message);
            return result;

        } catch (final CoreException e) {
            return new IMarkerResolution[0];
        }
    }
}
