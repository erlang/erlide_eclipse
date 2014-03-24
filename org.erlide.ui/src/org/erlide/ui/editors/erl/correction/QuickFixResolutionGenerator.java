package org.erlide.ui.editors.erl.correction;

import org.eclipse.core.resources.IMarker;
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

            final ErlangQuickFixCollector collector = new ErlangQuickFixCollector();
            final IMarkerResolution[] result = collector.getFixes(marker);
            return result;

        } catch (final CoreException e) {
            return new IMarkerResolution[0];
        }
    }
}
