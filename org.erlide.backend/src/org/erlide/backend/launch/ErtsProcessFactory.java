package org.erlide.backend.launch;

import java.util.Map;

import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.IProcessFactory;
import org.eclipse.debug.core.model.IProcess;
import org.erlide.backend.debug.model.ErtsProcess;

public class ErtsProcessFactory implements IProcessFactory {

    @Override
    public IProcess newProcess(final ILaunch launch, final Process process,
            final String label, @SuppressWarnings("rawtypes") final Map attributes0) {
        @SuppressWarnings("unchecked")
        final Map<String, String> attributes = attributes0;
        final String nodeName = attributes.get("NodeName");
        final String workingDir = attributes.get("WorkingDir");
        return new ErtsProcess(launch, process, nodeName, workingDir);
    }

}
