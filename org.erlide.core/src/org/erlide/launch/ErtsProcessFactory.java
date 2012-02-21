package org.erlide.launch;

import java.util.Map;

import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.IProcessFactory;
import org.eclipse.debug.core.model.IProcess;
import org.erlide.launch.debug.model.ErtsProcess;

public class ErtsProcessFactory implements IProcessFactory {

    @Override
    public IProcess newProcess(final ILaunch launch, final Process process,
            final String label,
            @SuppressWarnings("rawtypes") final Map attributes) {
        final String nodeName = (String) attributes.get("NodeName");
        final String workingDir = (String) attributes.get("WorkingDir");
        return new ErtsProcess(launch, process, nodeName, workingDir);
    }

}
