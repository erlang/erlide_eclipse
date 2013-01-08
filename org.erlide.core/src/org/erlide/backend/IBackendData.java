package org.erlide.backend;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.erlide.core.model.IBeamLocator;
import org.erlide.runtime.IRuntimeData;

public interface IBackendData extends IRuntimeData {

    public final static String PROJECT_NAME_SEPARATOR = ";";

    ILaunch getLaunch();

    ILaunchConfiguration asLaunchConfiguration();

    void setBeamLocator(final IBeamLocator beamLocator);

    IBeamLocator getBeamLocator();

    List<String> getInterpretedModules();

    void setInterpretedModules(final List<String> interpretedModules);

    Collection<IProject> getProjects();

}
