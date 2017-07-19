package org.erlide.engine.util;

import org.eclipse.core.resources.IProject;

public interface ContainerFilterCreator {

    ContainerFilter createFilterForProject(IProject project);

}
