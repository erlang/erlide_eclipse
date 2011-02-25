package org.erlide.core.model.erlang.util;

import org.eclipse.core.resources.IProject;

public interface ContainerFilterCreator {

    ContainerFilter createFilterForProject(IProject project);

}
