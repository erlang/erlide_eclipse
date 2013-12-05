package org.erlide.engine.services.search;

import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.erlide.engine.model.erlang.IErlModule;

public interface ModelSearcherService {

    Collection<IErlModule> findAllIncludedFiles(IErlModule module) throws CoreException;

}
