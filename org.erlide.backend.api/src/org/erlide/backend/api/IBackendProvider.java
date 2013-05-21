package org.erlide.backend.api;

import org.eclipse.core.resources.IProject;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.RuntimeVersion;

public interface IBackendProvider {

    IRpcSite get();

    IRpcSite get(RuntimeVersion version);

    IRpcSite get(IProject project);

}
