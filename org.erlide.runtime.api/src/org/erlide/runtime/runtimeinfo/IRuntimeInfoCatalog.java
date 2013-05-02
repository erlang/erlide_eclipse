package org.erlide.runtime.runtimeinfo;

import java.util.Collection;
import java.util.List;

import org.erlide.runtime.api.RuntimeVersion;

public interface IRuntimeInfoCatalog {

    public final static RuntimeVersion OLDEST_SUPPORTED_VERSION = new RuntimeVersion(
            14);

    public abstract Collection<RuntimeInfo> getRuntimes();

    public abstract void setRuntimes(final Collection<RuntimeInfo> elements,
            final String dfltRuntime, String ideRuntime);

    public abstract void addRuntime(final RuntimeInfo rt);

    public abstract Collection<String> getRuntimeNames();

    public abstract boolean hasRuntimeWithName(final String name);

    public abstract RuntimeInfo getRuntime(final String name);

    public abstract void removeRuntime(final String name);

    public abstract String getDefaultRuntimeName();

    public abstract void setDefaultRuntime(final String name);

    public abstract RuntimeInfo getErlideRuntime();

    public abstract RuntimeInfo getDefaultRuntime();

    public abstract RuntimeInfo getRuntime(final RuntimeVersion runtimeVersion,
            final String runtimeName);

    public abstract List<String> getAllRuntimesVersions();

    /**
     * If runtime is not set, try to locate one. The first one found as below is
     * set as default. All "obvious" runtimes found are stored.
     * <ul>
     * <li>A system property <code>erlide.runtime</code> can be set to point to
     * a location.</li>
     * <li>A preference in the default scope
     * <code>org.erlide.core/default_runtime</code> can be set to point to a
     * location.</li>
     * <li>Look for existing Erlang runtimes in a few obvious places and install
     * them, choosing a suitable one as default.</li>
     * </ul>
     * 
     */
    public abstract void initializeRuntimesList();

}
