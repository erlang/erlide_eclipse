package org.erlide.runtime.runtimeinfo;

import java.util.Collection;
import java.util.List;

public interface IRuntimeInfoCatalog {

    RuntimeVersion OLDEST_SUPPORTED_VERSION = new RuntimeVersion(15);

    Collection<RuntimeInfo> getRuntimes();

    void setRuntimes(final Collection<RuntimeInfo> elements, final String dfltRuntime,
            String ideRuntime);

    void addRuntime(final RuntimeInfo rt);

    Collection<String> getRuntimeNames();

    boolean hasRuntimeWithName(final String name);

    RuntimeInfo getRuntime(final String name);

    void removeRuntime(final String name);

    String getDefaultRuntimeName();

    void setDefaultRuntime(final String name);

    RuntimeInfo getErlideRuntime();

    RuntimeInfo getDefaultRuntime();

    RuntimeInfo getRuntime(final RuntimeVersion runtimeVersion, final String runtimeName);

    List<String> getAllRuntimesVersions();

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
    void initializeRuntimesList();

}
