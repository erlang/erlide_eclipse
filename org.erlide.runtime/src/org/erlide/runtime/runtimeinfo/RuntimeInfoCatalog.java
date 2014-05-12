/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.runtimeinfo;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import org.eclipse.jdt.annotation.NonNull;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public final class RuntimeInfoCatalog implements IRuntimeInfoCatalog {

    public RuntimeInfo erlideRuntime;
    public final Map<String, RuntimeInfo> runtimes;
    public String defaultRuntimeName;

    public RuntimeInfoCatalog() {
        runtimes = Maps.newHashMap();
        erlideRuntime = RuntimeInfo.NO_RUNTIME_INFO;
        defaultRuntimeName = null;
    }

    @Override
    public synchronized Collection<RuntimeInfo> getRuntimes() {
        return Collections.unmodifiableCollection(runtimes.values());
    }

    @Override
    public synchronized void setRuntimes(final Collection<RuntimeInfo> elements,
            final String dfltRuntime, final String ideRuntime) {
        runtimes.clear();
        if (elements.isEmpty()) {
            initializeRuntimesList();
        }

        for (final RuntimeInfo rt : elements) {
            addRuntime(rt);
        }
        defaultRuntimeName = dfltRuntime;
        if (defaultRuntimeName == null) {
            setDefaultRuntimes();
        }
        erlideRuntime = runtimes
                .get(ideRuntime != null ? ideRuntime : defaultRuntimeName);
        // Asserts.isNotNull(erlideRuntime);
    }

    @Override
    public synchronized void addRuntime(final RuntimeInfo rt) {
        if (rt.getVersion().isCompatible(OLDEST_SUPPORTED_VERSION)
                && !runtimes.containsKey(rt.getName())) {
            runtimes.put(rt.getName(), rt);
        }
    }

    @Override
    public synchronized Collection<String> getRuntimeNames() {
        return runtimes.keySet();
    }

    @Override
    public boolean hasRuntimeWithName(final String name) {
        return runtimes.containsKey(name);
    }

    @SuppressWarnings("null")
    @Override
    public @NonNull RuntimeInfo getRuntime(final String name) {
        final RuntimeInfo rt = runtimes.get(name);
        if (rt != null) {
            return rt;
        }
        return RuntimeInfo.NO_RUNTIME_INFO;
    }

    @Override
    public synchronized void removeRuntime(final String name) {
        runtimes.remove(name);
        if (!runtimes.isEmpty()) {
            if (erlideRuntime.getName().equals(name)) {
                erlideRuntime = runtimes.values().iterator().next();
            }
            if (defaultRuntimeName.equals(name)) {
                defaultRuntimeName = runtimes.keySet().iterator().next();
            }
        } else {
            erlideRuntime = RuntimeInfo.NO_RUNTIME_INFO;
            defaultRuntimeName = null;
        }
    }

    @Override
    public synchronized String getDefaultRuntimeName() {
        return defaultRuntimeName;
    }

    @Override
    public synchronized void setDefaultRuntime(final String name) {
        defaultRuntimeName = name;
    }

    private synchronized void setErlideRuntime(final @NonNull RuntimeInfo runtime) {
        final RuntimeInfo old = erlideRuntime;
        if (old == null || !old.equals(runtime)) {
            erlideRuntime = runtime;
            // this creates infinite recursion!
            // BackendManagerImpl.getDefault().getIdeBackend().stop();
        }
    }

    @SuppressWarnings("null")
    @Override
    public synchronized @NonNull RuntimeInfo getErlideRuntime() {
        return erlideRuntime;
    }

    @Override
    public synchronized @NonNull RuntimeInfo getDefaultRuntime() {
        return getRuntime(getDefaultRuntimeName());
    }

    @Override
    public RuntimeInfo getRuntime(final RuntimeVersion runtimeVersion,
            final String runtimeName) {
        final List<RuntimeInfo> vsns = VersionLocator.locateVersion(runtimeVersion,
                runtimes.values(), false);
        if (vsns.isEmpty()) {
            return null;
        } else if (vsns.size() == 1) {
            return vsns.get(0);
        } else {
            for (final RuntimeInfo ri : vsns) {
                if (runtimeName == null || ri.getName().equals(runtimeName)) {
                    return ri;
                }
            }
            return vsns.get(0);
        }
    }

    @Override
    public List<String> getAllRuntimesVersions() {
        final Collection<RuntimeInfo> rs = getRuntimes();
        final List<String> myVersions = Lists.newArrayList();
        for (final RuntimeInfo info : rs) {
            final String version = info.getVersion().asMinor().toString();
            if (!myVersions.contains(version)) {
                myVersions.add(version);
            }
        }
        Collections.sort(myVersions);
        return myVersions;
    }

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
    @Override
    public void initializeRuntimesList() {
        final Collection<RuntimeInfo> found = RuntimeFinder.guessRuntimeLocations();
        for (final RuntimeInfo info : found) {
            addRuntime(info);
        }
        setDefaultRuntimes();
    }

    private void setDefaultRuntimes() {
        final List<RuntimeInfo> list = new ArrayList<RuntimeInfo>(getRuntimes());
        Collections.sort(list, new Comparator<RuntimeInfo>() {
            @Override
            public int compare(final RuntimeInfo o1, final RuntimeInfo o2) {
                final int x = o2.getVersion().compareTo(o1.getVersion());
                if (x != 0) {
                    return x;
                }
                return o2.getName().compareTo(o1.getName());
            }
        });
        if (!list.isEmpty()) {
            final String firstName = list.get(0).getName();
            if (defaultRuntimeName == null) {
                setDefaultRuntime(firstName);
            }

            // the erlide backend is the most recent stable version
            for (final RuntimeInfo info : list) {
                if (info.getVersion().isStable()) {
                    setErlideRuntime(info);
                    break;
                }
            }
            if (erlideRuntime == null) {
                setErlideRuntime(getDefaultRuntime());
            }
        }
    }

}
