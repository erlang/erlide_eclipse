package org.erlide.core.preferences;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;

import org.erlide.jinterface.backend.RuntimeVersion;

import com.google.common.collect.Sets;

public class ErlProjectInfo {

    private final RuntimeVersion requiredRuntimeVersion;
    private final ErlProjectLayout layout;

    private final Set<PathEntry> codePathEntries = Sets.newHashSet();

    private final Set<PathEntry> dependencies = Sets.newHashSet();

    public ErlProjectInfo() {
        this(ErlProjectLayout.OTP_LAYOUT);
    }

    public ErlProjectInfo(ErlProjectLayout layout) {
        this(new RuntimeVersion("R14B"), layout);
    }

    public ErlProjectInfo(RuntimeVersion version, ErlProjectLayout layout) {
        this.requiredRuntimeVersion = version;
        this.layout = layout;
    }

    public Collection<PathEntry> getDependencies() {
        return Collections.unmodifiableCollection(dependencies);
    }

    public RuntimeVersion getRequiredRuntimeVersion() {
        return requiredRuntimeVersion;
    }

    public ErlProjectLayout getLayout() {
        return layout;
    }

    public ErlProjectInfo addDependencies(Collection<PathEntry> locations) {
        Collection<PathEntry> dependencies = getDependencies();
        for (PathEntry loc : locations) {
            if (!dependencies.contains(loc)) {
                dependencies.add(loc);
            }
        }
        return new ErlProjectInfo(/* dependencies */);
    }

    public ErlProjectInfo removeDependencies(Collection<PathEntry> locations) {
        Collection<PathEntry> dependencies = getDependencies();
        for (PathEntry loc : locations) {
            dependencies.remove(loc);
        }
        return new ErlProjectInfo(/* dependencies */);
    }

    public ErlProjectInfo setRequiredRuntimeVersion(
            RuntimeVersion runtimeVersion) {
        return new ErlProjectInfo(/* runtimeVersion */);
    }

}
