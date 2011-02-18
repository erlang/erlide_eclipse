package org.erlide.backend.util;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.erlide.backend.BackendPlugin;

import com.google.common.collect.Lists;

public class BackendUtils {

    public static Collection<SourcePathProvider> getSourcePathProviders()
            throws CoreException {
        // TODO should be cached and listening to plugin changes?
        final List<SourcePathProvider> result = Lists.newArrayList();
        final IConfigurationElement[] elements = BackendPlugin
                .getSourcepathConfigurationElements();
        for (final IConfigurationElement element : elements) {
            final SourcePathProvider provider = (SourcePathProvider) element
                    .createExecutableExtension("class");
            result.add(provider);
        }
        return result;
    }

    public static String getErlideNameTag() {
        String fUniqueId;
        final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        final String location = root.getLocation().toPortableString();
        final String user = System.getProperty("user.name");
        final String timestamp = Long
                .toHexString(System.currentTimeMillis() & 0xFFFFFF);
        fUniqueId = Long.toHexString(location.hashCode() & 0xFFFFF) + "_"
                + user + "_" + timestamp;
        return fUniqueId.replaceAll("[^a-zA-Z0-9_-]", "");
    }

}
