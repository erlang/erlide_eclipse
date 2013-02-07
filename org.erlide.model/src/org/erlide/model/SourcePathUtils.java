package org.erlide.model;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.RegistryFactory;
import org.erlide.utils.ErlLogger;

import com.google.common.collect.Lists;

public class SourcePathUtils {

    public static Collection<IPath> getExtraSourcePathsForBuild(
            final IProject project) {
        return getExtraSourcePathsGeneric(project, new SPPMethod() {
            @Override
            public Collection<IPath> call(final IProject myProject) {
                return target.getSourcePathsForBuild(myProject);
            }
        });
    }

    public static Collection<IPath> getExtraSourcePathsForModel(
            final IProject project) {
        return getExtraSourcePathsGeneric(project, new SPPMethod() {
            @Override
            public Collection<IPath> call(final IProject myProject) {
                return target.getSourcePathsForModel(myProject);
            }
        });
    }

    public static Collection<IPath> getExtraSourcePathsForExecution(
            final IProject project) {
        return getExtraSourcePathsGeneric(project, new SPPMethod() {
            @Override
            public Collection<IPath> call(final IProject myProject) {
                return target.getSourcePathsForExecution(myProject);
            }
        });
    }

    private static Collection<IPath> getExtraSourcePathsGeneric(
            final IProject project, final SPPMethod method) {
        final List<IPath> result = Lists.newArrayList();
        Collection<SourcePathProvider> spps;
        try {
            spps = getSourcePathProviders();
            for (final SourcePathProvider spp : spps) {
                method.setTarget(spp);
                final Collection<IPath> paths = method.call(project);
                result.addAll(paths);
            }
        } catch (final Exception e) {
            ErlLogger.error(e);
        }
        return result;
    }

    public static Collection<IPath> getExtraSourcePaths() {
        final IProject[] projects = ResourcesPlugin.getWorkspace().getRoot()
                .getProjects();
        final List<IPath> result = Lists.newArrayList();
        for (final IProject project : projects) {
            result.addAll(getExtraSourcePathsForModel(project));
        }
        return result;
    }

    private static Collection<SourcePathProvider> sourcePathProviders = null;

    public static synchronized Collection<SourcePathProvider> getSourcePathProviders()
            throws CoreException {
        if (sourcePathProviders != null) {
            return sourcePathProviders;
        }
        // TODO should be listening to plugin changes
        sourcePathProviders = Lists.newArrayList();
        final IConfigurationElement[] elements = getSourcepathConfigurationElements();
        for (final IConfigurationElement element : elements) {
            final SourcePathProvider provider = (SourcePathProvider) element
                    .createExecutableExtension("class");
            sourcePathProviders.add(provider);
        }
        return sourcePathProviders;
    }

    public static IConfigurationElement[] getSourcepathConfigurationElements() {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        return reg.getConfigurationElementsFor(ModelPlugin.PLUGIN_ID,
                "sourcePathProvider");
    }

    private abstract static class SPPMethod {
        protected SourcePathProvider target;

        public void setTarget(final SourcePathProvider spp) {
            target = spp;
        }

        abstract public Collection<IPath> call(IProject project);
    }

}
