package org.erlide.core.builder.external;

import java.util.List;
import java.util.function.Consumer;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.IBackend;
import org.erlide.core.builder.BuildNotifier;
import org.erlide.core.builder.ExternalBuilder;
import org.erlide.engine.MarkerUtils;
import org.erlide.engine.model.builder.BuilderProperties;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;

@SuppressWarnings("all")
public class EmakeBuilder extends ExternalBuilder {
    public EmakeBuilder() {
        super(BuilderTool.EMAKE);
    }

    @Override
    public String getOsCommand(final IErlProject erlProject) {
        String _xblockexpression = null;
        {
            final IBackend backend = BackendCore.getBackendManager()
                    .getBuildBackend(erlProject);
            final String _otpHome = backend.getRuntime().getOtpHome();
            final IPath path = new Path(_otpHome).append("bin/erl");
            String _xifexpression = null;
            final boolean _isOnWindows = SystemConfiguration.getInstance().isOnWindows();
            if (_isOnWindows) {
                final String _portableString = path.toPortableString();
                _xifexpression = _portableString + ".exe";
            } else {
                _xifexpression = path.toPortableString();
            }
            _xblockexpression = _xifexpression;
        }
        return _xblockexpression;
    }

    @Override
    protected String getCompileTarget() {
        return "-make";
    }

    @Override
    protected String getCleanTarget() {
        return null;
    }

    @Override
    public void clean(final IErlProject erlProject, final BuildNotifier notifier) {
        final IProject project = erlProject.getWorkspaceProject();
        MarkerUtils.removeProblemMarkersFor(project);
        final IFolder bf = project.getFolder(erlProject.getProperties().getOutputDir());
        final Procedure1<IFolder> _function = (final IFolder it) -> {
            try {
                final boolean _exists = it.exists();
                if (_exists) {
                    final Consumer<IResource> _function_1 = (final IResource it_1) -> {
                        try {
                            it_1.delete(true, null);
                        } catch (final Throwable _t) {
                            if (_t instanceof CoreException) {
                                final IPath _location = it_1.getLocation();
                                final String _plus = "Could not clean up output directory "
                                        + _location;
                                ErlLogger.warn(_plus);
                            } else {
                                throw Exceptions.sneakyThrow(_t);
                            }
                        }
                    };
                    ((List<IResource>) Conversions.doWrapArray(it.members()))
                            .forEach(_function_1);
                }
            } catch (final Throwable _e) {
                throw Exceptions.sneakyThrow(_e);
            }
        };
        ObjectExtensions.<IFolder> operator_doubleArrow(bf, _function);
    }

    @Override
    public BuilderProperties getProperties() {
        return null;
    }
}
