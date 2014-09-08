package org.erlide.engine.internal.model.root;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.root.ErlangContentDescriber;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IProjectConfigurator;
import org.erlide.engine.model.root.ProjectConfigurationSerializer;
import org.erlide.util.ErlLogger;
import org.erlide.util.IDisposable;

import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.io.Files;

public class FileProjectConfigurator implements IProjectConfigurator,
        IResourceChangeListener, IDisposable {

    private final String filePath;
    @NonNull
    private final ProjectConfigurationSerializer serializer;

    public FileProjectConfigurator(final ProjectConfigurationSerializer serializer,
            final String filePath) {
        Preconditions.checkArgument(filePath != null);
        Preconditions.checkArgument(serializer != null);
        this.serializer = serializer;
        this.filePath = filePath;
        ResourcesPlugin.getWorkspace().addResourceChangeListener(this,
                IResourceChangeEvent.POST_CHANGE);
    }

    @Override
    public ErlangProjectProperties getConfiguration() {
        if (filePath == null) {
            return null;
        }

        return getRawConfig(new File(filePath));
    }

    private ErlangProjectProperties getRawConfig(final File confFile) {
        ErlangProjectProperties result = null;
        try {
            final Charset coding = ErlangContentDescriber.detectCodingForFile(confFile);
            final List<String> confString = Files.readLines(confFile, coding);
            if (confString != null) {
                final String content = Joiner.on("\n").join(confString);
                if (content != null) {
                    result = getSerializer().decodeConfig(content);
                }
            }
        } catch (final IOException e) {
            ErlLogger.error(e);
            return null;
        }
        return result;
    }

    @Override
    public void setConfiguration(final ErlangProjectProperties info) {
        final File confFile = new File(filePath);
        final String confString = getSerializer().encodeConfig(info);
        if (confString != null) {
            final String content = "%% coding: UTF-8\n" + confString;
            try {
                Files.write(content, confFile, Charsets.UTF_8);
            } catch (final IOException e) {
                ErlLogger.error(e);
            }
        }
    }

    public ProjectConfigurationSerializer getSerializer() {
        return serializer;
    }

    @Override
    public void resourceChanged(final IResourceChangeEvent event) {
        final IResourceDelta delta = event.getDelta();
        try {
            delta.accept(new IResourceDeltaVisitor() {

                @Override
                public boolean visit(final IResourceDelta aDelta) throws CoreException {
                    final IResource res = aDelta.getResource();
                    if (res.getLocation().equals(new Path(filePath))) {
                        System.out.println("DETECTED " + aDelta.getKind() + " " + res);
                    }
                    return false;

                }
            });
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }
    }

    @Override
    public void dispose() {
        ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
    }

}
