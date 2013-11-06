package org.erlide.core.internal.builder;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.annotation.NonNull;
import org.erlide.core.content.ErlangContentDescriber;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.ProjectConfigurationPersister;
import org.erlide.engine.model.root.ProjectConfigurator;
import org.erlide.util.ErlLogger;

import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.io.Files;

public class FileProjectConfigurationPersister extends ProjectConfigurationPersister {

    private final String fileName;
    private final ProjectConfigurator configurator;
    private final ProjectConfigurationPersister extraPersister;

    public FileProjectConfigurationPersister(
            @NonNull final ProjectConfigurator configurator, final String fileName) {
        Preconditions.checkNotNull(fileName);
        Preconditions.checkNotNull(configurator);
        this.configurator = configurator;
        this.fileName = fileName;
        extraPersister = new PreferencesProjectConfigurationPersister();
    }

    @Override
    public ErlangProjectProperties getConfiguration(final IErlProject project) {
        if (fileName == null) {
            return null;
        }
        final IResource conf = getProject().findMember(fileName);
        final File confFile = new File(conf.getLocation().toString());

        String line;
        try {
            line = Files.readFirstLine(confFile, Charsets.ISO_8859_1);
            Charset coding = ErlangContentDescriber.detectEncoding(line);
            if (coding == null) {
                coding = Charsets.ISO_8859_1;
            }
            final List<String> confString = Files.readLines(confFile, coding);
            if (confString != null) {
                final String content = Joiner.on("\n").join(confString);
                if (content != null) {
                    return mergeWithExtraConfig(project,
                            configurator.decodeConfig(content));
                }
            }
        } catch (final IOException e) {
            ErlLogger.error(e);
        }
        return null;
    }

    private ErlangProjectProperties mergeWithExtraConfig(final IErlProject project,
            final ErlangProjectProperties source) {
        final ErlangProjectProperties extra = extraPersister.getConfiguration(project);
        if (source.getExternalModulesFile() == null) {
            source.setExternalModulesFile(extra.getExternalModulesFile());
        }
        if (source.getExternalIncludesFile() == null) {
            source.setExternalIncludesFile(extra.getExternalIncludesFile());
        }
        return source;
    }

    @Override
    public void setConfiguration(final IErlProject project,
            final ErlangProjectProperties info) {
        final IProject aProject = getProject();
        if (aProject == null) {
            return;
        }
        final IResource conf = aProject.findMember(fileName);
        final File confFile = new File(conf.getLocation().toString());
        final String confString = configurator.encodeConfig(aProject, info);
        if (confString != null) {
            final String content = "%% coding: UTF-8\n" + confString;
            try {
                Files.write(content, confFile, Charsets.UTF_8);
            } catch (final IOException e) {
                ErlLogger.error(e);
            }
        }
    }

}
