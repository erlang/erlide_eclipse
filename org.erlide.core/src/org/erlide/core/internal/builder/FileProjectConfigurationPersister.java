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
import org.erlide.engine.model.root.ProjectConfigurator;

import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.io.Files;

public class FileProjectConfigurationPersister extends ProjectConfigurationPersister {

    private final String fileName;

    public FileProjectConfigurationPersister(@NonNull final IProject project,
            @NonNull final ProjectConfigurator configurator,
            @NonNull final String fileName) {
        super(project, configurator);
        Preconditions.checkNotNull(fileName);
        this.fileName = fileName;
    }

    @Override
    public ErlangProjectProperties getConfiguration() throws IOException {
        if (fileName == null) {
            return null;
        }
        final IResource conf = getProject().findMember(fileName);
        final File confFile = new File(conf.getLocation().toString());

        final String line = Files.readFirstLine(confFile, Charsets.ISO_8859_1);
        Charset coding = ErlangContentDescriber.detectEncoding(line);
        if (coding == null) {
            coding = Charsets.ISO_8859_1;
        }
        final List<String> confString = Files.readLines(confFile, coding);
        if (confString != null) {
            return getConfigurator().decodeConfig(Joiner.on("\n").join(confString));
        }
        // TODO or throw exception?
        return null;
    }

    @Override
    public void setConfiguration(final ErlangProjectProperties info) throws IOException {
        final IResource conf = getProject().findMember(fileName);
        final File confFile = new File(conf.getLocation().toString());
        final String confString = getConfigurator().encodeConfig(getProject(), info);
        if (confString != null) {
            final String content = "%% coding: UTF-8\n" + confString;
            Files.write(content, confFile, Charsets.UTF_8);
        }
    }

}
