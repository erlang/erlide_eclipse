package org.erlide.core.internal.builder;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.List;

import org.erlide.core.content.ErlangContentDescriber;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.ProjectConfigurationPersister;
import org.erlide.engine.model.root.ProjectConfigurator;
import org.erlide.util.ErlLogger;

import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.io.Files;

public class FileProjectConfigurationPersister extends ProjectConfigurationPersister {

    private final String filePath;
    private final ProjectConfigurator configurator;

    public FileProjectConfigurationPersister(final ProjectConfigurator configurator,
            final String filePath) {
        Preconditions.checkNotNull(filePath);
        Preconditions.checkNotNull(configurator);
        this.configurator = configurator;
        this.filePath = filePath;
    }

    @Override
    public ErlangProjectProperties getConfiguration() {
        if (filePath == null) {
            return null;
        }

        return getRawConfig(new File(filePath));
    }

    private ErlangProjectProperties getRawConfig(final File confFile) {
        String line;
        ErlangProjectProperties result = null;
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
                    result = getConfigurator().decodeConfig(content);
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
        final String confString = getConfigurator().encodeConfig(info);
        if (confString != null) {
            final String content = "%% coding: UTF-8\n" + confString;
            try {
                Files.write(content, confFile, Charsets.UTF_8);
            } catch (final IOException e) {
                ErlLogger.error(e);
            }
        }
    }

    @Override
    public ProjectConfigurator getConfigurator() {
        return configurator;
    }

}
