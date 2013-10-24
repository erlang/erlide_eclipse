package org.erlide.core.internal.builder;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.erlide.core.content.ErlangContentDescriber;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.ProjectConfigurator;

import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.google.common.io.Files;

public class ProjectConfigurationPersister {

    private final ProjectConfigurator configurator;
    private final IProject project;

    public ProjectConfigurationPersister(final IProject project,
            final ProjectConfigurator configurator) {
        this.project = project;
        this.configurator = configurator;
    }

    public ErlangProjectProperties getConfiguration() throws IOException {
        if (configurator == null) {
            return null;
        }
        final String configFile = null;// configurator.getConfigFile();
        if (configFile == null) {
            return null;
        }
        final IResource conf = null;// getProject().findMember(configFile);
        final File confFile = new File(conf.getLocation().toString());

        final String line = Files.readFirstLine(confFile, Charsets.ISO_8859_1);
        Charset coding = ErlangContentDescriber.detectEncoding(line);
        if (coding == null) {
            coding = Charsets.ISO_8859_1;
        }
        final List<String> confString = Files.readLines(confFile, coding);
        if (confString != null) {
            return configurator.decodeConfig(Joiner.on("\n").join(confString));
        }
        // TODO or throw exception?
        return null;
    }

    public void setConfiguration(final ErlangProjectProperties info) throws IOException {
        if (configurator == null) {
            return;
        }
        final String configFile = null; // configurator.getConfigFile();
        if (configFile == null) {
            return;
        }
        final IResource conf = null; // getProject().findMember(configFile);
        final File confFile = new File(conf.getLocation().toString());
        final String confString = configurator.encodeConfig(project, info);
        if (confString != null) {
            Files.write("%% coding: UTF-8\n" + confString, confFile, Charsets.UTF_8);
        }
    }
}
