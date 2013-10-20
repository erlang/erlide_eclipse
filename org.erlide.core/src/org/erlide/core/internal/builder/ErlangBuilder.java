/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.internal.builder;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.content.ErlangContentDescriber;
import org.erlide.engine.model.root.IErlangProjectProperties;
import org.erlide.util.ErlLogger;

import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.google.common.io.Files;

public abstract class ErlangBuilder extends IncrementalProjectBuilder {

    private BuilderConfigurator configurator;

    public abstract String getId();

    @Override
    public abstract IProject[] build(int kind, Map<String, String> args,
            IProgressMonitor monitor) throws CoreException;

    BuilderConfigurator getConfigurator() {
        return configurator;
    }

    public void setConfigurator(final BuilderConfigurator configurator) {
        this.configurator = configurator;
    }

    public IErlangProjectProperties getConfiguration() {
        if (configurator == null) {
            return null;
        }
        final String configFile = configurator.getConfigFile();
        if (configFile == null) {
            return null;
        }
        final IResource conf = getProject().findMember(configFile);
        final File confFile = new File(conf.getLocation().toString());
        try {
            final String line = Files.readFirstLine(confFile, Charsets.ISO_8859_1);
            Charset coding = ErlangContentDescriber.detectEncoding(line);
            if (coding == null) {
                coding = Charsets.ISO_8859_1;
            }
            final List<String> confString = Files.readLines(confFile, coding);
            return configurator.decodeConfig(Joiner.on("\n").join(confString));
        } catch (final IOException e) {
            ErlLogger.error(e);
        }
        return null;
    }

    public void setConfiguration(final IErlangProjectProperties info) {
        if (configurator == null) {
            return;
        }
        final String configFile = configurator.getConfigFile();
        if (configFile == null) {
            return;
        }
        final IResource conf = getProject().findMember(configFile);
        final File confFile = new File(conf.getLocation().toString());
        try {
            final String confString = configurator.encodeConfig(getProject(), info);
            Files.write("%% coding: UTF-8\n" + confString, confFile, Charsets.UTF_8);
        } catch (final IOException e) {
            ErlLogger.error(e);
        }
    }

}
