package org.erlide.core.internal.builder;

import java.io.IOException;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.ProjectConfigurator;

import com.google.common.base.Preconditions;

public class PreferencesProjectConfigurationPersister extends
        ProjectConfigurationPersister {

    private final String nodeKey;

    public PreferencesProjectConfigurationPersister(@NonNull final IProject project,
            @NonNull final ProjectConfigurator configurator, @NonNull final String nodeKey) {
        super(project, configurator);
        Preconditions.checkNotNull(nodeKey);
        this.nodeKey = nodeKey;
    }

    @Override
    public ErlangProjectProperties getConfiguration() throws IOException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void setConfiguration(final ErlangProjectProperties info) throws IOException {
        // TODO Auto-generated method stub

    }

}
