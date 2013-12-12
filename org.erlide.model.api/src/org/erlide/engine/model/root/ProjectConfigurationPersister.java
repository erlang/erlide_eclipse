package org.erlide.engine.model.root;


public abstract class ProjectConfigurationPersister {

    private IErlProject project;

    public IErlProject getProject() {
        return project;
    }

    public void setProject(final IErlProject project) {
        this.project = project;
    }

    public abstract ProjectConfigurator getConfigurator();

    public abstract ErlangProjectProperties getConfiguration();

    public abstract void setConfiguration(final ErlangProjectProperties info);

}
