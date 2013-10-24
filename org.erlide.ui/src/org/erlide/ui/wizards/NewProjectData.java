package org.erlide.ui.wizards;

import org.eclipse.core.runtime.IPath;
import org.erlide.engine.model.root.ErlangProjectProperties;

public class NewProjectData extends ErlangProjectProperties {

    private String builderName;
    private String name;
    private IPath location;

    public String getBuilderName() {
        return builderName;
    }

    public void setBuilderName(final String builderName) {
        this.builderName = builderName;
    }

    public void setName(final String projectName) {
        name = projectName;
    }

    public String getName() {
        return name;
    }

    public IPath getLocation() {
        return location;
    }

    public void setLocation(final IPath location) {
        this.location = location;
    }

}
