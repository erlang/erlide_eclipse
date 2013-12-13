package org.erlide.ui.wizards;

import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.erlide.ui.wizards.NewProjectData;

@SuppressWarnings("all")
public class ErlangNewProjectCreationPage extends WizardNewProjectCreationPage {
  private final NewProjectData info;
  
  public ErlangNewProjectCreationPage(final String name, final NewProjectData info) {
    super(name);
    this.info = info;
  }
  
  public void setVisible(final boolean visible) {
    super.setVisible(visible);
    if ((!visible)) {
      String _projectName = this.getProjectName();
      this.info.setName(_projectName);
      IPath _locationPath = this.getLocationPath();
      this.info.setLocation(_locationPath);
    }
  }
}
