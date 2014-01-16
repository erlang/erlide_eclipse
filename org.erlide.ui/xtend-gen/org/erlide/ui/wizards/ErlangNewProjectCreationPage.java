package org.erlide.ui.wizards;

import java.net.URI;
import org.eclipse.core.runtime.Path;
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
      URI _locationURI = this.getLocationURI();
      String _path = _locationURI.getPath();
      Path _path_1 = new Path(_path);
      this.info.setLocation(_path_1);
    }
  }
}
