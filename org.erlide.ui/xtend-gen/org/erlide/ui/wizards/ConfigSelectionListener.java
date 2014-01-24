package org.erlide.ui.wizards;

import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.erlide.engine.model.root.NewProjectData;
import org.erlide.engine.model.root.ProjectConfigType;

@SuppressWarnings("all")
public class ConfigSelectionListener implements SelectionListener {
  private final NewProjectData info;
  
  public ConfigSelectionListener(final NewProjectData info) {
    this.info = info;
  }
  
  public void widgetDefaultSelected(final SelectionEvent e) {
  }
  
  public void widgetSelected(final SelectionEvent e) {
    Object _data = e.widget.getData();
    this.info.setConfigType(((ProjectConfigType) _data));
  }
}
