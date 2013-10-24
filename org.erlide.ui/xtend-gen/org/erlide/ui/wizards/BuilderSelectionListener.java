package org.erlide.ui.wizards;

import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.erlide.core.internal.builder.BuilderInfo;
import org.erlide.ui.wizards.NewProjectData;

@SuppressWarnings("all")
public class BuilderSelectionListener implements SelectionListener {
  private final NewProjectData info;
  
  public BuilderSelectionListener(final NewProjectData info) {
    this.info = info;
  }
  
  public void widgetDefaultSelected(final SelectionEvent e) {
  }
  
  public void widgetSelected(final SelectionEvent e) {
    Object _data = e.widget.getData();
    String _string = ((BuilderInfo) _data).toString();
    this.info.setBuilderName(_string);
  }
}
