package org.erlide.ui.wizards;

import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.erlide.core.internal.builder.BuilderInfo;
import org.erlide.engine.model.root.IErlangProjectProperties;

@SuppressWarnings("all")
public class BuilderSelectionListener implements SelectionListener {
  private final IErlangProjectProperties info;
  
  public BuilderSelectionListener(final IErlangProjectProperties info) {
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
