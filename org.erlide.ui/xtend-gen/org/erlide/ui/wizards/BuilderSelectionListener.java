package org.erlide.ui.wizards;

import com.google.common.base.Objects;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.erlide.engine.model.builder.BuilderConfig;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.ui.wizards.ErlangProjectBuilderPage;
import org.erlide.ui.wizards.NewProjectData;

@SuppressWarnings("all")
public class BuilderSelectionListener implements SelectionListener {
  private NewProjectData info;
  
  private ErlangProjectBuilderPage panel;
  
  public BuilderSelectionListener(final NewProjectData info, final ErlangProjectBuilderPage panel) {
    this.info = info;
    this.panel = panel;
  }
  
  public BuilderSelectionListener(final NewProjectData info) {
    this(info, null);
  }
  
  public void widgetDefaultSelected(final SelectionEvent e) {
  }
  
  public void widgetSelected(final SelectionEvent e) {
    boolean _tripleNotEquals = (this.panel != null);
    if (_tripleNotEquals) {
      Object _data = e.widget.getData();
      String _name = ((BuilderTool) _data).name();
      this.info.setBuilderName(_name);
      String _builderName = this.info.getBuilderName();
      String _name_1 = BuilderTool.MAKE.name();
      boolean _equals = Objects.equal(_builderName, _name_1);
      this.panel.configComposite.setVisible(_equals);
    } else {
      Object _data_1 = e.widget.getData();
      String _name_2 = ((BuilderConfig) _data_1).name();
      this.info.setBuilderConfig(_name_2);
    }
  }
}
