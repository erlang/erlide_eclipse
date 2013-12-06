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
  private final NewProjectData info;
  
  private final ErlangProjectBuilderPage page;
  
  public BuilderSelectionListener(final NewProjectData info, final ErlangProjectBuilderPage page) {
    this.info = info;
    this.page = page;
  }
  
  public BuilderSelectionListener(final NewProjectData info) {
    this(info, null);
  }
  
  public void widgetDefaultSelected(final SelectionEvent e) {
  }
  
  public void widgetSelected(final SelectionEvent e) {
    boolean _tripleNotEquals = (this.page != null);
    if (_tripleNotEquals) {
      Object _data = e.widget.getData();
      String _name = ((BuilderTool) _data).name();
      this.info.setBuilderName(_name);
      boolean _or = false;
      String _builderName = this.info.getBuilderName();
      String _name_1 = BuilderTool.MAKE.name();
      boolean _equals = Objects.equal(_builderName, _name_1);
      if (_equals) {
        _or = true;
      } else {
        String _builderName_1 = this.info.getBuilderName();
        String _name_2 = BuilderTool.INTERNAL.name();
        boolean _equals_1 = Objects.equal(_builderName_1, _name_2);
        _or = (_equals || _equals_1);
      }
      this.page.configComposite.setVisible(_or);
      String _builderName_2 = this.info.getBuilderName();
      String _name_3 = BuilderTool.MAKE.name();
      boolean _equals_2 = Objects.equal(_builderName_2, _name_3);
      this.page.makeConfigComposite.setVisible(_equals_2);
    } else {
      Object _data_1 = e.widget.getData();
      String _name_4 = ((BuilderConfig) _data_1).name();
      this.info.setBuilderConfig(_name_4);
    }
  }
}
