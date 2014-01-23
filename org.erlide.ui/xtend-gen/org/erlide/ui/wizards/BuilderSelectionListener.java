package org.erlide.ui.wizards;

import com.google.common.base.Objects;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.NewProjectData;
import org.erlide.ui.wizards.ErlangProjectBuilderPage;

@SuppressWarnings("all")
public class BuilderSelectionListener implements SelectionListener {
  private final NewProjectData info;
  
  private final ErlangProjectBuilderPage page;
  
  public BuilderSelectionListener(final NewProjectData info, final ErlangProjectBuilderPage page) {
    this.info = info;
    this.page = page;
  }
  
  public void widgetDefaultSelected(final SelectionEvent e) {
  }
  
  public void widgetSelected(final SelectionEvent e) {
    Object _data = e.widget.getData();
    this.info.setBuilder(((BuilderTool) _data));
    boolean _or = false;
    BuilderTool _builder = this.info.getBuilder();
    boolean _equals = Objects.equal(_builder, BuilderTool.MAKE);
    if (_equals) {
      _or = true;
    } else {
      BuilderTool _builder_1 = this.info.getBuilder();
      boolean _equals_1 = Objects.equal(_builder_1, BuilderTool.INTERNAL);
      _or = (_equals || _equals_1);
    }
    this.page.configComposite.setVisible(_or);
    BuilderTool _builder_2 = this.info.getBuilder();
    boolean _equals_2 = Objects.equal(_builder_2, BuilderTool.MAKE);
    this.page.makeConfigComposite.setVisible(_equals_2);
  }
}
