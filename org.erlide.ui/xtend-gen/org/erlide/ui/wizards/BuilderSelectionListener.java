package org.erlide.ui.wizards;

import com.google.common.base.Objects;
import java.util.Collection;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.core.executor.ToolExecutor;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.NewProjectData;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.ui.wizards.ErlangProjectBuilderPage;

@SuppressWarnings("all")
public class BuilderSelectionListener implements SelectionListener {
  private final NewProjectData info;
  
  private final ErlangProjectBuilderPage page;
  
  public BuilderSelectionListener(final NewProjectData info, final ErlangProjectBuilderPage page) {
    this.info = info;
    this.page = page;
  }
  
  @Override
  public void widgetDefaultSelected(final SelectionEvent e) {
  }
  
  @Override
  public void widgetSelected(final SelectionEvent e) {
    Object _data = e.widget.getData();
    this.info.setBuilder(((BuilderTool) _data));
    BuilderTool _builder = this.info.getBuilder();
    final Collection<ProjectConfigType> cfgs = _builder.getMatchingConfigs();
    ProjectConfigType _configType = this.info.getConfigType();
    boolean _contains = cfgs.contains(_configType);
    boolean _not = (!_contains);
    if (_not) {
      ProjectConfigType _head = IterableExtensions.<ProjectConfigType>head(cfgs);
      this.page.selectConfig(_head);
    }
    this.page.setMessage(null);
    final boolean toolExists = ((this.info.getBuilder().getOsCommand() == null) || 
      (ToolExecutor.getToolLocation(this.info.getBuilder().getOsCommand()) != null));
    if ((!toolExists)) {
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("The tool \'");
      BuilderTool _builder_2 = this.info.getBuilder();
      String _osCommand = _builder_2.getOsCommand();
      _builder_1.append(_osCommand, "");
      _builder_1.append("\' can\'t be found on your system\'s $PATH");
      this.page.setMessage(_builder_1.toString(), 
        DialogPage.WARNING);
    }
    this.page.configComposite.setVisible((Objects.equal(this.info.getBuilder(), BuilderTool.MAKE) || Objects.equal(this.info.getBuilder(), BuilderTool.INTERNAL)));
    BuilderTool _builder_3 = this.info.getBuilder();
    boolean _equals = Objects.equal(_builder_3, BuilderTool.MAKE);
    this.page.makeConfigComposite.setVisible(_equals);
  }
}
