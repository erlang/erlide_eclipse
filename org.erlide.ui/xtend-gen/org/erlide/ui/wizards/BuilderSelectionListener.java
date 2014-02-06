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
  
  public void widgetDefaultSelected(final SelectionEvent e) {
  }
  
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
    boolean _or = false;
    BuilderTool _builder_1 = this.info.getBuilder();
    String _osCommand = _builder_1.getOsCommand();
    boolean _tripleEquals = (_osCommand == null);
    if (_tripleEquals) {
      _or = true;
    } else {
      BuilderTool _builder_2 = this.info.getBuilder();
      String _osCommand_1 = _builder_2.getOsCommand();
      String _toolLocation = ToolExecutor.getToolLocation(_osCommand_1);
      boolean _tripleNotEquals = (_toolLocation != null);
      _or = _tripleNotEquals;
    }
    final boolean toolExists = _or;
    if ((!toolExists)) {
      StringConcatenation _builder_3 = new StringConcatenation();
      _builder_3.append("The tool \'");
      BuilderTool _builder_4 = this.info.getBuilder();
      String _osCommand_2 = _builder_4.getOsCommand();
      _builder_3.append(_osCommand_2, "");
      _builder_3.append("\' can\'t be found on your system\'s $PATH");
      this.page.setMessage(_builder_3.toString(), 
        DialogPage.WARNING);
    }
    boolean _or_1 = false;
    BuilderTool _builder_5 = this.info.getBuilder();
    boolean _equals = Objects.equal(_builder_5, BuilderTool.MAKE);
    if (_equals) {
      _or_1 = true;
    } else {
      BuilderTool _builder_6 = this.info.getBuilder();
      boolean _equals_1 = Objects.equal(_builder_6, BuilderTool.INTERNAL);
      _or_1 = _equals_1;
    }
    this.page.configComposite.setVisible(_or_1);
    BuilderTool _builder_7 = this.info.getBuilder();
    boolean _equals_2 = Objects.equal(_builder_7, BuilderTool.MAKE);
    this.page.makeConfigComposite.setVisible(_equals_2);
  }
}
