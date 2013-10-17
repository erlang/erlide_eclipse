package org.erlide.ui.wizards;

import java.util.List;
import org.eclipse.core.runtime.IPath;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.erlide.core.internal.builder.BuildersInfo;
import org.erlide.engine.model.root.IErlangProjectProperties;

@SuppressWarnings("all")
public class ProjectNewCreationPage extends WizardNewProjectCreationPage {
  private final IErlangProjectProperties info;
  
  public ProjectNewCreationPage(final String name, final IErlangProjectProperties info) {
    super(name);
    this.info = info;
  }
  
  public void createControl(final Composite parent) {
    super.createControl(parent);
    Control _control = this.getControl();
    Composite _composite = new Composite(((Composite) _control), SWT.NONE);
    final Composite composite = _composite;
    GridLayout _gridLayout = new GridLayout(2, false);
    composite.setLayout(_gridLayout);
    Label _label = new Label(composite, SWT.NONE);
    final Label label = _label;
    label.setText("Build system to be used:");
    Combo _combo = new Combo(composite, SWT.READ_ONLY);
    final Combo builder = _combo;
    final BuildersInfo[] builders = BuildersInfo.values();
    final Function1<BuildersInfo,String> _function = new Function1<BuildersInfo,String>() {
      public String apply(final BuildersInfo it) {
        String _string = it.toString();
        String _lowerCase = _string.toLowerCase();
        return _lowerCase;
      }
    };
    List<String> _map = ListExtensions.<BuildersInfo, String>map(((List<BuildersInfo>)Conversions.doWrapArray(builders)), _function);
    builder.setItems(((String[])Conversions.unwrapArray(_map, String.class)));
    int _ordinal = BuildersInfo.INTERNAL.ordinal();
    builder.select(_ordinal);
    final ModifyListener _function_1 = new ModifyListener() {
      public void modifyText(final ModifyEvent it) {
        String _text = builder.getText();
        String _upperCase = _text.toUpperCase();
        ProjectNewCreationPage.this.info.setBuilderName(_upperCase);
      }
    };
    builder.addModifyListener(_function_1);
  }
  
  public void setVisible(final boolean visible) {
    super.setVisible(visible);
    boolean _not = (!visible);
    if (_not) {
      String _projectName = this.getProjectName();
      this.info.setName(_projectName);
      IPath _locationPath = this.getLocationPath();
      this.info.setLocation(_locationPath);
    }
  }
}
