package org.erlide.ui.wizards;

import com.google.common.base.Objects;
import java.util.List;
import org.eclipse.core.runtime.IPath;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.engine.model.builder.BuilderInfo;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.ui.wizards.BuilderSelectionListener;
import org.erlide.ui.wizards.NewProjectData;

@SuppressWarnings("all")
public class ErlangNewProjectCreationPage extends WizardNewProjectCreationPage {
  private final NewProjectData info;
  
  public ErlangNewProjectCreationPage(final String name, final NewProjectData info) {
    super(name);
    this.info = info;
  }
  
  public void createControl(final Composite parent) {
    super.createControl(parent);
    Control _control = this.getControl();
    Composite _composite = new Composite(((Composite) _control), SWT.NONE);
    final Composite composite = _composite;
    GridLayout _gridLayout = new GridLayout(3, false);
    composite.setLayout(_gridLayout);
    Label _label = new Label(composite, SWT.NONE);
    final Label label2 = _label;
    label2.setText("Minimum Erlang version:");
    Combo _combo = new Combo(composite, SWT.READ_ONLY);
    final Combo version = _combo;
    final RuntimeVersion[] runtimeVersions = ProjectPreferencesConstants.SUPPORTED_VERSIONS;
    final Function1<RuntimeVersion,String> _function = new Function1<RuntimeVersion,String>() {
      public String apply(final RuntimeVersion it) {
        String _string = it.toString();
        return _string;
      }
    };
    List<String> _map = ListExtensions.<RuntimeVersion, String>map(((List<RuntimeVersion>)Conversions.doWrapArray(runtimeVersions)), _function);
    version.setItems(((String[])Conversions.unwrapArray(_map, String.class)));
    version.setText(ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
    final ModifyListener _function_1 = new ModifyListener() {
      public void modifyText(final ModifyEvent it) {
        String _text = version.getText();
        RuntimeVersion _runtimeVersion = new RuntimeVersion(_text);
        ErlangNewProjectCreationPage.this.info.setRuntimeVersion(_runtimeVersion);
      }
    };
    version.addModifyListener(_function_1);
    String _text = version.getText();
    RuntimeVersion _runtimeVersion = new RuntimeVersion(_text);
    this.info.setRuntimeVersion(_runtimeVersion);
    new Label(composite, SWT.NONE);
    Label _label_1 = new Label(composite, SWT.NONE);
    final Label label = _label_1;
    label.setText("Build system to be used:");
    BuilderSelectionListener _builderSelectionListener = new BuilderSelectionListener(this.info);
    final BuilderSelectionListener listener = _builderSelectionListener;
    final BuilderInfo[] builders = BuilderInfo.values();
    final Procedure1<BuilderInfo> _function_2 = new Procedure1<BuilderInfo>() {
      public void apply(final BuilderInfo builder) {
        Button _button = new Button(composite, SWT.RADIO);
        Button check = _button;
        String _string = builder.toString();
        String _lowerCase = _string.toLowerCase();
        check.setText(_lowerCase);
        check.setData(builder);
        boolean _tripleEquals = (builder == BuilderInfo.INTERNAL);
        if (_tripleEquals) {
          check.setSelection(true);
        }
        check.addSelectionListener(listener);
        Label _label = new Label(composite, SWT.NONE);
        final Label description = _label;
        String _description = ErlangNewProjectCreationPage.this.getDescription(builder);
        description.setText(_description);
        new Label(composite, SWT.NONE);
      }
    };
    IterableExtensions.<BuilderInfo>forEach(((Iterable<BuilderInfo>)Conversions.doWrapArray(builders)), _function_2);
    String _string = BuilderInfo.INTERNAL.toString();
    String _upperCase = _string.toUpperCase();
    this.info.setBuilderName(_upperCase);
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
  
  public String getDescription(final BuilderInfo builder) {
    String _switchResult = null;
    boolean _matched = false;
    if (!_matched) {
      if (Objects.equal(builder,BuilderInfo.INTERNAL)) {
        _matched=true;
        StringConcatenation _builder = new StringConcatenation();
        _builder.append(": let erlide do the compiling.");
        _switchResult = _builder.toString();
      }
    }
    if (!_matched) {
      if (Objects.equal(builder,BuilderInfo.MAKE)) {
        _matched=true;
        StringConcatenation _builder_1 = new StringConcatenation();
        _builder_1.append(": choose this if there is a Makefile (even if it calls rebar or emake).");
        _switchResult = _builder_1.toString();
      }
    }
    if (!_matched) {
      if (Objects.equal(builder,BuilderInfo.EMAKE)) {
        _matched=true;
        StringConcatenation _builder_2 = new StringConcatenation();
        _builder_2.append(": straight Emake.");
        _switchResult = _builder_2.toString();
      }
    }
    if (!_matched) {
      if (Objects.equal(builder,BuilderInfo.REBAR)) {
        _matched=true;
        StringConcatenation _builder_3 = new StringConcatenation();
        _builder_3.append(": straight rebar.");
        _switchResult = _builder_3.toString();
      }
    }
    return _switchResult;
  }
}
