package org.erlide.ui.wizards;

import com.google.common.base.Objects;
import java.io.File;
import java.util.List;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.engine.model.builder.BuilderConfig;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.ui.wizards.BuilderSelectionListener;
import org.erlide.ui.wizards.NewProjectData;

@SuppressWarnings("all")
public class ErlangProjectBuilderPage extends WizardPage {
  private NewProjectData info;
  
  protected Composite configComposite;
  
  protected ErlangProjectBuilderPage(final String pageName, final NewProjectData info) {
    super(pageName);
    this.info = info;
  }
  
  public void createControl(final Composite parent) {
    Composite _composite = new Composite(parent, SWT.NONE);
    final Composite composite = _composite;
    this.setControl(composite);
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
        ErlangProjectBuilderPage.this.info.setRuntimeVersion(_runtimeVersion);
      }
    };
    version.addModifyListener(_function_1);
    String _text = version.getText();
    RuntimeVersion _runtimeVersion = new RuntimeVersion(_text);
    this.info.setRuntimeVersion(_runtimeVersion);
    new Label(composite, SWT.NONE);
    new Label(composite, SWT.NONE);
    new Label(composite, SWT.NONE);
    new Label(composite, SWT.NONE);
    Label _label_1 = new Label(composite, SWT.NONE);
    final Label label = _label_1;
    label.setText("Build system to be used:");
    BuilderSelectionListener _builderSelectionListener = new BuilderSelectionListener(this.info, this);
    final BuilderSelectionListener listener = _builderSelectionListener;
    final BuilderTool[] builders = BuilderTool.values();
    final Procedure1<BuilderTool> _function_2 = new Procedure1<BuilderTool>() {
      public void apply(final BuilderTool builder) {
        Button _button = new Button(composite, SWT.RADIO);
        Button check = _button;
        String _string = builder.toString();
        String _lowerCase = _string.toLowerCase();
        check.setText(_lowerCase);
        check.setData(builder);
        boolean _tripleEquals = (builder == BuilderTool.INTERNAL);
        check.setSelection(_tripleEquals);
        check.addSelectionListener(listener);
        Label _label = new Label(composite, SWT.NONE);
        final Label description = _label;
        String _description = ErlangProjectBuilderPage.this.getDescription(builder);
        description.setText(_description);
        new Label(composite, SWT.NONE);
      }
    };
    IterableExtensions.<BuilderTool>forEach(((Iterable<BuilderTool>)Conversions.doWrapArray(builders)), _function_2);
    String _name = BuilderTool.INTERNAL.name();
    this.info.setBuilderName(_name);
    Composite _composite_1 = new Composite(composite, SWT.NONE);
    this.configComposite = _composite_1;
    GridData _gridData = new GridData(SWT.NONE, SWT.NONE, false, false, 3, 1);
    this.configComposite.setLayoutData(_gridData);
    GridLayout _gridLayout_1 = new GridLayout(3, false);
    this.configComposite.setLayout(_gridLayout_1);
    this.configComposite.setVisible(false);
    Label _label_2 = new Label(this.configComposite, SWT.NONE);
    final Label label1 = _label_2;
    label1.setText("The directory layout is described");
    BuilderSelectionListener _builderSelectionListener_1 = new BuilderSelectionListener(this.info);
    final BuilderSelectionListener listener1 = _builderSelectionListener_1;
    final BuilderConfig[] configs = BuilderConfig.values();
    final Procedure1<BuilderConfig> _function_3 = new Procedure1<BuilderConfig>() {
      public void apply(final BuilderConfig config) {
        Button _button = new Button(ErlangProjectBuilderPage.this.configComposite, SWT.RADIO);
        Button check = _button;
        String _description = ErlangProjectBuilderPage.this.getDescription(config);
        check.setText(_description);
        check.setData(config);
        boolean _tripleEquals = (config == BuilderConfig.INTERNAL);
        check.setSelection(_tripleEquals);
        check.addSelectionListener(listener1);
        new Label(ErlangProjectBuilderPage.this.configComposite, SWT.NONE);
        new Label(ErlangProjectBuilderPage.this.configComposite, SWT.NONE);
      }
    };
    IterableExtensions.<BuilderConfig>forEach(((Iterable<BuilderConfig>)Conversions.doWrapArray(configs)), _function_3);
    String _name_1 = BuilderConfig.INTERNAL.name();
    this.info.setBuilderConfig(_name_1);
  }
  
  public String getDescription(final BuilderTool builder) {
    String _switchResult = null;
    boolean _matched = false;
    if (!_matched) {
      if (Objects.equal(builder,BuilderTool.INTERNAL)) {
        _matched=true;
        StringConcatenation _builder = new StringConcatenation();
        _builder.append(": let erlide do the compiling.");
        _switchResult = _builder.toString();
      }
    }
    if (!_matched) {
      if (Objects.equal(builder,BuilderTool.MAKE)) {
        _matched=true;
        StringConcatenation _builder_1 = new StringConcatenation();
        _builder_1.append(": choose this if there is a Makefile (even if it calls rebar or emake).");
        _switchResult = _builder_1.toString();
      }
    }
    if (!_matched) {
      if (Objects.equal(builder,BuilderTool.EMAKE)) {
        _matched=true;
        StringConcatenation _builder_2 = new StringConcatenation();
        _builder_2.append(": straight Emake.");
        _switchResult = _builder_2.toString();
      }
    }
    if (!_matched) {
      if (Objects.equal(builder,BuilderTool.REBAR)) {
        _matched=true;
        StringConcatenation _builder_3 = new StringConcatenation();
        _builder_3.append(": straight rebar.");
        _switchResult = _builder_3.toString();
      }
    }
    return _switchResult;
  }
  
  public String getDescription(final BuilderConfig config) {
    String _switchResult = null;
    boolean _matched = false;
    if (!_matched) {
      if (Objects.equal(config,BuilderConfig.INTERNAL)) {
        _matched=true;
        StringConcatenation _builder = new StringConcatenation();
        _builder.append("manually");
        _switchResult = _builder.toString();
      }
    }
    if (!_matched) {
      if (Objects.equal(config,BuilderConfig.EMAKE)) {
        _matched=true;
        StringConcatenation _builder_1 = new StringConcatenation();
        _builder_1.append("Emakefile");
        _switchResult = _builder_1.toString();
      }
    }
    if (!_matched) {
      if (Objects.equal(config,BuilderConfig.REBAR)) {
        _matched=true;
        StringConcatenation _builder_2 = new StringConcatenation();
        _builder_2.append("rebar.config");
        _switchResult = _builder_2.toString();
      }
    }
    return _switchResult;
  }
  
  public void setVisible(final boolean visible) {
    super.setVisible(visible);
    if (visible) {
      this.detectBuilderConfig();
    }
  }
  
  public Object detectBuilderConfig() {
    Object _xblockexpression = null;
    {
      final IPath location = this.info.getLocation();
      Object _xifexpression = null;
      boolean _and = false;
      boolean _notEquals = (!Objects.equal(location, null));
      if (!_notEquals) {
        _and = false;
      } else {
        String _portableString = location.toPortableString();
        File _file = new File(_portableString);
        boolean _exists = _file.exists();
        _and = (_notEquals && _exists);
      }
      if (_and) {
        _xifexpression = null;
      }
      _xblockexpression = (_xifexpression);
    }
    return _xblockexpression;
  }
}
