package org.erlide.ui.wizards;

import com.google.common.base.Objects;
import java.util.List;
import java.util.Map;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.NewProjectData;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.ui.util.XtendSWTLib;
import org.erlide.ui.wizards.BuilderSelectionListener;
import org.erlide.ui.wizards.ConfigSelectionListener;
import org.erlide.ui.wizards.ErlangWizardPage;

@SuppressWarnings("all")
public class ErlangProjectBuilderPage extends ErlangWizardPage {
  private NewProjectData info;
  
  protected Composite configComposite;
  
  protected Composite makeConfigComposite;
  
  private Combo runtimeCombo;
  
  protected ErlangProjectBuilderPage(final String pageName, final NewProjectData info) {
    super(pageName);
    this.info = info;
  }
  
  public void createControl(final Composite parent) {
    try {
      final Procedure1<Composite> _function = new Procedure1<Composite>() {
        public void apply(final Composite it) {
          try {
            GridLayout _gridLayout = new GridLayout(3, false);
            it.setLayout(_gridLayout);
            final Procedure1<Label> _function = new Procedure1<Label>() {
              public void apply(final Label it) {
                it.setText("Minimum Erlang version:");
              }
            };
            XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function);
            final Procedure1<Combo> _function_1 = new Procedure1<Combo>() {
              public void apply(final Combo it) {
                final RuntimeVersion[] runtimeVersions = ProjectPreferencesConstants.SUPPORTED_VERSIONS;
                final Function1<RuntimeVersion,String> _function = new Function1<RuntimeVersion,String>() {
                  public String apply(final RuntimeVersion it) {
                    String _string = it.toString();
                    return _string;
                  }
                };
                List<String> _map = ListExtensions.<RuntimeVersion, String>map(((List<RuntimeVersion>)Conversions.doWrapArray(runtimeVersions)), _function);
                it.setItems(((String[])Conversions.unwrapArray(_map, String.class)));
                RuntimeInfo _bestRuntime = ErlangProjectBuilderPage.this.info.bestRuntime();
                RuntimeVersion _version = _bestRuntime.getVersion();
                RuntimeVersion _asMajor = _version.asMajor();
                String _string = _asMajor.toString();
                it.setText(_string);
                String _text = it.getText();
                RuntimeVersion _runtimeVersion = new RuntimeVersion(_text);
                ErlangProjectBuilderPage.this.info.setRequiredRuntimeVersion(_runtimeVersion);
              }
            };
            Combo _newControl = XtendSWTLib.<Combo>newControl(it, Combo.class, SWT.READ_ONLY, _function_1);
            ErlangProjectBuilderPage.this.runtimeCombo = _newControl;
            final Procedure1<Label> _function_2 = new Procedure1<Label>() {
              public void apply(final Label it) {
              }
            };
            XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function_2);
            final Procedure1<Label> _function_3 = new Procedure1<Label>() {
              public void apply(final Label it) {
              }
            };
            XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function_3);
            final Procedure1<Label> _function_4 = new Procedure1<Label>() {
              public void apply(final Label it) {
              }
            };
            XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function_4);
            final Procedure1<Label> _function_5 = new Procedure1<Label>() {
              public void apply(final Label it) {
              }
            };
            XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function_5);
            final Procedure1<Label> _function_6 = new Procedure1<Label>() {
              public void apply(final Label it) {
                it.setText("Build system to be used:");
              }
            };
            XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function_6);
            BuilderSelectionListener _builderSelectionListener = new BuilderSelectionListener(ErlangProjectBuilderPage.this.info, ErlangProjectBuilderPage.this);
            final BuilderSelectionListener builderListener = _builderSelectionListener;
            final BuilderTool[] builders = BuilderTool.values();
            final Procedure1<BuilderTool> _function_7 = new Procedure1<BuilderTool>() {
              public void apply(final BuilderTool builder) {
                try {
                  final Procedure1<Button> _function = new Procedure1<Button>() {
                    public void apply(final Button it) {
                      String _string = builder.toString();
                      String _lowerCase = _string.toLowerCase();
                      it.setText(_lowerCase);
                      it.setData(builder);
                      it.addSelectionListener(builderListener);
                      boolean _tripleEquals = (builder == BuilderTool.INTERNAL);
                      it.setSelection(_tripleEquals);
                    }
                  };
                  XtendSWTLib.<Button>newControl(it, Button.class, SWT.RADIO, _function);
                  final Procedure1<Label> _function_1 = new Procedure1<Label>() {
                    public void apply(final Label it) {
                      String _description = ErlangProjectBuilderPage.this.getDescription(builder);
                      it.setText(_description);
                    }
                  };
                  XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function_1);
                  final Procedure1<Label> _function_2 = new Procedure1<Label>() {
                    public void apply(final Label it) {
                    }
                  };
                  XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function_2);
                } catch (Throwable _e) {
                  throw Exceptions.sneakyThrow(_e);
                }
              }
            };
            IterableExtensions.<BuilderTool>forEach(((Iterable<BuilderTool>)Conversions.doWrapArray(builders)), _function_7);
            ErlangProjectBuilderPage.this.info.setBuilder(BuilderTool.INTERNAL);
            final Procedure1<Composite> _function_8 = new Procedure1<Composite>() {
              public void apply(final Composite it) {
                try {
                  GridData _gridData = new GridData(SWT.NONE, SWT.NONE, false, false, 3, 1);
                  it.setLayoutData(_gridData);
                  GridLayout _gridLayout = new GridLayout(3, false);
                  it.setLayout(_gridLayout);
                  final Procedure1<Label> _function = new Procedure1<Label>() {
                    public void apply(final Label it) {
                      it.setText("The directory layout and the build \nconfiguration are described");
                    }
                  };
                  XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function);
                  ConfigSelectionListener _configSelectionListener = new ConfigSelectionListener(ErlangProjectBuilderPage.this.info);
                  final ConfigSelectionListener configListener = _configSelectionListener;
                  final ProjectConfigType[] configs = ProjectConfigType.values();
                  final Procedure1<ProjectConfigType> _function_1 = new Procedure1<ProjectConfigType>() {
                    public void apply(final ProjectConfigType config) {
                      try {
                        final Procedure1<Button> _function = new Procedure1<Button>() {
                          public void apply(final Button it) {
                            String _description = ErlangProjectBuilderPage.this.getDescription(config);
                            it.setText(_description);
                            it.setData(config);
                            it.addSelectionListener(configListener);
                            boolean _tripleEquals = (config == ProjectConfigType.INTERNAL);
                            it.setSelection(_tripleEquals);
                          }
                        };
                        XtendSWTLib.<Button>newControl(it, Button.class, SWT.RADIO, _function);
                        final Procedure1<Label> _function_1 = new Procedure1<Label>() {
                          public void apply(final Label it) {
                          }
                        };
                        XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function_1);
                        final Procedure1<Label> _function_2 = new Procedure1<Label>() {
                          public void apply(final Label it) {
                          }
                        };
                        XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function_2);
                      } catch (Throwable _e) {
                        throw Exceptions.sneakyThrow(_e);
                      }
                    }
                  };
                  IterableExtensions.<ProjectConfigType>forEach(((Iterable<ProjectConfigType>)Conversions.doWrapArray(configs)), _function_1);
                } catch (Throwable _e) {
                  throw Exceptions.sneakyThrow(_e);
                }
              }
            };
            Composite _newControl_1 = XtendSWTLib.<Composite>newControl(it, Composite.class, SWT.NONE, _function_8);
            ErlangProjectBuilderPage.this.configComposite = _newControl_1;
            ErlangProjectBuilderPage.this.info.setConfigType(ProjectConfigType.INTERNAL);
            final Procedure1<Composite> _function_9 = new Procedure1<Composite>() {
              public void apply(final Composite it) {
                try {
                  GridData _gridData = new GridData(SWT.NONE, SWT.NONE, false, false, 3, 1);
                  it.setLayoutData(_gridData);
                  GridLayout _gridLayout = new GridLayout(3, false);
                  it.setLayout(_gridLayout);
                  it.setVisible(false);
                  final Procedure1<Label> _function = new Procedure1<Label>() {
                    public void apply(final Label it) {
                      GridData _gridData = new GridData(SWT.LEFT, SWT.CENTER, false, false, 2, 1);
                      final GridData gd = _gridData;
                      gd.widthHint = 163;
                      it.setLayoutData(gd);
                      it.setText("Make uses these targets");
                    }
                  };
                  XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function);
                  final Procedure1<Label> _function_1 = new Procedure1<Label>() {
                    public void apply(final Label it) {
                    }
                  };
                  XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function_1);
                  final Procedure1<Label> _function_2 = new Procedure1<Label>() {
                    public void apply(final Label it) {
                    }
                  };
                  XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function_2);
                  final Procedure1<Label> _function_3 = new Procedure1<Label>() {
                    public void apply(final Label it) {
                      it.setText("- to compile project:");
                    }
                  };
                  XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function_3);
                  final Procedure1<Text> _function_4 = new Procedure1<Text>() {
                    public void apply(final Text it) {
                      GridData _gridData = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
                      it.setLayoutData(_gridData);
                      final ModifyListener _function = new ModifyListener() {
                        public void modifyText(final ModifyEvent l) {
                          Map<String,String> _builderData = ErlangProjectBuilderPage.this.info.getBuilderData();
                          String _text = it.getText();
                          _builderData.put("compile", _text);
                        }
                      };
                      it.addModifyListener(_function);
                      it.setText("compile");
                    }
                  };
                  XtendSWTLib.<Text>newControl(it, Text.class, SWT.BORDER, _function_4);
                  final Procedure1<Label> _function_5 = new Procedure1<Label>() {
                    public void apply(final Label it) {
                    }
                  };
                  XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function_5);
                  final Procedure1<Label> _function_6 = new Procedure1<Label>() {
                    public void apply(final Label it) {
                      it.setText("- to clean project:");
                    }
                  };
                  XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function_6);
                  final Procedure1<Text> _function_7 = new Procedure1<Text>() {
                    public void apply(final Text it) {
                      GridData _gridData = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
                      final GridData gd = _gridData;
                      gd.widthHint = 250;
                      it.setLayoutData(gd);
                      final ModifyListener _function = new ModifyListener() {
                        public void modifyText(final ModifyEvent l) {
                          Map<String,String> _builderData = ErlangProjectBuilderPage.this.info.getBuilderData();
                          String _text = it.getText();
                          _builderData.put("clean", _text);
                        }
                      };
                      it.addModifyListener(_function);
                      it.setText("clean");
                    }
                  };
                  XtendSWTLib.<Text>newControl(it, Text.class, SWT.BORDER, _function_7);
                } catch (Throwable _e) {
                  throw Exceptions.sneakyThrow(_e);
                }
              }
            };
            Composite _newControl_2 = XtendSWTLib.<Composite>newControl(it, Composite.class, SWT.NONE, _function_9);
            ErlangProjectBuilderPage.this.makeConfigComposite = _newControl_2;
          } catch (Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
          }
        }
      };
      final Composite composite = XtendSWTLib.<Composite>newControl(parent, Composite.class, SWT.NONE, _function);
      this.setControl(composite);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
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
  
  public String getDescription(final ProjectConfigType config) {
    String _switchResult = null;
    boolean _matched = false;
    if (!_matched) {
      if (Objects.equal(config,ProjectConfigType.INTERNAL)) {
        _matched=true;
        StringConcatenation _builder = new StringConcatenation();
        _builder.append("manually (on next page)");
        _switchResult = _builder.toString();
      }
    }
    if (!_matched) {
      if (Objects.equal(config,ProjectConfigType.EMAKE)) {
        _matched=true;
        StringConcatenation _builder_1 = new StringConcatenation();
        _builder_1.append("in Emakefile");
        _switchResult = _builder_1.toString();
      }
    }
    if (!_matched) {
      if (Objects.equal(config,ProjectConfigType.REBAR)) {
        _matched=true;
        StringConcatenation _builder_2 = new StringConcatenation();
        _builder_2.append("in rebar.config");
        _switchResult = _builder_2.toString();
      }
    }
    return _switchResult;
  }
  
  protected void onEntry() {
    boolean _isExistingProject = this.info.isExistingProject();
    if (_isExistingProject) {
      this.info.detectProjectConfig();
    }
  }
  
  protected void onExit() {
    String _text = this.runtimeCombo.getText();
    RuntimeVersion _runtimeVersion = new RuntimeVersion(_text);
    this.info.setRequiredRuntimeVersion(_runtimeVersion);
  }
}
