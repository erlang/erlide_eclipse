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
import org.eclipse.swt.widgets.Text;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.InputOutput;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.engine.model.builder.BuilderConfigType;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.builder.ErlangBuilder;
import org.erlide.engine.model.builder.IErlangBuilderFactory;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.ProjectConfig;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.ui.util.XtendSWTLib;
import org.erlide.ui.wizards.BuilderSelectionListener;
import org.erlide.ui.wizards.ConfigSelectionListener;
import org.erlide.ui.wizards.NewProjectData;

@SuppressWarnings("all")
public class ErlangProjectBuilderPage extends WizardPage {
  private NewProjectData info;
  
  protected Composite configComposite;
  
  protected Composite makeConfigComposite;
  
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
                it.setText(ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
                final String theText = it.getText();
                final ModifyListener _function_1 = new ModifyListener() {
                  public void modifyText(final ModifyEvent it) {
                    RuntimeVersion _runtimeVersion = new RuntimeVersion(theText);
                    ErlangProjectBuilderPage.this.info.setRequiredRuntimeVersion(_runtimeVersion);
                  }
                };
                it.addModifyListener(_function_1);
                RuntimeVersion _runtimeVersion = new RuntimeVersion(theText);
                ErlangProjectBuilderPage.this.info.setRequiredRuntimeVersion(_runtimeVersion);
              }
            };
            XtendSWTLib.<Combo>newControl(it, Combo.class, SWT.READ_ONLY, _function_1);
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
            String _name = BuilderTool.INTERNAL.name();
            ErlangProjectBuilderPage.this.info.setBuilderName(_name);
            final Procedure1<Composite> _function_8 = new Procedure1<Composite>() {
              public void apply(final Composite it) {
                try {
                  GridData _gridData = new GridData(SWT.NONE, SWT.NONE, false, false, 3, 1);
                  it.setLayoutData(_gridData);
                  GridLayout _gridLayout = new GridLayout(3, false);
                  it.setLayout(_gridLayout);
                  it.setVisible(false);
                  final Procedure1<Label> _function = new Procedure1<Label>() {
                    public void apply(final Label it) {
                      it.setText("The directory layout and the build \nconfiguration are described");
                    }
                  };
                  XtendSWTLib.<Label>newControl(it, Label.class, SWT.NONE, _function);
                  ConfigSelectionListener _configSelectionListener = new ConfigSelectionListener(ErlangProjectBuilderPage.this.info);
                  final ConfigSelectionListener configListener = _configSelectionListener;
                  final BuilderConfigType[] configs = BuilderConfigType.values();
                  final Procedure1<BuilderConfigType> _function_1 = new Procedure1<BuilderConfigType>() {
                    public void apply(final BuilderConfigType config) {
                      try {
                        final Procedure1<Button> _function = new Procedure1<Button>() {
                          public void apply(final Button it) {
                            String _description = ErlangProjectBuilderPage.this.getDescription(config);
                            it.setText(_description);
                            it.setData(config);
                            it.addSelectionListener(configListener);
                            boolean _tripleEquals = (config == BuilderConfigType.INTERNAL);
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
                  IterableExtensions.<BuilderConfigType>forEach(((Iterable<BuilderConfigType>)Conversions.doWrapArray(configs)), _function_1);
                } catch (Throwable _e) {
                  throw Exceptions.sneakyThrow(_e);
                }
              }
            };
            Composite _newControl = XtendSWTLib.<Composite>newControl(it, Composite.class, SWT.NONE, _function_8);
            ErlangProjectBuilderPage.this.configComposite = _newControl;
            String _name_1 = BuilderConfigType.INTERNAL.name();
            ErlangProjectBuilderPage.this.info.setBuilderConfigName(_name_1);
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
                      it.setText("compile");
                      GridData _gridData = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
                      it.setLayoutData(_gridData);
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
                      it.setText("clean");
                      GridData _gridData = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
                      final GridData gd = _gridData;
                      gd.widthHint = 250;
                      it.setLayoutData(gd);
                    }
                  };
                  XtendSWTLib.<Text>newControl(it, Text.class, SWT.BORDER, _function_7);
                } catch (Throwable _e) {
                  throw Exceptions.sneakyThrow(_e);
                }
              }
            };
            Composite _newControl_1 = XtendSWTLib.<Composite>newControl(it, Composite.class, SWT.NONE, _function_9);
            ErlangProjectBuilderPage.this.makeConfigComposite = _newControl_1;
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
  
  public String getDescription(final BuilderConfigType config) {
    String _switchResult = null;
    boolean _matched = false;
    if (!_matched) {
      if (Objects.equal(config,BuilderConfigType.INTERNAL)) {
        _matched=true;
        StringConcatenation _builder = new StringConcatenation();
        _builder.append("manually (on next page)");
        _switchResult = _builder.toString();
      }
    }
    if (!_matched) {
      if (Objects.equal(config,BuilderConfigType.EMAKE)) {
        _matched=true;
        StringConcatenation _builder_1 = new StringConcatenation();
        _builder_1.append("in Emakefile");
        _switchResult = _builder_1.toString();
      }
    }
    if (!_matched) {
      if (Objects.equal(config,BuilderConfigType.REBAR)) {
        _matched=true;
        StringConcatenation _builder_2 = new StringConcatenation();
        _builder_2.append("in rebar.config");
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
  
  public String detectBuilderConfig() {
    String _xblockexpression = null;
    {
      final IPath location = this.info.getLocation();
      String _xifexpression = null;
      boolean _tripleNotEquals = (location != null);
      if (_tripleNotEquals) {
        String _xblockexpression_1 = null;
        {
          String _portableString = location.toPortableString();
          File _file = new File(_portableString);
          final File directory = _file;
          String _xifexpression_1 = null;
          boolean _and = false;
          boolean _isDirectory = directory.isDirectory();
          if (!_isDirectory) {
            _and = false;
          } else {
            boolean _exists = directory.exists();
            _and = (_isDirectory && _exists);
          }
          if (_and) {
            String _xblockexpression_2 = null;
            {
              String _builderConfigName = this.info.getBuilderConfigName();
              final BuilderConfigType config = BuilderConfigType.valueOf(_builderConfigName);
              IErlangBuilderFactory _factory = ErlangBuilder.getFactory();
              final ProjectConfig persister = _factory.getConfig(config, directory);
              final ErlangProjectProperties props = persister.getConfiguration();
              String _println = InputOutput.<String>println(("PROPS: " + props));
              _xblockexpression_2 = (_println);
            }
            _xifexpression_1 = _xblockexpression_2;
          }
          _xblockexpression_1 = (_xifexpression_1);
        }
        _xifexpression = _xblockexpression_1;
      }
      _xblockexpression = (_xifexpression);
    }
    return _xblockexpression;
  }
}
