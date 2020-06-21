package org.erlide.ui.wizards;

import java.util.List;
import java.util.function.Consumer;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.engine.NewProjectData;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.ui.util.XtendSWTLib;
import org.erlide.util.SystemConfiguration;

@SuppressWarnings("all")
public class ErlangProjectBuilderPage extends ErlangWizardPage {
    private final NewProjectData info;

    protected Composite configComposite;

    protected Composite makeConfigComposite;

    private Combo runtimeCombo;

    protected ErlangProjectBuilderPage(final String pageName, final NewProjectData info) {
        super(pageName);
        this.info = info;
    }

    @Override
    public void createControl(final Composite parent) {
        try {
            final Procedure1<Composite> _function = (final Composite it) -> {
                try {
                    final GridLayout _gridLayout = new GridLayout(3, false);
                    it.setLayout(_gridLayout);
                    final Procedure1<Label> _function_1 = (final Label it_1) -> {
                        it_1.setText("Minimum Erlang version:");
                    };
                    XtendSWTLib.<Label> newControl(it, Label.class, SWT.NONE,
                            _function_1);
                    final Procedure1<Combo> _function_2 = (final Combo it_1) -> {
                        final RuntimeVersion[] runtimeVersions = ProjectPreferencesConstants.SUPPORTED_VERSIONS;
                        final Function1<RuntimeVersion, String> _function_3 = (
                                final RuntimeVersion it_2) -> it_2.toString();
                        it_1.setItems((String[]) Conversions
                                .unwrapArray(ListExtensions.<RuntimeVersion, String> map(
                                        (List<RuntimeVersion>) Conversions
                                                .doWrapArray(runtimeVersions),
                                        _function_3), String.class));
                        it_1.setText(
                                info.bestRuntime().getVersion().asMajor().toString());
                        info.setRequiredRuntimeVersion(
                                RuntimeVersion.Serializer.parse(it_1.getText()));
                    };
                    runtimeCombo = XtendSWTLib.<Combo> newControl(it, Combo.class,
                            SWT.READ_ONLY, _function_2);
                    final Procedure1<Label> _function_3 = (final Label it_1) -> {
                    };
                    XtendSWTLib.<Label> newControl(it, Label.class, SWT.NONE,
                            _function_3);
                    final Procedure1<Label> _function_4 = (final Label it_1) -> {
                    };
                    XtendSWTLib.<Label> newControl(it, Label.class, SWT.NONE,
                            _function_4);
                    final Procedure1<Label> _function_5 = (final Label it_1) -> {
                    };
                    XtendSWTLib.<Label> newControl(it, Label.class, SWT.NONE,
                            _function_5);
                    final Procedure1<Label> _function_6 = (final Label it_1) -> {
                    };
                    XtendSWTLib.<Label> newControl(it, Label.class, SWT.NONE,
                            _function_6);
                    final Procedure1<Label> _function_7 = (final Label it_1) -> {
                        final GridData _gridData = new GridData(SWT.NONE, SWT.NONE, false,
                                false, 3, 1);
                        it_1.setLayoutData(_gridData);
                        it_1.setText(
                                "Build system to be used (disabled options will be implemented soon): ");
                    };
                    XtendSWTLib.<Label> newControl(it, Label.class, SWT.NONE,
                            _function_7);
                    final Procedure1<Composite> _function_8 = (final Composite it_1) -> {
                        final GridData _gridData = new GridData(SWT.NONE, SWT.NONE, false,
                                false, 3, 1);
                        it_1.setLayoutData(_gridData);
                        final GridLayout _gridLayout_1 = new GridLayout(3, false);
                        it_1.setLayout(_gridLayout_1);
                        final BuilderSelectionListener builderListener = new BuilderSelectionListener(
                                info, this);
                        final BuilderTool[] builders = BuilderTool.values();
                        final Consumer<BuilderTool> _function_9 = (
                                final BuilderTool builder) -> {
                            try {
                                final Procedure1<Label> _function_10 = (
                                        final Label it_2) -> {
                                    it_2.setText("        ");
                                };
                                XtendSWTLib.<Label> newControl(it_1, Label.class,
                                        SWT.NONE, _function_10);
                                final Procedure1<Button> _function_11 = (
                                        final Button it_2) -> {
                                    it_2.setText(builder.toString().toLowerCase());
                                    it_2.setData(builder);
                                    it_2.addSelectionListener(builderListener);
                                    it_2.setSelection(builder == BuilderTool.INTERNAL);
                                    if (!SystemConfiguration
                                            .hasFeatureEnabled("erlide.newbuilders")
                                            && builder != BuilderTool.INTERNAL) {
                                        it_2.setEnabled(false);
                                    }
                                };
                                XtendSWTLib.<Button> newControl(it_1, Button.class,
                                        SWT.RADIO, _function_11);
                                final Procedure1<Label> _function_12 = (
                                        final Label it_2) -> {
                                    it_2.setText(this.getDescription(builder));
                                    if (!SystemConfiguration
                                            .hasFeatureEnabled("erlide.newbuilders")
                                            && builder != BuilderTool.INTERNAL) {
                                        it_2.setEnabled(false);
                                    }
                                };
                                XtendSWTLib.<Label> newControl(it_1, Label.class,
                                        SWT.NONE, _function_12);
                            } catch (final Throwable _e) {
                                throw Exceptions.sneakyThrow(_e);
                            }
                        };
                        ((List<BuilderTool>) Conversions.doWrapArray(builders))
                                .forEach(_function_9);
                    };
                    XtendSWTLib.<Composite> newControl(it, Composite.class, SWT.NONE,
                            _function_8);
                    final Procedure1<Label> _function_9 = (final Label it_1) -> {
                        final GridData _gridData = new GridData(SWT.NONE, SWT.NONE, false,
                                false, 3, 1);
                        it_1.setLayoutData(_gridData);
                        it_1.setText(
                                "The directory layout and the build configuration are described:");
                    };
                    XtendSWTLib.<Label> newControl(it, Label.class, SWT.NONE,
                            _function_9);
                    info.setBuilder(BuilderTool.INTERNAL);
                    final Procedure1<Composite> _function_10 = (final Composite it_1) -> {
                        final GridData _gridData = new GridData(SWT.NONE, SWT.NONE, false,
                                false, 3, 1);
                        it_1.setLayoutData(_gridData);
                        final GridLayout _gridLayout_1 = new GridLayout(3, false);
                        it_1.setLayout(_gridLayout_1);
                        final ConfigSelectionListener configListener = new ConfigSelectionListener(
                                info);
                        final ProjectConfigType[] configs = ProjectConfigType.values();
                        final Consumer<ProjectConfigType> _function_11 = (
                                final ProjectConfigType config) -> {
                            try {
                                final Procedure1<Label> _function_12 = (
                                        final Label it_2) -> {
                                    it_2.setText("        ");
                                };
                                XtendSWTLib.<Label> newControl(it_1, Label.class,
                                        SWT.NONE, _function_12);
                                final Procedure1<Button> _function_13 = (
                                        final Button it_2) -> {
                                    it_2.setText(this.getDescription(config));
                                    it_2.setData(config);
                                    it_2.addSelectionListener(configListener);
                                    it_2.setSelection(
                                            config == ProjectConfigType.INTERNAL);
                                    if (!SystemConfiguration
                                            .hasFeatureEnabled("erlide.newbuilders")
                                            && config != ProjectConfigType.INTERNAL) {
                                        it_2.setEnabled(false);
                                    }
                                };
                                XtendSWTLib.<Button> newControl(it_1, Button.class,
                                        SWT.RADIO, _function_13);
                                final Procedure1<Label> _function_14 = (
                                        final Label it_2) -> {
                                };
                                XtendSWTLib.<Label> newControl(it_1, Label.class,
                                        SWT.NONE, _function_14);
                            } catch (final Throwable _e) {
                                throw Exceptions.sneakyThrow(_e);
                            }
                        };
                        ((List<ProjectConfigType>) Conversions.doWrapArray(configs))
                                .forEach(_function_11);
                    };
                    configComposite = XtendSWTLib.<Composite> newControl(it,
                            Composite.class, SWT.NONE, _function_10);
                    info.setConfigType(ProjectConfigType.INTERNAL);
                    final Procedure1<Composite> _function_11 = (final Composite it_1) -> {
                        try {
                            final GridData _gridData = new GridData(SWT.NONE, SWT.NONE,
                                    false, false, 3, 1);
                            it_1.setLayoutData(_gridData);
                            final GridLayout _gridLayout_1 = new GridLayout(3, false);
                            it_1.setLayout(_gridLayout_1);
                            it_1.setVisible(false);
                            final Procedure1<Label> _function_12 = (final Label it_2) -> {
                                final GridData gd = new GridData(SWT.LEFT, SWT.CENTER,
                                        false, false, 2, 1);
                                gd.widthHint = 163;
                                it_2.setLayoutData(gd);
                                it_2.setText("Make uses these targets");
                            };
                            XtendSWTLib.<Label> newControl(it_1, Label.class, SWT.NONE,
                                    _function_12);
                            final Procedure1<Label> _function_13 = (final Label it_2) -> {
                            };
                            XtendSWTLib.<Label> newControl(it_1, Label.class, SWT.NONE,
                                    _function_13);
                            final Procedure1<Label> _function_14 = (final Label it_2) -> {
                            };
                            XtendSWTLib.<Label> newControl(it_1, Label.class, SWT.NONE,
                                    _function_14);
                            final Procedure1<Label> _function_15 = (final Label it_2) -> {
                                it_2.setText("- to compile project:");
                            };
                            XtendSWTLib.<Label> newControl(it_1, Label.class, SWT.NONE,
                                    _function_15);
                            final Procedure1<Text> _function_16 = (final Text it_2) -> {
                                final GridData _gridData_1 = new GridData(SWT.FILL,
                                        SWT.CENTER, true, false, 1, 1);
                                it_2.setLayoutData(_gridData_1);
                                it_2.setEnabled(false);
                                final ModifyListener _function_17 = (
                                        final ModifyEvent l) -> {
                                    info.getBuilderData().put("compile", it_2.getText());
                                };
                                it_2.addModifyListener(_function_17);
                                it_2.setText("compile");
                            };
                            XtendSWTLib.<Text> newControl(it_1, Text.class, SWT.BORDER,
                                    _function_16);
                            final Procedure1<Label> _function_17 = (final Label it_2) -> {
                            };
                            XtendSWTLib.<Label> newControl(it_1, Label.class, SWT.NONE,
                                    _function_17);
                            final Procedure1<Label> _function_18 = (final Label it_2) -> {
                                it_2.setText("- to clean project:");
                            };
                            XtendSWTLib.<Label> newControl(it_1, Label.class, SWT.NONE,
                                    _function_18);
                            final Procedure1<Text> _function_19 = (final Text it_2) -> {
                                final GridData gd = new GridData(SWT.FILL, SWT.CENTER,
                                        true, false, 1, 1);
                                gd.widthHint = 250;
                                it_2.setLayoutData(gd);
                                it_2.setEnabled(false);
                                final ModifyListener _function_20 = (
                                        final ModifyEvent l) -> {
                                    info.getBuilderData().put("clean", it_2.getText());
                                };
                                it_2.addModifyListener(_function_20);
                                it_2.setText("clean");
                            };
                            XtendSWTLib.<Text> newControl(it_1, Text.class, SWT.BORDER,
                                    _function_19);
                        } catch (final Throwable _e) {
                            throw Exceptions.sneakyThrow(_e);
                        }
                    };
                    makeConfigComposite = XtendSWTLib.<Composite> newControl(it,
                            Composite.class, SWT.NONE, _function_11);
                } catch (final Throwable _e) {
                    throw Exceptions.sneakyThrow(_e);
                }
            };
            final Composite composite = XtendSWTLib.<Composite> newControl(parent,
                    Composite.class, SWT.NONE, _function);
            setControl(composite);
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    public String getDescription(final BuilderTool builder) {
        String _switchResult = null;
        if (builder != null) {
            switch (builder) {
            case INTERNAL:
                final StringConcatenation _builder = new StringConcatenation();
                _builder.append(": let erlide do the compiling.");
                _switchResult = _builder.toString();
                break;
            case MAKE:
                final StringConcatenation _builder_1 = new StringConcatenation();
                _builder_1.append(
                        ": choose this if there is a Makefile (even if it calls rebar or emake).");
                _switchResult = _builder_1.toString();
                break;
            case EMAKE:
                final StringConcatenation _builder_2 = new StringConcatenation();
                _builder_2.append(": straight Emake.");
                _switchResult = _builder_2.toString();
                break;
            case REBAR:
                final StringConcatenation _builder_3 = new StringConcatenation();
                _builder_3.append(": straight rebar.");
                _switchResult = _builder_3.toString();
                break;
            default:
                break;
            }
        }
        return _switchResult;
    }

    public String getDescription(final ProjectConfigType config) {
        String _switchResult = null;
        if (config != null) {
            switch (config) {
            case INTERNAL:
                final StringConcatenation _builder = new StringConcatenation();
                _builder.append("manually (on next page)");
                _switchResult = _builder.toString();
                break;
            case EMAKE:
                final StringConcatenation _builder_1 = new StringConcatenation();
                _builder_1.append("in Emakefile");
                _switchResult = _builder_1.toString();
                break;
            case REBAR:
                final StringConcatenation _builder_2 = new StringConcatenation();
                _builder_2.append("in rebar.config");
                _switchResult = _builder_2.toString();
                break;
            default:
                break;
            }
        }
        return _switchResult;
    }

    @Override
    protected void onEntry() {
        final boolean _isExistingProject = info.isExistingProject();
        if (_isExistingProject) {
            info.detectProjectConfig();
        }
    }

    @Override
    protected void onExit() {
        info.setRequiredRuntimeVersion(
                RuntimeVersion.Serializer.parse(runtimeCombo.getText()));
    }

    public void selectConfig(final ProjectConfigType type) {
        final Consumer<Control> _function = (final Control it) -> {
            if (it instanceof Button) {
                final boolean _equals = type.equals(((Button) it).getData());
                if (_equals) {
                    final Event event = new Event();
                    event.widget = it;
                    ((Button) it).notifyListeners(SWT.Selection, event);
                }
            }
        };
        ((List<Control>) Conversions.doWrapArray(configComposite.getChildren()))
                .forEach(_function);
    }
}
