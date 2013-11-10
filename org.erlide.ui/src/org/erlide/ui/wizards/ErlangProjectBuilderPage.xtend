package org.erlide.ui.wizards

import java.io.File
import org.eclipse.jface.wizard.WizardPage
import org.eclipse.swt.SWT
import org.eclipse.swt.events.SelectionEvent
import org.eclipse.swt.events.SelectionListener
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.layout.GridLayout
import org.eclipse.swt.widgets.Button
import org.eclipse.swt.widgets.Combo
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Label
import org.erlide.engine.model.builder.BuilderConfig
import org.erlide.engine.model.builder.BuilderTool
import org.erlide.engine.model.root.ProjectPreferencesConstants
import org.erlide.runtime.runtimeinfo.RuntimeVersion
import org.erlide.engine.model.builder.ErlangBuilder

class ErlangProjectBuilderPage extends WizardPage {

    NewProjectData info
    protected Composite configComposite

    protected new(String pageName, NewProjectData info) {
        super(pageName)
        this.info = info;
    }

    override createControl(Composite parent) {
        val composite = new Composite(parent, SWT.NONE)
        setControl(composite);
        composite.layout = new GridLayout(3, false)

        val label2 = new Label(composite, SWT.NONE)
        label2.text = 'Minimum Erlang version:'

        val version = new Combo(composite, SWT.READ_ONLY)
        val runtimeVersions = ProjectPreferencesConstants.SUPPORTED_VERSIONS
        version.setItems(runtimeVersions.map[toString])
        version.setText(ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION)
        version.addModifyListener [
            info.runtimeVersion = new RuntimeVersion(version.text)
        ]
        info.runtimeVersion = new RuntimeVersion(version.text)

        new Label(composite, SWT.NONE)

        new Label(composite, SWT.NONE)
        new Label(composite, SWT.NONE)
        new Label(composite, SWT.NONE)

        val label = new Label(composite, SWT.NONE)
        label.text = 'Build system to be used:'

        val listener = new BuilderSelectionListener(info, this)
        val builders = BuilderTool.values
        builders.forEach [ builder |
            var check = new Button(composite, SWT.RADIO)
            check.text = builder.toString.toLowerCase
            check.data = builder
            check.selection = (builder === BuilderTool.INTERNAL)
            check.addSelectionListener(listener)
            val description = new Label(composite, SWT.NONE)
            description.text = getDescription(builder)
            new Label(composite, SWT.NONE)
        ]
        info.builderName = BuilderTool.INTERNAL.name

        configComposite = new Composite(composite, SWT.NONE)
        configComposite.setLayoutData(new GridData(SWT.NONE, SWT.NONE, false, false, 3, 1))
        configComposite.layout = new GridLayout(3, false)
        configComposite.visible = false

        val label1 = new Label(configComposite, SWT.NONE)
        label1.text = 'The directory layout is described'

        val listener1 = new BuilderSelectionListener(info)
        val configs = BuilderConfig.values
        configs.forEach [ config |
            var check = new Button(configComposite, SWT.RADIO)
            check.text = getDescription(config)
            check.data = config
            check.selection = (config === BuilderConfig.INTERNAL)
            check.addSelectionListener(listener1)
            new Label(configComposite, SWT.NONE)
            new Label(configComposite, SWT.NONE)
        ]
        info.builderConfig = BuilderConfig.INTERNAL.name
    }

    def String getDescription(BuilderTool builder) {
        switch (builder) {
            case BuilderTool.INTERNAL: ''': let erlide do the compiling.'''
            case BuilderTool.MAKE: ''': choose this if there is a Makefile (even if it calls rebar or emake).'''
            case BuilderTool.EMAKE: ''': straight Emake.'''
            case BuilderTool.REBAR: ''': straight rebar.'''
        }
    }

    def String getDescription(BuilderConfig config) {
        switch (config) {
            case BuilderConfig.INTERNAL: '''manually (next page)'''
            case BuilderConfig.EMAKE: '''in Emakefile'''
            case BuilderConfig.REBAR: '''in rebar.config'''
        }
    }

    override setVisible(boolean visible) {
        super.setVisible(visible)
        if (visible) {
            detectBuilderConfig
        }
    }

    def detectBuilderConfig() {
        val location = info.location
        if (location !== null && new File(location.toPortableString).exists) {

            // TODO 
            val config = BuilderConfig.valueOf(info.builderConfig)
            val persister = ErlangBuilder.getFactory().getConfigurationPersister(config)
            //val props = persister.getRawConfiguration()
        }
    }

}

class BuilderSelectionListener implements SelectionListener {

    NewProjectData info
    ErlangProjectBuilderPage panel

    new(NewProjectData info, ErlangProjectBuilderPage panel) {
        this.info = info
        this.panel = panel
    }

    new(NewProjectData info) {
        this(info, null)
    }

    override widgetDefaultSelected(SelectionEvent e) {
    }

    override widgetSelected(SelectionEvent e) {
        if (panel !== null) {
            info.builderName = (e.widget.data as BuilderTool).name
            panel.configComposite.visible = (info.builderName == BuilderTool.MAKE.name)
        } else {
            info.builderConfig = (e.widget.data as BuilderConfig).name
        }
    }

}
