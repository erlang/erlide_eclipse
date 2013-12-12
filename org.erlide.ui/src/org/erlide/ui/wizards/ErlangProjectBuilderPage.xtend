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
import org.erlide.engine.model.builder.BuilderTool
import org.erlide.engine.model.root.ProjectPreferencesConstants
import org.erlide.runtime.runtimeinfo.RuntimeVersion
import org.erlide.engine.model.builder.ErlangBuilder
import org.eclipse.swt.widgets.Text
import org.erlide.engine.model.builder.BuilderConfigType

class ErlangProjectBuilderPage extends WizardPage {

    NewProjectData info
    protected Composite configComposite
    protected Composite makeConfigComposite

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
            info.requiredRuntimeVersion = new RuntimeVersion(version.text)
        ]
        info.requiredRuntimeVersion = new RuntimeVersion(version.text)

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
        val configs = BuilderConfigType.values
        configs.forEach [ config |
            var check = new Button(configComposite, SWT.RADIO)
            check.text = getDescription(config)
            check.data = config
            check.selection = (config === BuilderConfigType.INTERNAL)
            check.addSelectionListener(listener1)
            new Label(configComposite, SWT.NONE)
            new Label(configComposite, SWT.NONE)
        ]
        info.builderConfigName = BuilderConfigType.INTERNAL.name

        makeConfigComposite = new Composite(composite, SWT.NONE)
        makeConfigComposite.setLayoutData(new GridData(SWT.NONE, SWT.NONE, false, false, 3, 1))
        makeConfigComposite.layout = new GridLayout(3, false)
        makeConfigComposite.visible = false

        {
            val Label lblNewLabel = new Label(makeConfigComposite, SWT.NONE)
            val GridData gd_lblNewLabel = new GridData(SWT.LEFT, SWT.CENTER, false, false, 2, 1)
            gd_lblNewLabel.widthHint = 163
            lblNewLabel.setLayoutData(gd_lblNewLabel)
            lblNewLabel.setText("Make uses these targets")
        }
        new Label(makeConfigComposite, SWT.NONE)

        new Label(makeConfigComposite, SWT.NONE)
        {
            val Label lblNewLabel_1 = new Label(makeConfigComposite, SWT.NONE)
            lblNewLabel_1.setText("- to compile project:")
        }
        {
            val txtCompile = new Text(makeConfigComposite, SWT.BORDER)
            txtCompile.setText("compile")
            txtCompile.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1))
        }

        new Label(makeConfigComposite, SWT.NONE)
        {
            val Label lblNewLabel_2 = new Label(makeConfigComposite, SWT.NONE)
            lblNewLabel_2.setText("- to clean project:")
        }
        {
            val txtClean = new Text(makeConfigComposite, SWT.BORDER)
            txtClean.setText("clean")
            val GridData gd_txtClean = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1)
            gd_txtClean.widthHint = 250
            txtClean.setLayoutData(gd_txtClean)
        }

    }

    def String getDescription(BuilderTool builder) {
        switch (builder) {
            case BuilderTool.INTERNAL: ''': let erlide do the compiling.'''
            case BuilderTool.MAKE: ''': choose this if there is a Makefile (even if it calls rebar or emake).'''
            case BuilderTool.EMAKE: ''': straight Emake.'''
            case BuilderTool.REBAR: ''': straight rebar.'''
        }
    }

    def String getDescription(BuilderConfigType config) {
        switch (config) {
            case BuilderConfigType.INTERNAL: '''manually (next page)'''
            case BuilderConfigType.EMAKE: '''in Emakefile'''
            case BuilderConfigType.REBAR: '''in rebar.config'''
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
            val config = BuilderConfigType.valueOf(info.builderConfigName)
            val persister = ErlangBuilder.getFactory().getConfig(config, null).persister
            
            // TODO we need to get config without a project! it is not created yet
            val props = persister.getConfiguration()
        }
    }

}

class BuilderSelectionListener implements SelectionListener {

    val NewProjectData info
    val ErlangProjectBuilderPage page

    new(NewProjectData info, ErlangProjectBuilderPage page) {
        this.info = info
        this.page = page
    }

    new(NewProjectData info) {
        this(info, null)
    }

    override widgetDefaultSelected(SelectionEvent e) {
    }

    override widgetSelected(SelectionEvent e) {
        if (page !== null) {
            info.builderName = (e.widget.data as BuilderTool).name
            page.configComposite.visible = (info.builderName == BuilderTool.MAKE.name) ||
                (info.builderName == BuilderTool.INTERNAL.name)
            page.makeConfigComposite.visible = info.builderName == BuilderTool.MAKE.name
        } else {
            info.builderConfigName = (e.widget.data as BuilderConfigType).name
        }
    }

}
