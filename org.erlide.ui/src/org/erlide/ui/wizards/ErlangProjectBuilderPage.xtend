package org.erlide.ui.wizards

import org.eclipse.jface.wizard.WizardPage
import org.eclipse.swt.SWT
import org.eclipse.swt.events.SelectionEvent
import org.eclipse.swt.events.SelectionListener
import org.eclipse.swt.layout.GridLayout
import org.eclipse.swt.widgets.Button
import org.eclipse.swt.widgets.Combo
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Label
import org.erlide.engine.model.root.ProjectPreferencesConstants
import org.erlide.runtime.runtimeinfo.RuntimeVersion
import java.io.File
import org.erlide.engine.model.builder.BuilderTool

class ErlangProjectBuilderPage extends WizardPage {

    NewProjectData info

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

        val label = new Label(composite, SWT.NONE)
        label.text = 'Build system to be used:'

        val listener = new BuilderSelectionListener(info)
        val builders = BuilderTool.values
        builders.forEach [ builder |
            var check = new Button(composite, SWT.RADIO)
            check.text = builder.toString.toLowerCase
            check.data = builder
            if (builder === BuilderTool.INTERNAL) {
                check.selection = true
            }
            check.addSelectionListener(listener)
            val description = new Label(composite, SWT.NONE)
            description.text = getDescription(builder)
            new Label(composite, SWT.NONE)
        ]
        info.builderName = BuilderTool.INTERNAL.toString.toUpperCase
    }

    def String getDescription(BuilderTool builder) {
        switch (builder) {
            case BuilderTool.INTERNAL: ''': let erlide do the compiling.'''
            case BuilderTool.MAKE: ''': choose this if there is a Makefile (even if it calls rebar or emake).'''
            case BuilderTool.EMAKE: ''': straight Emake.'''
            case BuilderTool.REBAR: ''': straight rebar.'''
        }
    }

    override setVisible(boolean visible) {
        super.setVisible(visible)
        if (visible) {
            detectBuilderConfig
        }
    }

    def detectBuilderConfig() {
        if (new File(info.location.toPortableString).exists) {
                         
        } 
    }

}

class BuilderSelectionListener implements SelectionListener {

    val NewProjectData info

    new(NewProjectData info) {
        this.info = info
    }

    override widgetDefaultSelected(SelectionEvent e) {
    }

    override widgetSelected(SelectionEvent e) {
        info.builderName = (e.widget.data as BuilderTool).toString
    }

}
