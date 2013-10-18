package org.erlide.ui.wizards

import org.eclipse.swt.SWT
import org.eclipse.swt.layout.GridLayout
import org.eclipse.swt.widgets.Combo
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Label
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage
import org.erlide.core.internal.builder.BuildersInfo
import org.erlide.engine.model.root.IErlangProjectProperties
import org.erlide.engine.model.root.ProjectPreferencesConstants
import org.erlide.runtime.runtimeinfo.RuntimeVersion

class ErlangNewProjectCreationPage extends WizardNewProjectCreationPage {
    val IErlangProjectProperties info

    new(String name, IErlangProjectProperties info) {
        super(name);
        this.info = info
    }

    override createControl(Composite parent) {
        super.createControl(parent)

        val composite = new Composite((control as Composite), SWT::NONE)
        composite.layout = new GridLayout(2, false)

        val label = new Label(composite, SWT::NONE)
        label.text = 'Build system to be used:'

        val builder = new Combo(composite, SWT::READ_ONLY)
        val builders = BuildersInfo::values
        builder.items = builders.map[toString.toLowerCase]
        builder.select(BuildersInfo.INTERNAL.ordinal)
        builder.addModifyListener [
            info.builderName = builder.text.toUpperCase
        ]
        info.builderName = builder.text.toUpperCase

        val label2 = new Label(composite, SWT::NONE)
        label2.text = 'Minimum Erlang version:'

        val version = new Combo(composite, SWT::READ_ONLY)

        val runtimeVersions = ProjectPreferencesConstants.SUPPORTED_VERSIONS
        version.setItems(runtimeVersions.map[toString])
        version.setText(ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION)
        version.addModifyListener [
            info.runtimeVersion = new RuntimeVersion(version.text)
        ]
        info.runtimeVersion = new RuntimeVersion(version.text)

    }

    override setVisible(boolean visible) {
        super.setVisible(visible)
        if (!visible) {
            info.name = projectName
            info.location = locationPath
        }
    }

}
