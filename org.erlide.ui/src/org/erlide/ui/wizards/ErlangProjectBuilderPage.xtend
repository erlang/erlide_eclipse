package org.erlide.ui.wizards

import org.eclipse.swt.SWT
import org.eclipse.swt.events.SelectionEvent
import org.eclipse.swt.events.SelectionListener
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.layout.GridLayout
import org.eclipse.swt.widgets.Button
import org.eclipse.swt.widgets.Combo
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Label
import org.eclipse.swt.widgets.Text
import org.erlide.engine.model.builder.BuilderTool
import org.erlide.engine.model.root.NewProjectData
import org.erlide.engine.model.root.ProjectConfigType
import org.erlide.engine.model.root.ProjectPreferencesConstants
import org.erlide.runtime.runtimeinfo.RuntimeVersion

import static extension org.erlide.ui.util.XtendSWTLib.*
import org.erlide.core.executor.ToolExecutor
import org.eclipse.jface.dialogs.DialogPage

class ErlangProjectBuilderPage extends ErlangWizardPage {

  NewProjectData info
  protected Composite configComposite
  protected Composite makeConfigComposite

  Combo runtimeCombo

  protected new(String pageName, NewProjectData info) {
    super(pageName)
    this.info = info;
  }

  override createControl(Composite parent) {
    val composite = newControl(parent, Composite, SWT.NONE) [
      layout = new GridLayout(3, false)
      newControl(Label, SWT.NONE) [
        text = 'Minimum Erlang version:'
      ]
      runtimeCombo = newControl(Combo, SWT.READ_ONLY) [
        val runtimeVersions = ProjectPreferencesConstants.SUPPORTED_VERSIONS
        items = runtimeVersions.map[toString]
        text = info.bestRuntime.version.asMajor.toString
        info.requiredRuntimeVersion = new RuntimeVersion(text)
      ]
      newControl(Label, SWT.NONE)[]
      newControl(Label, SWT.NONE)[]
      newControl(Label, SWT.NONE)[]
      newControl(Label, SWT.NONE)[]
      newControl(Label, SWT.NONE) [
        text = 'Build system to be used:'
      ]
      val builderListener = new BuilderSelectionListener(info, this)
      val builders = BuilderTool.values
      builders.forEach [ builder |
        newControl(Button, SWT.RADIO) [
          text = builder.toString.toLowerCase
          data = builder
          addSelectionListener(builderListener)
          selection = (builder === BuilderTool.INTERNAL)
        ]
        newControl(Label, SWT.NONE) [
          text = getDescription(builder)
        ]
        newControl(Label, SWT.NONE)[]
      ]
      info.builder = BuilderTool.INTERNAL
      configComposite = newControl(Composite, SWT.NONE) [
        layoutData = new GridData(SWT.NONE, SWT.NONE, false, false, 3, 1)
        layout = new GridLayout(3, false)
        newControl(Label, SWT.NONE) [
          text = 'The directory layout and the build \nconfiguration are described'
        ]
        val configListener = new ConfigSelectionListener(info)
        val configs = ProjectConfigType.values
        configs.forEach [ config |
          newControl(Button, SWT.RADIO) [
            text = getDescription(config)
            data = config
            addSelectionListener(configListener)
            selection = (config === ProjectConfigType.INTERNAL)
          ]
          newControl(Label, SWT.NONE)[]
          newControl(Label, SWT.NONE)[]
        ]
      ]
      info.configType = ProjectConfigType.INTERNAL
      makeConfigComposite = newControl(Composite, SWT.NONE) [
        layoutData = new GridData(SWT.NONE, SWT.NONE, false, false, 3, 1)
        layout = new GridLayout(3, false)
        visible = false
        newControl(Label, SWT.NONE) [
          val gd = new GridData(SWT.LEFT, SWT.CENTER, false, false, 2, 1)
          gd.widthHint = 163
          layoutData = gd
          setText("Make uses these targets")
        ]
        newControl(Label, SWT.NONE)[]
        newControl(Label, SWT.NONE)[]
        newControl(Label, SWT.NONE) [
          setText("- to compile project:")
        ]
        newControl(Text, SWT.BORDER) [
          layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1)
          addModifyListener [ l |
            info.builderData.put("compile", text)
          ]
          text = "compile"
        ]
        newControl(Label, SWT.NONE)[]
        newControl(Label, SWT.NONE) [
          setText("- to clean project:")
        ]
        newControl(Text, SWT.BORDER) [
          val gd = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1)
          gd.widthHint = 250
          layoutData = gd
          addModifyListener [ l |
            info.builderData.put("clean", text)
          ]
          text = "clean"
        ]
      ]
    ]
    control = composite;
  }

  def String getDescription(BuilderTool builder) {
    switch (builder) {
      case BuilderTool.INTERNAL: ''': let erlide do the compiling.'''
      case BuilderTool.MAKE: ''': choose this if there is a Makefile (even if it calls rebar or emake).'''
      case BuilderTool.EMAKE: ''': straight Emake.'''
      case BuilderTool.REBAR: ''': straight rebar.'''
    }
  }

  def String getDescription(ProjectConfigType config) {
    switch (config) {
      case ProjectConfigType.INTERNAL: '''manually (on next page)'''
      case ProjectConfigType.EMAKE: '''in Emakefile'''
      case ProjectConfigType.REBAR: '''in rebar.config'''
    }
  }

  override protected onEntry() {
    if (info.existingProject) {
      info.detectProjectConfig

    // TODO disable selecting different config
    }

  // TODO set fields from info
  }

  override protected onExit() {
    info.requiredRuntimeVersion = new RuntimeVersion(runtimeCombo.text)
  }

}

class ConfigSelectionListener implements SelectionListener {

  val NewProjectData info

  new(NewProjectData info) {
    this.info = info
  }

  override widgetDefaultSelected(SelectionEvent e) {
  }

  override widgetSelected(SelectionEvent e) {
    info.configType = e.widget.data as ProjectConfigType
  }

}

class BuilderSelectionListener implements SelectionListener {

  val NewProjectData info
  val ErlangProjectBuilderPage page

  new(NewProjectData info, ErlangProjectBuilderPage page) {
    this.info = info
    this.page = page
  }

  override widgetDefaultSelected(SelectionEvent e) {
  }

  override widgetSelected(SelectionEvent e) {
    info.builder = e.widget.data as BuilderTool

    page.message = null
    val toolExists = info.builder.osCommand === null ||
      ToolExecutor.getToolLocation(info.builder.osCommand) !== null
    if (!toolExists) {
      page.setMessage(
        '''The tool '«info.builder.osCommand»' can't be found on your system's $PATH''',
        DialogPage.WARNING
      )
    }

    page.configComposite.visible = (info.builder == BuilderTool.MAKE) || (info.builder == BuilderTool.INTERNAL)
    page.makeConfigComposite.visible = info.builder == BuilderTool.MAKE
  }

}
