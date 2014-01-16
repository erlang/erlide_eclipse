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
import org.eclipse.swt.widgets.Text
import org.erlide.engine.model.builder.BuilderConfigType
import org.erlide.engine.model.builder.BuilderTool
import org.erlide.engine.model.builder.ErlangBuilder
import org.erlide.engine.model.root.ProjectPreferencesConstants
import org.erlide.runtime.runtimeinfo.RuntimeVersion
import static extension org.erlide.ui.util.XtendSWTLib.*

class ErlangProjectBuilderPage extends WizardPage {

  NewProjectData info
  protected Composite configComposite
  protected Composite makeConfigComposite

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
      newControl(Combo, SWT.READ_ONLY) [
        val runtimeVersions = ProjectPreferencesConstants.SUPPORTED_VERSIONS
        setItems(runtimeVersions.map[toString])
        setText(ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION)
        val theText = text
        addModifyListener [
          info.requiredRuntimeVersion = new RuntimeVersion(theText)
        ]
        info.requiredRuntimeVersion = new RuntimeVersion(theText)
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
      info.builderName = BuilderTool.INTERNAL.name
      configComposite = newControl(Composite, SWT.NONE) [
        layoutData = new GridData(SWT.NONE, SWT.NONE, false, false, 3, 1)
        layout = new GridLayout(3, false)
        visible = false
        newControl(Label, SWT.NONE) [
          text = 'The directory layout and the build \nconfiguration are described'
        ]
        val configListener = new ConfigSelectionListener(info)
        val configs = BuilderConfigType.values
        configs.forEach [ config |
          newControl(Button, SWT.RADIO) [
            text = getDescription(config)
            data = config
            addSelectionListener(configListener)
            selection = (config === BuilderConfigType.INTERNAL)
          ]
          newControl(Label, SWT.NONE)[]
          newControl(Label, SWT.NONE)[]
        ]
      ]
      info.builderConfigName = BuilderConfigType.INTERNAL.name
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
          setText("compile")
          layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1)
        ]
        newControl(Label, SWT.NONE)[]
        newControl(Label, SWT.NONE) [
          setText("- to clean project:")
        ]
        newControl(Text, SWT.BORDER) [
          setText("clean")
          val gd = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1)
          gd.widthHint = 250
          layoutData = gd
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

  def String getDescription(BuilderConfigType config) {
    switch (config) {
      case BuilderConfigType.INTERNAL: '''manually (on next page)'''
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
    if (location !== null) {
      val directory = new File(location.toPortableString)
      if (directory.directory && directory.exists) {
        val config = BuilderConfigType.valueOf(info.builderConfigName)
        val persister = ErlangBuilder.factory.getConfig(config, directory)
        if (persister !== null) {
          val props = persister.getConfiguration()
          println("detected PROPS: " + props)
        }
      }
    }
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
    info.builderConfigName = (e.widget.data as BuilderConfigType).name
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
    info.builderName = (e.widget.data as BuilderTool).name
    page.configComposite.visible = (info.builderName == BuilderTool.MAKE.name) ||
      (info.builderName == BuilderTool.INTERNAL.name)
    page.makeConfigComposite.visible = info.builderName == BuilderTool.MAKE.name
  }

}
