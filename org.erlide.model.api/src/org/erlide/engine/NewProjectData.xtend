package org.erlide.engine

import com.google.common.base.Objects
import java.io.File
import java.util.Map
import org.eclipse.core.runtime.IPath
import org.eclipse.xtend.lib.annotations.Accessors
import org.erlide.engine.model.builder.BuilderTool
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.model.root.IProjectConfigurator
import org.erlide.engine.model.root.IProjectConfiguratorFactory
import org.erlide.engine.model.root.ProjectConfigType
import org.erlide.engine.model.root.ProjectPreferencesConstants
import org.erlide.runtime.api.RuntimeCore
import org.erlide.runtime.runtimeinfo.RuntimeInfo

@Accessors
class NewProjectData extends ErlangProjectProperties {

    String name = null
    IPath location = null
    boolean existingProject = false

    BuilderTool builder = BuilderTool.INTERNAL
    ProjectConfigType configType = ProjectConfigType.INTERNAL

    Map<String, String> builderData = newHashMap

    val IProjectConfiguratorFactory factory

    new() {
        this.factory = ErlangEngine.getInstance.projectConfiguratorFactory
    }

    override String toString() {
        val helper = Objects.toStringHelper(this) => [
            add("name", name)
            add("location", location)
            add("existingProject", existingProject)
            add("configType", configType)
            add("builder", builder)
            add("builderData", builderData)
            add("super", super.toString)
        ]
        helper.toString
    }

    def void loadFromFile() {
        val File f = new File(getLocation().append(getConfigType().getConfigName()).toPortableString())
        if (f.exists()) {
            System.out.println("» LOAD " + f.getAbsolutePath())
            val IProjectConfigurator config = factory.getConfig(getConfigType(),
                new File(getLocation().toPortableString()))
            val ErlangProjectProperties props = config.getConfiguration()
            setOutputDir(props.getOutputDir())
            setSourceDirs(props.getSourceDirs())
            setIncludeDirs(props.getIncludeDirs())
            setTestDirs(props.getTestDirs())
        }

    }

    def detectProjectConfig() {
        println("» DETECT builder config")
        if (location !== null) {
            println("DETECT builder config")
            val directory = new File(location.toPortableString)
            if (directory.directory && directory.exists) {
                val persister = factory.getConfig(configType, directory)
                println("PERSISTER " + persister)
                if (persister !== null) {
                    val props = persister.getConfiguration()
                    println("detected PROPS: " + props)
                }
            }
        }
    }

    def RuntimeInfo bestRuntime() {
        RuntimeCore.getRuntimeInfoCatalog().getRuntime(ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION, null)
    }

}
