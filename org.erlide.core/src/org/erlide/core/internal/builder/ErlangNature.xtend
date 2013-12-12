package org.erlide.core.internal.builder

import org.eclipse.core.resources.ICommand
import org.eclipse.core.resources.IContainer
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IProjectNature
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.NullProgressMonitor
import org.erlide.engine.model.builder.BuilderTool
import org.erlide.engine.model.builder.ErlangBuilder
import org.erlide.engine.model.builder.BuilderConfigType

/** 
 * Erlang project nature
 * @author Eric Merritt [cyberlync at yahoo dot com]
 * @author Vlad Dumitrescu [vladdu55 att gmail dot com]
 */
class ErlangNature implements IProjectNature {
    var IProject project

    override configure() throws CoreException {
        setErlangProjectBuilder(project, BuilderTool.INTERNAL)
    }

    override deconfigure() throws CoreException {
        unsetAllErlangBuilders(project)
    }

    override getProject() {
        project
    }

    override setProject(IProject lproject) {
        project = lproject
    }

    static def setErlangProjectBuilder(IProject prj, BuilderTool builder) throws CoreException {
        unsetAllErlangBuilders(prj)
        val description = prj.description
        val old = description.buildSpec
        val ICommand[] specs = newArrayOfSize(old.length + 1)
        System.arraycopy(old, 0, specs, 0, old.length)
        val command = description.newCommand
        command.builderName = ErlangBuilder.factory.getBuilder(builder).id
        specs.set(old.length, command)
        description.buildSpec = specs
        prj.setDescription(description, new NullProgressMonitor())
    }

    static def unsetAllErlangBuilders(IProject prj) throws CoreException {
        val description = prj.description
        val old = description.buildSpec
        val allIds = BuilderTool.values.map[ErlangBuilder.factory.getBuilder(it).id]

        val specs = newArrayList
        for (cmd : old) {
            val oldBuilderName = cmd.builderName
            if (!allIds.contains(oldBuilderName)) {
                specs.add(cmd)
            }
        }
        description.buildSpec = specs
        prj.setDescription(description, new NullProgressMonitor())
    }

    static def BuilderTool detectBuilderTool(IContainer folder) {
        if (!folder.exists) {
            return null
        }
        if (folder.findMember(BuilderTool.MAKE.getToolMarker) !== null) {
            return BuilderTool.MAKE
        }
        if (folder.findMember(BuilderTool.EMAKE.getToolMarker) !== null) {
            return BuilderTool.EMAKE
        }
        if (folder.findMember(BuilderTool.REBAR.getToolMarker) !== null) {
            return BuilderTool.REBAR
        }
        BuilderTool.INTERNAL
    }

    static def BuilderConfigType detectBuilderConfig(IContainer folder) {
        if (!folder.exists) {
            return null
        }
        if (folder.findMember(BuilderTool.EMAKE.getToolMarker) !== null) {
            return BuilderConfigType.EMAKE
        }
        if (folder.findMember(BuilderTool.REBAR.getToolMarker) !== null) {
            return BuilderConfigType.REBAR
        }
        BuilderConfigType.INTERNAL
    }


}
