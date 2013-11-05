package org.erlide.core.internal.builder

import org.eclipse.core.resources.ICommand
import org.eclipse.core.resources.IContainer
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IProjectNature
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.NullProgressMonitor
import org.erlide.engine.model.builder.BuilderInfo
import org.erlide.engine.model.builder.ErlangBuilder

/** 
 * Erlang project nature
 * @author Eric Merritt [cyberlync at yahoo dot com]
 * @author Vlad Dumitrescu [vladdu55 att gmail dot com]
 */
class ErlangNature implements IProjectNature {
    var IProject project

    override configure() throws CoreException {
        setErlangProjectBuilder(project, 'internal')
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

    static def setErlangProjectBuilder(IProject prj, String builderName) throws CoreException {
        unsetAllErlangBuilders(prj)
        val description = prj.description
        val old = description.buildSpec
        val ICommand[] specs = newArrayOfSize(old.length + 1)
        System.arraycopy(old, 0, specs, 0, old.length)
        val command = description.newCommand
        command.builderName = ErlangBuilder.factory.getBuilder(builderName.toUpperCase).id
        specs.set(old.length, command)
        description.buildSpec = specs
        prj.setDescription(description, new NullProgressMonitor())
    }

    static def unsetAllErlangBuilders(IProject prj) throws CoreException {
        val description = prj.description
        val old = description.buildSpec
        val allIds = BuilderInfo.values.map[ErlangBuilder.factory.getBuilder(name).id]

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

    static def detectBuilder(IContainer folder) {
        if (folder.findMember(BuilderInfo.MAKE.configFile) !== null) {
            return BuilderInfo.MAKE        }
        if (folder.findMember(BuilderInfo.EMAKE.configFile) !== null) {
            return BuilderInfo.EMAKE
        }
        if (folder.findMember(BuilderInfo.REBAR.configFile) !== null) {
            return BuilderInfo.REBAR
        }
        if (folder.exists) {
            return BuilderInfo.INTERNAL
        }
        null
    }
}
