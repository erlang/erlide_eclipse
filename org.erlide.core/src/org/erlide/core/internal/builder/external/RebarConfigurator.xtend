package org.erlide.core.internal.builder.external

import com.google.common.base.Charsets
import com.google.common.io.Files
import java.io.File
import org.eclipse.core.resources.IProject
import org.erlide.core.internal.builder.BuilderConfigurator
import org.erlide.engine.model.root.IErlangProjectProperties

class RebarConfigurator implements BuilderConfigurator {

    override createConfig(IProject project, IErlangProjectProperties info) {
        val content = getConfigString(project, info)
        Files.write(content, new File(project.location.append("rebar.config").toPortableString), Charsets.UTF_8);
    }

    def getConfigString(IProject project, IErlangProjectProperties properties) {
        '''
            %% coding: utf-8
            
        '''
    }

    override getConfigParser() {
        null
    }

}