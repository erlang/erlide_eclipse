package org.erlide.core.internal.builder.external

import java.io.StringBufferInputStream
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.CoreException
import org.erlide.core.internal.builder.BuilderConfigurator
import org.erlide.engine.model.root.IErlangProjectProperties
import org.erlide.util.ErlLogger

class EmakeConfigurator implements BuilderConfigurator {

    override createConfig(IProject project, IErlangProjectProperties info) {
        val config = project.getFile('Emakefile')
        try {
            val template = '''
                «FOR src : info.sourceDirs»
                    {'«src.toPortableString»/*',[«FOR inc : info.includeDirs»{i, "«inc.toPortableString»"},«ENDFOR»]}.
                «ENDFOR»
            '''
            println(">> " + template)
            config.create(new StringBufferInputStream(template), true, null)
        } catch (CoreException e) {
            ErlLogger::error(e)
        }
    }
  
    override getConfigParser() {
        null
    }

}
