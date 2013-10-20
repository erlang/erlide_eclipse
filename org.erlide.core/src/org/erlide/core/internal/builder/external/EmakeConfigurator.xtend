package org.erlide.core.internal.builder.external

import org.eclipse.core.resources.IProject
import org.erlide.core.internal.builder.BuilderConfigurator
import org.erlide.engine.model.root.IErlangProjectProperties

class EmakeConfigurator implements BuilderConfigurator {

    override String encodeConfig(IProject project, IErlangProjectProperties info) {
        '''
            «FOR src : info.sourceDirs»
                {'«src.toPortableString»/*',[«FOR inc : info.includeDirs»{i, "«inc.toPortableString»"},«ENDFOR»]}.
            «ENDFOR»
        '''
    }

    override decodeConfig(String config) {
        null
    }

    override getConfigFile() {
        'Emakefile'
    }

}
