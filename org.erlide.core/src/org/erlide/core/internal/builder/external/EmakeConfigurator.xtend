package org.erlide.core.internal.builder.external

import org.eclipse.core.resources.IProject
import org.eclipse.jdt.annotation.NonNull
import org.erlide.core.internal.builder.FileProjectConfigurationPersister
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.model.root.ProjectConfigurator

class EmakeConfigurator implements ProjectConfigurator {

    override String encodeConfig(@NonNull IProject project, @NonNull ErlangProjectProperties info) {
        '''
            «FOR src : info.sourceDirs»
                {'«src.toPortableString»/*',[«FOR inc : info.includeDirs»{i, "«inc.toPortableString»"},«ENDFOR»]}.
            «ENDFOR»
        '''
    }

    override decodeConfig(@NonNull String config) {
        null
    }

    def getConfigFile() {
        'Emakefile'
    }
    
    override getPersister(IProject project) {
        return new FileProjectConfigurationPersister(project, this, 'Emakefile')
    }

}
