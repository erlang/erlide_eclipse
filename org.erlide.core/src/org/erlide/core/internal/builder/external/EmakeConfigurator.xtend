package org.erlide.core.internal.builder.external

import org.eclipse.core.resources.IProject
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.model.root.ProjectConfigurator

class EmakeConfigurator implements ProjectConfigurator {

    override String encodeConfig(IProject project, ErlangProjectProperties info) {

        // TODO do nothing at the moment, will be implemented in step 2
        null

    //
    //        '''
    //            «FOR src : info.sourceDirs»
    //                {'«src.toPortableString»/*',[«FOR inc : info.includeDirs»{i, "«inc.toPortableString»"},«ENDFOR»]}.
    //            «ENDFOR»
    //        '''
    }

    override ErlangProjectProperties decodeConfig(String config) {
        null
    }

}
