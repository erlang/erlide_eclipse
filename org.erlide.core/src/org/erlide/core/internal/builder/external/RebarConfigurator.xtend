package org.erlide.core.internal.builder.external

import org.eclipse.core.resources.IProject
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.model.root.ProjectConfigurator

class RebarConfigurator implements ProjectConfigurator {

    override encodeConfig(IProject project, ErlangProjectProperties info) {
        null

    // TODO we need to keep the original parsed config and only replace changed parts!
    //        // TODO compiler options
    //        '''
    //            %% coding: utf-8
    //            {require_min_otp_vsn, "«info.runtimeVersion»"}.
    //            {erl_opts, 
    //                [
    //                 debug_info,
    //                 «FOR inc: info.includeDirs SEPARATOR ','»{i, "«inc»"},«ENDFOR»
    //                 {src_dirs, [«FOR src: info.sourceDirs SEPARATOR ','»«src.toPortableString»«ENDFOR»]}
    //                ]}.
    //            
    //        '''
    }

    override decodeConfig(String config) {
        //        List<OtpErlangObject> content = parseErlangTerms(string)
    }

    def getConfigFile() {
        'rebar.config'
    }

}
