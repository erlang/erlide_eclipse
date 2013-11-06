package org.erlide.core.internal.builder.external

import com.ericsson.otp.erlang.OtpErlangString
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.Path
import org.erlide.engine.ErlangEngine
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.model.root.ProjectConfigurator
import org.erlide.util.erlang.ErlUtils

class RebarConfigurator implements ProjectConfigurator {

    override encodeConfig(IProject project, ErlangProjectProperties info) {

        // TODO do nothing at the moment, will be implemented in step 2
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
        val result = new ErlangProjectProperties()

        val content = ErlangEngine.instance.simpleParserService.parse(config)
        if(content.empty) return result
        content.forEach [
            val bindings = ErlUtils.match("{erl_opts,Opts}", it)
            if (bindings !== null) {

                val opts = bindings.getList("Opts")
                opts.forEach [
                    val b = ErlUtils.match("{Tag,Arg}", it)
                    switch b.getAtom("Tag") {
                        case "i":
                            // TODO can be a list!
                            result.setIncludeDirs(new Path(b.getString("Arg")))
                        case "src_dirs":
                            // TODO can be a string!
                            result.setSourceDirs(
                                b.getList("Arg").map [
                                    val s = (it as OtpErlangString).stringValue
                                    new Path(s)
                                ])
                    }
                ]
            }
        // FIXME other tags
        ]

        result
    }

}
