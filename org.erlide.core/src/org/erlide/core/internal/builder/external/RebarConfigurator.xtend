package org.erlide.core.internal.builder.external

import com.ericsson.otp.erlang.OtpErlangString
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.Path
import org.erlide.engine.ErlangEngine
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.model.root.ProjectConfigurator
import org.erlide.util.erlang.ErlUtils
import org.eclipse.core.runtime.IPath
import java.util.List
import java.util.ArrayList

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

        content.fold(false) [ seenIncludes, erl_opts |
            var acc0 = seenIncludes
            val bindings = ErlUtils.match("{erl_opts,Opts}", erl_opts)
            val opts = bindings?.getList("Opts")
            if (opts !== null)
                acc0 = opts.fold(acc0) [ seenIncludes1, opt |
                    var acc = seenIncludes1
                    val b = ErlUtils.match("{Tag,Arg}", opt)
                    if (b !== null)
                        switch b.getAtom("Tag") {
                            case "i": {

                                // there can be multiple instances of 'i' that need to be merged
                                val List<IPath> incs = if (seenIncludes1)
                                        new ArrayList(result.includeDirs)
                                    else
                                        newArrayList
                                incs.add(new Path(b.getString("Arg")))
                                result.setIncludeDirs(incs)
                                acc = true
                            }
                            case "src_dirs": {
                                result.setSourceDirs(
                                    b.getList("Arg").map [
                                        val s = (it as OtpErlangString).stringValue
                                        new Path(s)
                                    ])
                            }
                        }
                    acc
                ]
            acc0
        // FIXME other tags
        ]

        result
    }

}
