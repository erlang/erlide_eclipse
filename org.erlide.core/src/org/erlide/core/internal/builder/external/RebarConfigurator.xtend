package org.erlide.core.internal.builder.external

import com.ericsson.otp.erlang.OtpErlangString
import java.util.ArrayList
import java.util.List
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.erlide.engine.ErlangEngine
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.model.root.ProjectConfigurator
import org.erlide.util.erlang.Bindings
import org.erlide.util.erlang.ErlUtils

class RebarConfigurator implements ProjectConfigurator {

    override encodeConfig(ErlangProjectProperties info) {

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
        result.setOutputDir(new Path("ebin"))

        val content = ErlangEngine.instance.simpleParserService.parse(config)
        if(content.empty) return result

        content.fold(false) [ seenIncludes, erl_opts |
            var acc0 = seenIncludes
            val bindings = ErlUtils.match("{erl_opts,Opts}", erl_opts)
            if (bindings !== null) {
                val opts = bindings.getList("Opts")
                if (opts !== null)
                    acc0 = opts.fold(acc0) [ seenIncludes1, opt |
                        var acc = seenIncludes1
                        val b = ErlUtils.match("{Tag,Arg}", opt)
                        if (b !== null)
                            acc = parseOption(b, acc, result)
                        acc
                    ]
            }
            acc0
        ]

        result
    }

    def parseOption(Bindings b, boolean seenIncludes, ErlangProjectProperties result) {
        switch b.getAtom("Tag") {
            case "i": {
                val List<IPath> incs = if (seenIncludes)
                        new ArrayList(result.includeDirs)
                    else
                        newArrayList
                val inc = new Path(b.getString("Arg"))
                if (!incs.contains(inc))
                    incs.add(inc)
                result.setIncludeDirs(incs)
                true
            }
            case "src_dirs": {
                result.setSourceDirs(
                    b.getList("Arg").map [
                        val s = (it as OtpErlangString).stringValue
                        new Path(s)
                    ])
                seenIncludes
            }
        }
    }
}
