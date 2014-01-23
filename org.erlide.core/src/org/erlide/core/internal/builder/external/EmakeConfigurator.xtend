package org.erlide.core.internal.builder.external

import java.util.ArrayList
import java.util.List
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.erlide.engine.ErlangEngine
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.util.erlang.Bindings
import org.erlide.util.erlang.ErlUtils
import org.erlide.engine.model.root.ProjectConfigurationSerializer

class EmakeConfigurator implements ProjectConfigurationSerializer {

    override String encodeConfig(ErlangProjectProperties info) {

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
        val result = new ErlangProjectProperties()
        result.setOutputDir(new Path("ebin"))
        result.setSourceDirs()

        val content = ErlangEngine.instance.simpleParserService.parse(config)
        if(content.empty) return result

        content.fold(false) [ seenIncludes, erl_opts |
            var acc0 = seenIncludes
            val bindings = ErlUtils.match("{Src,Opts}", erl_opts)
            if (bindings !== null) {
                val src = bindings.getAtom("Src")
                val path = if (src.contains("/")) {
                        src.split("/").head
                    } else {
                        "src"
                    }
                val sd = new ArrayList(result.sourceDirs)
                sd.add(new Path(path))
                result.setSourceDirs(sd)

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
        }
    }
}
