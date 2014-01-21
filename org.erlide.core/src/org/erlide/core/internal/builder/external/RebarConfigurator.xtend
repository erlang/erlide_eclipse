package org.erlide.core.internal.builder.external

import com.ericsson.otp.erlang.OtpErlangString
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
    if (content.empty) return result

    content.fold(false) [ seenIncludes, erl_opts |
      val bindings = ErlUtils.match("{erl_opts,Opts}", erl_opts)
      if (bindings !== null) {
        val opts = bindings.getList("Opts")
        if (opts !== null)
          opts.fold(seenIncludes) [ seenIncludes_1, opt |
            val b = ErlUtils.match("{Tag,Arg}", opt)
            if (b !== null)
              result.parseOption(b, seenIncludes_1)
            else
              seenIncludes_1
          ]
      } else
        seenIncludes
    ]

    result
  }

  // TODO this is not efficient, a new list is created for every "i" tag
  def parseOption(ErlangProjectProperties result, Bindings b, boolean seenIncludes) {
    switch b.getAtom("Tag") {
      case "i": {
        val List<IPath> incs = if (seenIncludes)
            newArrayList(result.includeDirs)
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
