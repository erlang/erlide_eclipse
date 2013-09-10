package org.erlide.engine.services.search;

import java.util.Collection;
import java.util.List;

import org.erlide.engine.services.ErlangService;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.util.ErlangFunctionCall;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public interface OtpDocService extends ErlangService {

    OtpErlangObject getProposalsWithDoc(IRpcSite b, String mod, String prefix,
            String stateDir);

    OtpErlangObject getModules(IRpcSite b, String prefix,
            List<String> projectModules, boolean includes);

    OtpErlangObject getOtpDoc(IRpcSite b, ErlangFunctionCall functionCall,
            String stateDir);

    OtpErlangObject getOtpDoc(IRpcSite b, int offset, String stateDir,
            String module, Collection<OtpErlangObject> imports,
            String externalModules, OtpErlangList pathVars);

    String getOtpDocLocation(IRpcSite b);

}
