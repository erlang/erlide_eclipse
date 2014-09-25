package org.erlide.engine.services.search;

import java.util.Collection;
import java.util.List;

import org.erlide.engine.services.ErlangService;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.util.ErlangFunctionCall;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public interface OtpDocService extends ErlangService {

    OtpErlangObject getProposalsWithDoc(IOtpRpc b, String mod, String prefix,
            String stateDir);

    OtpErlangObject getModules(IOtpRpc b, String prefix, List<String> projectModules,
            boolean includes);

    OtpErlangObject getOtpDoc(IOtpRpc b, ErlangFunctionCall functionCall, String stateDir);

    OtpErlangObject getOtpDoc(IOtpRpc b, int offset, String stateDir, String module,
            Collection<OtpErlangObject> imports, String externalModules,
            OtpErlangList pathVars);

    String getOtpDocLocation(IOtpRpc b);

}
