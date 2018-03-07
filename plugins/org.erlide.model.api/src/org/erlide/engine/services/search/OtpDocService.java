package org.erlide.engine.services.search;

import java.util.Collection;
import java.util.List;

import org.erlide.engine.services.ErlangService;
import org.erlide.runtime.rpc.IOtpRpc;
import org.erlide.util.ErlangFunctionCall;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public interface OtpDocService extends ErlangService {

    OtpErlangObject getProposalsWithDoc(IOtpRpc b, String mod, String prefix);

    OtpErlangObject getModules(IOtpRpc b, String prefix, List<String> projectModules,
            boolean includes);

    OtpErlangObject getOtpDoc(IOtpRpc b, ErlangFunctionCall functionCall);

    OtpErlangObject getOtpDoc(IOtpRpc b, int offset, String module,
            Collection<OtpErlangObject> imports, String externalModules,
            OtpErlangList pathVars);

    String getOtpDocLocation(IOtpRpc b);

}
