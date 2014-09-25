package org.erlide.engine.internal.services.parsing;

import java.util.List;

import org.erlide.engine.services.parsing.SimpleParserService;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.rpc.RpcResult;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;

public class ErlideParser implements SimpleParserService {

    private final IOtpRpc backend;

    public ErlideParser(final IOtpRpc backend) {
        this.backend = backend;
    }

    @Override
    public List<OtpErlangObject> parse(final String s) {
        final RpcResult res = backend.call_noexception("erlide_parse", "consult", "b", s);
        if (res.isOk()) {
            final OtpErlangObject val = res.getValue();
            if (val instanceof OtpErlangList) {
                return Lists.newArrayList(((OtpErlangList) val).elements());
            }
        }
        return Lists.newArrayList();
    }

}
