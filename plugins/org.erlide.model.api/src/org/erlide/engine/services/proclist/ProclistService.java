package org.erlide.engine.services.proclist;

import org.erlide.runtime.rpc.IOtpRpc;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public interface ProclistService {

    void processListInit(IOtpRpc b);

    OtpErlangList getProcessList(IOtpRpc b);

    OtpErlangObject getProcessInfo(IOtpRpc b, OtpErlangPid pid);

}
