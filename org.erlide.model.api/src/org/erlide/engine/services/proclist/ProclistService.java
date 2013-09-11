package org.erlide.engine.services.proclist;

import org.erlide.runtime.api.IRpcSite;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public interface ProclistService {

    public abstract void processListInit(IRpcSite b);

    public abstract OtpErlangList getProcessList(IRpcSite b);

    public abstract OtpErlangObject getProcessInfo(IRpcSite b, OtpErlangPid pid);

}
