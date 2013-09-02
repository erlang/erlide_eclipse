package org.erlide.model.services.search;

import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.rpc.IRpcResultCallback;
import org.erlide.runtime.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public interface SearchServerService {

    public abstract void startFindRefs(IRpcSite backend,
            ErlangSearchPattern pattern, ErlSearchScope scope, String stateDir,
            IRpcResultCallback callback, boolean updateSearchServer)
            throws RpcException;

    public abstract OtpErlangObject findRefs(IRpcSite backend,
            ErlangSearchPattern pattern, ErlSearchScope scope, String stateDir,
            boolean updateSearchServer) throws RpcException;

    public abstract void cancelSearch(IRpcSite backend,
            OtpErlangPid searchDeamonPid) throws RpcException;

}
