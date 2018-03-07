package org.erlide.engine.services.search;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.engine.services.ErlangService;
import org.erlide.runtime.rpc.IOtpRpc;
import org.erlide.runtime.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public interface OpenService extends ErlangService {

    OtpErlangObject getSourceFromModule(final OtpErlangList pathVars, final String mod,
            final String externalModules) throws RpcException;

    OpenResult open(final String scannerName, final int offset,
            final List<OtpErlangObject> imports, final String externalModules,
            final OtpErlangList pathVars) throws RpcException;

    OpenResult openText(final String text, final int offset) throws RpcException;

    OtpErlangTuple mkContext(final String externalModules, final String externalIncludes,
            final OtpErlangList pathVars, final Collection<IPath> extraSourcePaths,
            final Collection<OtpErlangObject> imports);

    OtpErlangTuple findFirstVar(final String name, final String source);

    List<ExternalTreeEntry> getExternalModuleTree(IOtpRpc backend,
            final String externalModules, final OtpErlangList pathVars);

    String getExternalInclude(final String filePath, final String externalIncludes,
            final OtpErlangList pathVars);

    List<String> getLibFiles(final String entry);

    Collection<String> getIncludesInDir(final String directory);

    OtpErlangList getOtpLibStructure(IOtpRpc backend);
}
