package org.erlide.model.services.search;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public interface OpenService {
    OtpErlangObject getSourceFromModule(final IRpcSite backend,
            final OtpErlangList pathVars, final String mod,
            final String externalModules) throws RpcException;

    OpenResult open(final IRpcSite backend, final String scannerName,
            final int offset, final List<OtpErlangObject> imports,
            final String externalModules, final OtpErlangList pathVars)
            throws RpcException;

    OpenResult openText(final IRpcSite backend, final String text,
            final int offset) throws RpcException;

    OtpErlangTuple mkContext(final String externalModules,
            final String externalIncludes, final OtpErlangList pathVars,
            final Collection<IPath> extraSourcePaths,
            final Collection<OtpErlangObject> imports);

    OtpErlangTuple findFirstVar(final IRpcSite backend, final String name,
            final String source);

    public class ExternalTreeEntry {
        private final String parentPath;
        private final String path;
        // private final String name;
        private final boolean isModule;

        public ExternalTreeEntry(final String parentPath, final String path,
        // final String name,
                final boolean isModule) {
            super();
            this.parentPath = parentPath;
            this.path = path;
            // this.name = name;
            this.isModule = isModule;
        }

        public String getParentPath() {
            return parentPath;
        }

        public String getPath() {
            return path;
        }

        // public String getName() {
        // return name;
        // }

        public boolean isModule() {
            return isModule;
        }
    }

    List<ExternalTreeEntry> getExternalModuleTree(final IRpcSite backend,
            final String externalModules, final OtpErlangList pathVars);

    String getExternalInclude(final IRpcSite backend, final String filePath,
            final String externalIncludes, final OtpErlangList pathVars);

    List<String> getLibDirs(final IRpcSite backend);

    List<String> getLibFiles(final IRpcSite backend, final String entry);

    List<List<String>> getLibSrcInclude(final IRpcSite backend,
            final List<String> libList);

    Collection<String> getIncludesInDir(final IRpcSite backend,
            final String directory);
}
