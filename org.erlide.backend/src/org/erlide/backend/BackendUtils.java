package org.erlide.backend;

import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.RegistryFactory;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class BackendUtils {

    public static String getErlideNodeNameTag() {
        String fUniqueId;
        final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        final String location = root.getLocation().toPortableString();
        final String user = System.getProperty("user.name");
        final String timestamp = Long
                .toHexString(System.currentTimeMillis() & 0xFFFFFF);
        fUniqueId = Long.toHexString(location.hashCode() & 0xFFFFF) + "_"
                + user + "_" + timestamp;
        return fUniqueId.replaceAll("[^a-zA-Z0-9_-]", "");
    }

    public static String getBeamModuleName(final String path) {
        return getBeamModuleName(new Path(path));
    }

    public static String getBeamModuleName(final IPath path) {
        if (path.getFileExtension() != null
                && "beam".equals(path.getFileExtension())) {
            return path.removeFileExtension().lastSegment();
        }
        return null;
    }

    public static OtpErlangObject ok(final OtpErlangObject v0) {
        if (!(v0 instanceof OtpErlangTuple)) {
            return v0;
        }
        final OtpErlangTuple v = (OtpErlangTuple) v0;
        if (Util.isOk(v)) {
            return v.elementAt(1);
        }
        return v;
    }

    public static IConfigurationElement[] getSourcepathConfigurationElements() {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        return reg.getConfigurationElementsFor(BackendPlugin.PLUGIN_ID,
                "sourcePathProvider");
    }

    public static IConfigurationElement[] getCodepathConfigurationElements() {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        return reg.getConfigurationElementsFor(BackendPlugin.PLUGIN_ID,
                "codepath");
    }

    public static IExtensionPoint getCodepathExtension() {
        final IExtensionRegistry reg = Platform.getExtensionRegistry();
        return reg.getExtensionPoint(BackendPlugin.PLUGIN_ID, "codepath");
    }

    @SuppressWarnings("boxing")
    public static OtpErlangObject call(final IRpcSite b, final String module,
            final String fun, final int offset, final int length,
            final String text) {
        try {
            final OtpErlangObject r1 = b.call(module, fun, "sii", text, offset,
                    length);
            return r1;
        } catch (final RpcException e) {
            return new OtpErlangString("");
        }
    }

}
