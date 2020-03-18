package org.erlide.runtime.internal;

import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.util.HostnameChecker;

import com.ericsson.otp.erlang.OtpNode;
import com.google.common.base.Strings;

@SuppressWarnings("all")
public class LocalNodeCreator {
    public static OtpNode createOtpNode(final String cookie, final boolean longName) {
        try {
            OtpNode _xblockexpression = null;
            {
                final String hostName = HostnameChecker.getInstance()
                        .getErlangHostName(longName);
                OtpNode _xifexpression = null;
                final boolean _isNullOrEmpty = Strings.isNullOrEmpty(cookie);
                if (_isNullOrEmpty) {
                    final String _create = NodeNameCreator.create(hostName);
                    _xifexpression = new OtpNode(_create);
                } else {
                    final String _create_1 = NodeNameCreator.create(hostName);
                    _xifexpression = new OtpNode(_create_1, cookie);
                }
                _xblockexpression = _xifexpression;
            }
            return _xblockexpression;
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    public static OtpNode startLocalNode(final OtpNodeProxy runtime, final String cookie,
            final boolean hasLongName) {
        LocalNodeCreator.wait_for_epmd();
        final OtpNode lNode = LocalNodeCreator.createOtpNode(cookie, hasLongName);
        final ErlideNodeStatus statusWatcher = new ErlideNodeStatus(runtime);
        lNode.registerStatusHandler(statusWatcher);
        return lNode;
    }

    public static void wait_for_epmd() {
        LocalNodeCreator.wait_for_epmd(null);
    }

    public static void wait_for_epmd(final String host) {
        final EpmdChecker initializer = new EpmdChecker(host);
    }
}
