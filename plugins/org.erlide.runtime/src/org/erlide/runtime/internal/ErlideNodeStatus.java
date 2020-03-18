package org.erlide.runtime.internal;

import com.ericsson.otp.erlang.OtpNodeStatus;
import com.google.common.base.Objects;

@SuppressWarnings("all")
public class ErlideNodeStatus extends OtpNodeStatus {
    private final OtpNodeProxy runtime;

    public ErlideNodeStatus(final OtpNodeProxy runtime) {
        this.runtime = runtime;
    }

    @Override
    public void remoteStatus(final String node, final boolean up, final Object info) {
        if (Objects.equal(node, runtime.getNodeName()) && !up) {
        }
    }
}
