package org.erlide.runtime.events;

import org.erlide.runtime.api.IErlRuntime;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class ErlEvent {

    private final String topic;
    private final IErlRuntime runtime;
    private final OtpErlangObject event;
    private final OtpErlangPid sender;

    public ErlEvent(final String topic, final IErlRuntime runtime,
            final OtpErlangObject event, final OtpErlangPid sender) {
        this.topic = topic;
        this.runtime = runtime;
        this.event = event;
        this.sender = sender;
    }

    public OtpErlangObject getEvent() {
        return event;
    }

    public String getTopic() {
        return topic;
    }

    public IErlRuntime getRuntime() {
        return runtime;
    }

    public OtpErlangPid getSender() {
        return sender;
    }
}
