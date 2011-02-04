package org.erlide.test_support.ui.suites;

import com.ericsson.otp.erlang.OtpErlangObject;

public class TestCaseData {

    enum TestState {
        NOT_RUN, RUNNING, SUCCESS, FAILURE
    }

    private final String suite;
    private final String testcase;
    private TestState state;
    private OtpErlangObject failReason;
    private OtpErlangObject failLocations;

    public TestCaseData(final String mod, final String fun) {
        suite = mod;
        testcase = fun;
        state = TestState.NOT_RUN;
    }

    @Override
    public String toString() {
        return suite + ":" + testcase;
    }

    public void setRunning() {
        state = TestState.RUNNING;
    }

    public void setSuccesful() {
        state = TestState.SUCCESS;
    }

    public void setFailed(final OtpErlangObject reason,
            final OtpErlangObject locations) {
        state = TestState.FAILURE;
        failReason = reason;
        failLocations = locations;
    }

    public String getModule() {
        return suite;
    }

    public String getFunction() {
        return testcase;
    }

    public TestState getState() {
        return state;
    }
}
