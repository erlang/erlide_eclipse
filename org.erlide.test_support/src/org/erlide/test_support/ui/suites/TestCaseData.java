package org.erlide.test_support.ui.suites;

import com.ericsson.otp.erlang.OtpErlangObject;

public class TestCaseData {

    enum TestState {
        // order is important!
        NOT_RUN, SUCCESS, SKIPPED, RUNNING, FAILED
    }

    private final String suite;
    private final String testcase;
    private TestState state;
    private OtpErlangObject failReason;
    private OtpErlangObject failLocations;
    private OtpErlangObject skipComment;

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
        state = TestState.FAILED;
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

    public String getFailLocations() {
        return failLocations.toString();
    }

    public String getFailReason() {
        return failReason.toString();
    }

    public void setSkipped(final OtpErlangObject comment) {
        state = TestState.SKIPPED;
        skipComment = comment;
    }

    public OtpErlangObject getSkipComment() {
        return skipComment;
    }
}
