package org.erlide.runtime;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.internal.ErlRuntime;
import org.erlide.runtime.internal.ManagedErlRuntime;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeInfoCatalog;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.util.concurrent.Service.State;

public class ErlRuntimeTest {

    private Process process;
    private ManagedErlRuntime runtime;

    @Before
    public void prepareRuntime() {
        final RuntimeInfoCatalog cat = new RuntimeInfoCatalog();
        cat.initializeRuntimesList();
        assertThat("not empty", !cat.getRuntimes().isEmpty());
        final RuntimeInfo info = cat.getRuntimes().iterator().next();
        assertThat("default info", info != RuntimeInfo.NO_RUNTIME_INFO);

        final RuntimeData data = new RuntimeData(info, "run");
        data.setNodeName("etest" + System.currentTimeMillis());
        data.setLongName(false);
        data.setCookie("c");

        runtime = new ManagedErlRuntime(data);
        runtime.startAndWait();
        process = runtime.getProcess();
        assertThat("beam process", process, is(not(nullValue())));
    }

    @After
    public void cleanupRuntime() {
        process.destroy();
        process = null;
    }

    @Test
    public void runtimeProcessStartsAndIsAvailableForRpc() {
        int val;
        try {
            val = process.exitValue();
        } catch (final IllegalThreadStateException e) {
            val = -1;
        }
        assertThat("exit value", val, is(-1));
        assertThat("running", runtime.isRunning(), is(true));
        final IRpcSite site = runtime.getRpcSite();
        OtpErlangObject r;
        try {
            r = site.call("erlang", "now", "");
        } catch (final RpcException e) {
            r = null;
        }
        assertThat("rpc", r, is(not(nullValue())));
        try {
            site.cast("erlang", "halt", "i", 0);
        } catch (final RpcException e1) {
        }
        expect(runtime, process, 0, State.TERMINATED);
    }

    @Test
    public void shutdownIsDetected() {
        final IRpcSite site = runtime.getRpcSite();
        try {
            site.cast("erlang", "halt", "i", 0);
        } catch (final RpcException e1) {
        }
        expect(runtime, process, 0, State.TERMINATED);
    }

    @Test
    public void exitCodeIsDetected() {
        final IRpcSite site = runtime.getRpcSite();
        try {
            site.cast("erlang", "halt", "i", 3);
        } catch (final RpcException e1) {
        }
        expect(runtime, process, 3, State.FAILED);
    }

    @Test
    public void crashIsDetected() {
        process.destroy();
        expect(runtime, process, 143, State.FAILED);
    }

    @Test
    public void haltIsDetected() throws RpcException {
        runtime.getRpcSite().cast("erlang", "halt", "i", 136);
        expect(runtime, process, 136, State.FAILED);
    }

    private void expect(final ErlRuntime aRuntime, final Process aProcess,
            final int code, final State state) {
        while (aRuntime.isRunning()) {
            try {
                Thread.sleep(ErlRuntime.POLL_INTERVAL);
            } catch (final InterruptedException e) {
            }
        }
        assertThat("state", aRuntime.state(), is(state));
        if (aProcess != null) {
            int val;
            try {
                val = aProcess.waitFor();
            } catch (final InterruptedException e) {
                val = -1;
            }
            assertThat("exit code", val, is(code));
        }
    }

    @Test
    public void nonManagedRuntimeWorks() {
        final RuntimeInfo info = runtime.getRuntimeData().getRuntimeInfo();
        final RuntimeData data = new RuntimeData(info, "run");
        data.setNodeName(runtime.getNodeName());
        data.setLongName(false);
        data.setCookie("c");
        data.setManaged(false);

        final ErlRuntime runtime2 = new ErlRuntime(data);
        runtime2.startAndWait();
        final Process process2 = runtime2.getProcess();
        assertThat("running", runtime2.isRunning(), is(true));
        assertThat("beam process", process2, is(nullValue()));

        final IRpcSite site = runtime2.getRpcSite();
        OtpErlangObject r;
        try {
            r = site.call("erlang", "now", "");
        } catch (final RpcException e) {
            r = null;
        }
        assertThat("rpc", r, is(not(nullValue())));
        try {
            runtime2.stopAndWait();
        } catch (final Throwable t) {
            System.out.println("EXCEPTION:::: " + t);
        }
        expect(runtime2, process2, -1, State.TERMINATED);
        assertThat("state", runtime.state(), is(State.RUNNING));
    }
}
