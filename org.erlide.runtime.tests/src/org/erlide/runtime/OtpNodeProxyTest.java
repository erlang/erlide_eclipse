package org.erlide.runtime;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeInfoCatalog;
import org.erlide.util.ErlLogger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.util.concurrent.Service.State;

public class OtpNodeProxyTest {

    private Process process;
    private ManagedOtpNodeProxy runtime;
    private RuntimeInfo info;

    @Before
    public void prepareRuntime() {
        final RuntimeInfoCatalog cat = new RuntimeInfoCatalog();
        cat.initializeRuntimesList();
        System.out.println(cat.getRuntimes());
        assertThat("empty runtime list", !cat.getRuntimes().isEmpty());
        info = cat.getRuntimes().iterator().next();
        assertThat("no default info", info != RuntimeInfo.NO_RUNTIME_INFO);

        final RuntimeData data = new RuntimeData(info, "run");
        data.setNodeName("etest" + System.currentTimeMillis());
        data.setLongName(false);
        data.setCookie("c");

        runtime = new ManagedOtpNodeProxy(data);
        runtime.startAsync();
        runtime.awaitRunning();
        process = runtime.getProcess();
        assertThat("beam process", process, is(not(nullValue())));
    }

    @After
    public void cleanupRuntime() {
        if (process != null) {
            process.destroy();
        }
        process = null;
        runtime = null;
    }

    @Test
    public void runtimeProcessStartsAndIsAvailableForRpc() {
        int val;
        try {
            val = process.exitValue();
        } catch (final IllegalThreadStateException e) {
            val = -1;
        }
        assertThat("bad exit value", val, is(-1));
        assertThat("not running", runtime.isRunning(), is(true));
        final IOtpRpc site = runtime.getOtpRpc();
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
        final IOtpRpc site = runtime.getOtpRpc();
        try {
            site.cast("erlang", "halt", "i", 0);
        } catch (final RpcException e1) {
        }
        expect(runtime, process, 0, State.TERMINATED);
    }

    @Test
    public void exitCodeIsDetected() {
        final IOtpRpc site = runtime.getOtpRpc();
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
        runtime.getOtpRpc().cast("erlang", "halt", "i", 136);
        expect(runtime, process, 136, State.FAILED);
    }

    private void expect(final OtpNodeProxy aRuntime, final Process aProcess,
            final int code, final State state) {
        while (aRuntime.isRunning()) {
            try {
                Thread.sleep(OtpNodeProxy.POLL_INTERVAL);
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
        final RuntimeData data = new RuntimeData(info, "run");
        data.setNodeName(runtime.getNodeName());
        data.setLongName(false);
        data.setCookie("c");
        data.setManaged(false);

        final OtpNodeProxy runtime2 = new OtpNodeProxy(data);
        runtime2.startAsync();
        runtime2.awaitRunning();

        final Process process2 = runtime2.getProcess();
        assertThat("running", runtime2.isRunning(), is(true));
        assertThat("beam process", process2, is(nullValue()));

        final IOtpRpc site = runtime2.getOtpRpc();
        OtpErlangObject r;
        try {
            r = site.call("erlang", "now", "");
        } catch (final RpcException e) {
            r = null;
        }
        assertThat("rpc", r, is(not(nullValue())));
        try {
            runtime2.stopAsync();
            runtime2.awaitTerminated();
        } catch (final Throwable t) {
            ErlLogger.error(t);
        }
        expect(runtime2, process2, -1, State.TERMINATED);
        assertThat("state", runtime.state(), is(State.RUNNING));
    }
}
