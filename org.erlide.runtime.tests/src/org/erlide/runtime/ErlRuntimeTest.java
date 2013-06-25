package org.erlide.runtime;

import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.internal.ErlRuntime;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeInfoCatalog;
import org.erlide.util.Asserts;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.util.concurrent.Service.State;

public class ErlRuntimeTest {

    private Process process;
    private ErlRuntime runtime;

    @Before
    public void prepareRuntime() {
        final RuntimeInfoCatalog cat = new RuntimeInfoCatalog();
        cat.initializeRuntimesList();
        Asserts.isTrue(!cat.getRuntimes().isEmpty());
        final RuntimeInfo info = cat.getRuntimes().iterator().next();
        Asserts.isTrue(info != RuntimeInfo.NO_RUNTIME_INFO,
                "no default info found");

        final RuntimeData data = new RuntimeData(info, "run");
        data.setNodeName("etest");
        data.setLongName(false);
        data.setCookie("c");

        runtime = new ErlRuntime(data);
        runtime.startAndWait();
        process = runtime.getProcess();
        Asserts.isNotNull(process, "beam process");
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
        Asserts.isTrue(val == -1, "process exited " + val);
        Asserts.isTrue(runtime.isRunning(), "not running");
        final IRpcSite site = runtime.getRpcSite();
        OtpErlangObject r;
        try {
            r = site.call("erlang", "now", "");
        } catch (final RpcException e) {
            r = null;
        }
        Asserts.isNotNull(r, "rpc not working");
        try {
            runtime.stopAndWait();
        } catch (final Throwable t) {
            System.out.println("EXCEPTION:::: " + t);
        }
        expect(-1, State.TERMINATED);
    }

    @Test
    public void shutdownIsDetected() {
        final IRpcSite site = runtime.getRpcSite();
        try {
            site.cast("erlang", "halt", "i", 0);
        } catch (final RpcException e1) {
        }
        expect(0, State.TERMINATED);
    }

    @Test
    public void crashIsDetected() {
        process.destroy();
        expect(143, State.FAILED);
    }

    @Test
    public void haltIsDetected() throws RpcException {
        runtime.getRpcSite().cast("erlang", "halt", "i", 136);
        expect(136, State.FAILED);
    }

    private void expect(final int code, final State state) {
        while (runtime.isRunning()) {
            try {
                Thread.sleep(ErlRuntime.POLL_INTERVAL);
            } catch (final InterruptedException e) {
            }
        }
        int val;
        try {
            val = process.exitValue();
        } catch (final IllegalThreadStateException e) {
            val = -1;
        }
        Asserts.isTrue(val == code, "process exited with code " + val);
        Asserts.isTrue(runtime.state() == state, "state: " + runtime.state());
    }

}
