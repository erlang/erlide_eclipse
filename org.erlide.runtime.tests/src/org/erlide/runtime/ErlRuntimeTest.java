package org.erlide.runtime;

import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.internal.ErlRuntime;
import org.erlide.runtime.internal.ManagedErlRuntime;
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
    private ManagedErlRuntime runtime;

    @Before
    public void prepareRuntime() {
        final RuntimeInfoCatalog cat = new RuntimeInfoCatalog();
        cat.initializeRuntimesList();
        Asserts.isTrue(!cat.getRuntimes().isEmpty());
        final RuntimeInfo info = cat.getRuntimes().iterator().next();
        Asserts.isTrue(info != RuntimeInfo.NO_RUNTIME_INFO,
                "no default info found");

        final RuntimeData data = new RuntimeData(info, "run");
        data.setNodeName("etest" + System.currentTimeMillis());
        data.setLongName(false);
        data.setCookie("c");

        runtime = new ManagedErlRuntime(data);
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
        expect(runtime, process, -1, State.TERMINATED);
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
        Asserts.isTrue(aRuntime.state() == state, "state: " + aRuntime.state());
        if (aProcess != null) {
            int val;
            try {
                val = aProcess.exitValue();
            } catch (final IllegalThreadStateException e) {
                val = -1;
            }
            Asserts.isTrue(val == code, "process exited with code " + val);
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
        Asserts.isTrue(process2 == null, "beam process " + process2);
        Asserts.isTrue(runtime2.isRunning(), "not running");

        final IRpcSite site = runtime2.getRpcSite();
        OtpErlangObject r;
        try {
            r = site.call("erlang", "now", "");
        } catch (final RpcException e) {
            r = null;
        }
        Asserts.isNotNull(r, "rpc not working");
        try {
            runtime2.stopAndWait();
        } catch (final Throwable t) {
            System.out.println("EXCEPTION:::: " + t);
        }
        expect(runtime2, process2, -1, State.TERMINATED);
        Asserts.isTrue(runtime.state() == State.RUNNING,
                "state: " + runtime.state());
    }
}
