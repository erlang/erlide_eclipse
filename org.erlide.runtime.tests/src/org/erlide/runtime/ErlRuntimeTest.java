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
        runtime.stopAndWait();
        Asserts.isTrue(runtime.state() == State.TERMINATED);
    }

    @Test
    public void shutdownIsDetected() {
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
            r = site.call("init", "stop", "");
        } catch (final RpcException e) {
            r = null;
        }
        Asserts.isNotNull(r, "rpc not working");

        while (runtime.state() == State.RUNNING) {
            try {
                Thread.sleep(200);
            } catch (final InterruptedException e) {
            }
        }
        Asserts.isTrue(runtime.state() == State.TERMINATED);
        try {
            val = process.exitValue();
        } catch (final IllegalThreadStateException e) {
            val = -1;
        }
        Asserts.isTrue(val == 0, "process exited " + val);

    }

    @Test
    public void crashIsDetected() {
        int val;
        try {
            val = process.exitValue();
        } catch (final IllegalThreadStateException e) {
            val = -1;
        }
        Asserts.isTrue(val == -1, "process exited " + val);
        Asserts.isTrue(runtime.isRunning(), "not running");

        process.destroy();
        val = -1;
        while (val < 0) {
            try {
                val = process.exitValue();
            } catch (final IllegalThreadStateException e) {
                val = -1;
            }
        }
        try {
            Thread.sleep(ErlRuntime.POLL_INTERVAL);
        } catch (final InterruptedException e) {
        }
        try {
            val = process.exitValue();
        } catch (final IllegalThreadStateException e) {
            val = -1;
        }
        Asserts.isTrue(val > 0, "process didn't crash");
        Asserts.isTrue(runtime.state() == State.FAILED,
                "state: " + runtime.state() + " " + val);

    }
}
