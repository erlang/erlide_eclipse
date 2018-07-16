package org.erlide.runtime;

import static com.google.common.truth.Truth.assertThat;

import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.internal.OtpNodeProxy;
import org.erlide.runtime.rpc.IOtpRpc;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeInfoCatalog;
import org.erlide.util.ErlLogger;
import org.erlide.util.HostnameChecker;
import org.erlide.util.SystemConfiguration;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.util.concurrent.Service.State;

public class OtpNodeProxyTest {

    private Process process;
    private OtpNodeProxy runtime;
    private RuntimeInfo info;

    @Before
    public void prepareRuntime() {
        final RuntimeInfoCatalog cat = new RuntimeInfoCatalog();
        cat.initializeRuntimesList();
        assertThat(cat.getRuntimes()).isNotEmpty();
        info = cat.getRuntimes().iterator().next();
        assertThat(info).isNotEqualTo(RuntimeInfo.NO_RUNTIME_INFO);

        HostnameChecker.getInstance().detectHostNames(info.getOtpHome());

        final RuntimeData data = new RuntimeData(info, "run");
        data.setNodeName("etest" + System.currentTimeMillis());
        data.setLongName(false);
        data.setCookie("c");
        data.setManaged(true);
        data.setRestartable(false);

        runtime = new OtpNodeProxy(data);
        runtime.ensureRunning();
        process = runtime.getProcess();
        assertThat(process).isNotNull();
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
        assertThat(val).isEqualTo(-1);
        assertThat(runtime.isRunning()).isEqualTo(true);
        final IOtpRpc site = runtime.getOtpRpc();
        OtpErlangObject r;
        try {
            r = site.call("erlang", "now", "");
        } catch (final RpcException e) {
            r = null;
        }
        assertThat(r).isNotNull();
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

//    @Test
//    public void crashIsDetected() throws InterruptedException {
//        process.destroy();
//        Thread.sleep(200);
//        int code;
//        if (SystemConfiguration.getInstance().isOnWindows()) {
//            code = 1;
//        } else {
//            code = 143;
//        }
//        expect(runtime, process, code, State.FAILED);
//    }

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
        if (aProcess != null) {
            int val;
            try {
                val = aProcess.waitFor();
            } catch (final InterruptedException e) {
                val = -1;
            }
            assertThat(val).isEqualTo(code);
        }
        assertThat(aRuntime.state()).isEqualTo(state);
    }

    @Test
    public void nonManagedRuntimeWorks() {
        final RuntimeData data = new RuntimeData(info, "run");
        data.setNodeName(runtime.getNodeName());
        data.setLongName(false);
        data.setCookie("c");
        data.setManaged(false);

        final OtpNodeProxy runtime2 = new OtpNodeProxy(data);
        runtime2.ensureRunning();

        final Process process2 = runtime2.getProcess();
        assertThat(runtime2.isRunning()).isEqualTo(true);
        assertThat(process2).isNull();

        final IOtpRpc site = runtime2.getOtpRpc();
        OtpErlangObject r;
        try {
            r = site.call("erlang", "now", "");
        } catch (final RpcException e) {
            r = null;
        }
        assertThat(r).isNotNull();
        try {
            runtime2.dispose();
        } catch (final Throwable t) {
            ErlLogger.error(t);
        }
        try {
            Thread.sleep(100);
        } catch (final InterruptedException e) {
        }
        expect(runtime2, process2, -1, State.TERMINATED);
        assertThat(runtime.state()).isEqualTo(State.RUNNING);
    }
}
