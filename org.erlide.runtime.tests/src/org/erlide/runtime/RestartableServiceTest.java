package org.erlide.runtime;

import java.util.List;

import org.erlide.runtime.service.AlwaysRestartPolicy;
import org.erlide.runtime.service.CooldownRestartPolicy;
import org.erlide.runtime.service.NeverRestartPolicy;
import org.erlide.runtime.service.RestartableService;
import org.erlide.util.services.Provider;
import org.hamcrest.Matchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Test;

import com.google.common.collect.Lists;
import com.google.common.util.concurrent.AbstractExecutionThreadService;
import com.google.common.util.concurrent.MoreExecutors;
import com.google.common.util.concurrent.Service;
import com.google.common.util.concurrent.Service.Listener;
import com.google.common.util.concurrent.Service.State;

public class RestartableServiceTest {

    static class DummyService extends AbstractExecutionThreadService {

        private boolean running = false;
        private boolean crashed = false;

        @Override
        protected void run() throws Exception {
            while (running) {
                Thread.sleep(30);
                if (crashed) {
                    throw new RuntimeException("#hello#");
                }
            }
        }

        @Override
        protected void startUp() throws Exception {
            running = true;
        }

        @Override
        protected void triggerShutdown() {
            running = false;
        }

        public void triggerCrash() {
            crashed = true;
        }

    }

    private RestartableService service;
    private final Provider<Service> dummyFactory = new Provider<Service>() {
        @Override
        public Service get() {
            return new DummyService();
        }
    };

    @After
    public void cleanup() {
        if (service.state() != State.FAILED) {
            service.stopAsync().awaitTerminated();
        }
        service = null;
    }

    @Test
    public void shouldNotCrashAfterInterval_always() throws InterruptedException {
        service = new RestartableService(dummyFactory, new CooldownRestartPolicy(1000));
        service.startAsync().awaitRunning();
        final DummyService dummy = (DummyService) service.getDelegate();
        Thread.sleep(1100);
        dummy.triggerCrash();
        Thread.sleep(100);
        Assert.assertThat(dummy.state(), Matchers.is(State.FAILED));
        Assert.assertThat(service.state(), Matchers.is(State.RUNNING));
    }

    @Test
    public void shouldCrashBeforeInterval_always() throws InterruptedException {
        service = new RestartableService(dummyFactory, new CooldownRestartPolicy());
        service.startAsync().awaitRunning();
        final DummyService dummy = (DummyService) service.getDelegate();
        dummy.triggerCrash();
        Thread.sleep(100);
        Assert.assertThat(dummy.state(), Matchers.is(State.FAILED));
        Assert.assertThat(service.state(), Matchers.is(State.FAILED));
    }

    @Test
    public void shouldCrashBeforeInterval_secondTime() throws InterruptedException {
        service = new RestartableService(dummyFactory, new CooldownRestartPolicy(1000));
        service.startAsync().awaitRunning();
        final DummyService dummy = (DummyService) service.getDelegate();
        Thread.sleep(1100);
        dummy.triggerCrash();
        Thread.sleep(100);
        Assert.assertThat(dummy.state(), Matchers.is(State.FAILED));
        Assert.assertThat(service.state(), Matchers.is(State.RUNNING));

        final DummyService dummy2 = (DummyService) service.getDelegate();
        Assert.assertThat(dummy2, Matchers.is(Matchers.not(dummy)));
        dummy2.triggerCrash();
        Thread.sleep(100);
        Assert.assertThat(dummy2.state(), Matchers.is(State.FAILED));
        Assert.assertThat(service.state(), Matchers.is(State.FAILED));
    }

    @Test
    public void shouldNotCrashBeforeInterval_secondTime() throws InterruptedException {
        service = new RestartableService(dummyFactory, new CooldownRestartPolicy(1000));
        service.startAsync().awaitRunning();
        final DummyService dummy = (DummyService) service.getDelegate();
        Thread.sleep(1100);
        dummy.triggerCrash();
        Thread.sleep(100);
        Assert.assertThat(dummy.state(), Matchers.is(State.FAILED));
        Assert.assertThat(service.state(), Matchers.is(State.RUNNING));

        final DummyService dummy2 = (DummyService) service.getDelegate();
        Assert.assertThat(dummy2, Matchers.is(Matchers.not(dummy)));
        Thread.sleep(1100);
        dummy2.triggerCrash();
        Thread.sleep(100);
        Assert.assertThat(dummy2.state(), Matchers.is(State.FAILED));
        Assert.assertThat(service.state(), Matchers.is(State.RUNNING));
    }

    @Test
    public void shouldShutdown_always() throws InterruptedException {
        service = new RestartableService(dummyFactory, new AlwaysRestartPolicy());
        service.startAsync().awaitRunning();
        final DummyService dummy = (DummyService) service.getDelegate();
        dummy.triggerShutdown();
        Thread.sleep(100);
        Assert.assertThat(dummy.state(), Matchers.is(State.TERMINATED));
        Assert.assertThat(service.state(), Matchers.is(State.TERMINATED));
    }

    @Test
    public void shouldCrash_never() throws InterruptedException {
        service = new RestartableService(dummyFactory, new NeverRestartPolicy());
        service.startAsync().awaitRunning();
        final DummyService dummy = (DummyService) service.getDelegate();
        dummy.triggerCrash();
        Thread.sleep(100);
        Assert.assertThat(dummy.state(), Matchers.is(State.FAILED));
        Assert.assertThat(service.state(), Matchers.is(State.FAILED));
    }

    @Test
    public void shouldShutdown_never() throws InterruptedException {
        service = new RestartableService(dummyFactory, new NeverRestartPolicy());
        service.startAsync().awaitRunning();
        final DummyService dummy = (DummyService) service.getDelegate();
        dummy.triggerShutdown();
        Thread.sleep(100);
        Assert.assertThat(dummy.state(), Matchers.is(State.TERMINATED));
        Assert.assertThat(service.state(), Matchers.is(State.TERMINATED));
    }

    @Test
    public void shouldPostNotifications() throws InterruptedException {
        final List<String> events = Lists.newArrayList();

        service = new RestartableService(dummyFactory, new AlwaysRestartPolicy());
        service.addListener(new Listener() {
            @Override
            public void starting() {
                events.add("starting");
            }

            @Override
            public void running() {
                events.add("running");
            }

            @Override
            public void stopping(final State from) {
                events.add("stopping " + from);
            }

            @Override
            public void terminated(final State from) {
                events.add("terminated " + from);
            }

            @Override
            public void failed(final State from, final Throwable failure) {
                events.add("failed " + from);
            }
        }, MoreExecutors.sameThreadExecutor());
        service.startAsync().awaitRunning();
        Thread.sleep(100);
        DummyService dummy = (DummyService) service.getDelegate();
        dummy.triggerCrash();
        Thread.sleep(100);
        dummy = (DummyService) service.getDelegate();
        dummy.triggerCrash();
        Thread.sleep(100);
        service.stopAsync().awaitTerminated();
        Thread.sleep(100);

        System.out.println(events);
        Assert.assertThat(events,
                Matchers.contains("starting", "running", "starting", "running",
                        "starting", "running", "stopping RUNNING",
                        "terminated STOPPING"));
    }

}
