package org.erlide.runtime.service;

import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.erlide.util.services.Provider;

import com.google.common.collect.Maps;
import com.google.common.util.concurrent.MoreExecutors;
import com.google.common.util.concurrent.Service;

public class RestartableService implements Service {

    private Service delegate;
    private final ServiceRestartPolicy policy;
    private final Provider<Service> factory;
    private final Map<Listener, Executor> listeners;

    public RestartableService(final Provider<Service> factory,
            final ServiceRestartPolicy policy) {
        this.factory = factory;
        this.policy = policy;
        listeners = Maps.newHashMap();
    }

    private final class RestartListener extends Listener {
        @Override
        public void failed(final State from, final Throwable failure) {
            if (policy.shouldRestart()) {
                startAsync();
            } else {
                for (final Entry<Listener, Executor> l : listeners.entrySet()) {
                    l.getKey().failed(from, failure);
                }
            }
        }

        @Override
        public void starting() {
            for (final Entry<Listener, Executor> l : listeners.entrySet()) {
                l.getKey().starting();
            }
        }

        @Override
        public void running() {
            for (final Entry<Listener, Executor> l : listeners.entrySet()) {
                l.getKey().running();
            }
        }

        @Override
        public void stopping(final Service.State from) {
            for (final Entry<Listener, Executor> l : listeners.entrySet()) {
                l.getKey().stopping(from);
            }
        }

        @Override
        public void terminated(final State from) {
            for (final Entry<Listener, Executor> l : listeners.entrySet()) {
                l.getKey().terminated(from);
            }
        }

    }

    /**
     * Used for testing only.
     *
     * @return delegate
     */
    public Service getDelegate() {
        return delegate;
    }

    @Override
    public Service startAsync() {
        delegate = factory.get();
        delegate.addListener(new RestartListener(), MoreExecutors.directExecutor());
        delegate.startAsync();
        policy.notifyRestart();
        return this;
    }

    @Override
    public boolean isRunning() {
        return delegate.isRunning();
    }

    @Override
    public State state() {
        return delegate.state();
    }

    @Override
    public Service stopAsync() {
        delegate.stopAsync();
        return this;
    }

    @Override
    public void awaitRunning() {
        delegate.awaitRunning();
    }

    @Override
    public void awaitRunning(final long timeout, final TimeUnit unit)
            throws TimeoutException {
        delegate.awaitRunning(timeout, unit);
    }

    @Override
    public void awaitTerminated() {
        delegate.awaitTerminated();
    }

    @Override
    public void awaitTerminated(final long timeout, final TimeUnit unit)
            throws TimeoutException {
        delegate.awaitTerminated(timeout, unit);
    }

    @Override
    public Throwable failureCause() {
        return delegate.failureCause();
    }

    @Override
    public void addListener(final Listener listener, final Executor executor) {
        synchronized (listeners) {
            listeners.put(listener, executor);
        }
    }

}
