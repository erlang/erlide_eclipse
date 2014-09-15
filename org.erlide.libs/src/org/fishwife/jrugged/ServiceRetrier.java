/* ServiceRetrier.java
 *
 * Copyright 2009-2012 Comcast Interactive Media, LLC.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.fishwife.jrugged;

import java.util.concurrent.Callable;

/**
 * Calls a service multiple times until the call succeeds or the maximum number
 * of tries is exceeded. A delay can be configured between calls and that delay
 * can be constant or configured to double between each call.
 */
public class ServiceRetrier implements ServiceWrapper {

    public static final int DEFAULT_MAX_TRIES = 10;
    public static final int DEFAULT_DELAY = 1000;

    private int _delay = DEFAULT_DELAY;
    private int _maxTries = DEFAULT_MAX_TRIES;
    private boolean _doubleDelay = false;
    private boolean _throwCauseException = false;
    private Class<? extends Throwable>[] _retryOn = null;

    public ServiceRetrier(final int delay, final int maxTries) {
        setDelay(delay);
        setMaxTries(maxTries);
    }

    public ServiceRetrier(final int delay, final int maxTries,
            final boolean doubleDelay, final boolean throwCauseException,
            final Class<? extends Throwable>[] retryOn) {
        setDelay(delay);
        setMaxTries(maxTries);
        setDoubleDelay(doubleDelay);
        setThrowCauseException(throwCauseException);
        setRetryOn(retryOn);
    }

    public ServiceRetrier() {
    }

    @Override
    public <V> V invoke(final Callable<V> c) throws Exception {

        int tries = 0;
        int delay = _delay;

        while (true) {
            try {
                return c.call();
            } catch (final Exception cause) {

                // If this type of Exception should be retried...
                if (shouldRetry(cause)) {
                    tries++;

                    // Don't delay after max tries reached.
                    if (tries < _maxTries) {

                        if (delay > 0) {
                            sleep(delay);
                        }

                        // Double the next delay if configured to do so.
                        if (_doubleDelay) {
                            delay = delay * 2;
                        }

                        // Try again.
                        continue;
                    }
                }

                if (_throwCauseException) {
                    throw cause;
                } else {
                    throw new Exception("Call failed " + tries + " times",
                            cause);
                }
            }
        }
    }

    private boolean shouldRetry(final Throwable cause) {
        if (_retryOn == null || _retryOn.length == 0) {
            return true;
        }

        for (final Class<? extends Throwable> clazz : _retryOn) {
            if (clazz.isInstance(cause)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void invoke(final Runnable r) throws Exception {

        final Callable<Void> adapter = new CallableAdapter<Void>(r);
        invoke(adapter);
    }

    @Override
    public <T> T invoke(final Runnable r, final T result) throws Exception {

        final Callable<T> adapter = new CallableAdapter<T>(r, result);
        return invoke(adapter);
    }

    public int getDelay() {
        return _delay;
    }

    public void setDelay(final int delay) {

        if (delay < 0) {
            throw new IllegalArgumentException("Delay cannot be negative");
        }

        _delay = delay;
    }

    public int getMaxTries() {
        return _maxTries;
    }

    public void setMaxTries(final int maxTries) {

        if (maxTries < 1) {
            throw new IllegalArgumentException(
                    "Maximum number of tries must be greater than zero");
        }

        _maxTries = maxTries;
    }

    public boolean isDoubleDelay() {
        return _doubleDelay;
    }

    public void setDoubleDelay(final boolean doubleDelay) {
        _doubleDelay = doubleDelay;
    }

    public boolean isThrowCauseException() {
        return _throwCauseException;
    }

    public void setThrowCauseException(final boolean throwCauseException) {
        _throwCauseException = throwCauseException;
    }

    public Class<? extends Throwable>[] getRetryOn() {
        return _retryOn;
    }

    public void setRetryOn(final Class<? extends Throwable>[] retryOn) {
        _retryOn = retryOn;
    }

    protected void sleep(final long millis) {
        try {
            Thread.sleep(millis);
        } catch (final InterruptedException e) {
            // Nothing much to do here.
        }
    }
}
