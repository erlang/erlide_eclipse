/* LatencyTracker.java
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
 * This is a statistics wrapper that records the latency of requests, both for
 * successes and failures. The most recent measurement is available for query,
 * and can be polled periodically to determine average latencies.
 */
public class LatencyTracker implements ServiceWrapper {
    private long lastSuccessMillis;
    private long lastFailureMillis;

    @Override
    public <T> T invoke(final Callable<T> c) throws Exception {
        final long start = System.currentTimeMillis();
        try {
            final T result = c.call();
            lastSuccessMillis = System.currentTimeMillis() - start;
            return result;
        } catch (final Exception e) {
            lastFailureMillis = System.currentTimeMillis() - start;
            throw e;
        }
    }

    @Override
    public void invoke(final Runnable r) throws Exception {
        final long start = System.currentTimeMillis();
        try {
            r.run();
            lastSuccessMillis = System.currentTimeMillis() - start;
        } catch (final Exception e) {
            lastFailureMillis = System.currentTimeMillis() - start;
            throw e;
        }
    }

    @Override
    public <T> T invoke(final Runnable r, final T result) throws Exception {
        final long start = System.currentTimeMillis();
        try {
            r.run();
            lastSuccessMillis = System.currentTimeMillis() - start;
            return result;
        } catch (final Exception e) {
            lastFailureMillis = System.currentTimeMillis() - start;
            throw e;
        }
    }

    /**
     * Returns how long the last successful request took.
     * 
     * @return long request service time in milliseconds
     */
    public long getLastSuccessMillis() {
        return lastSuccessMillis;
    }

    /**
     * Returns how long the last failed request took.
     * 
     * @return long request service time in milliseconds.
     */
    public long getLastFailureMillis() {
        return lastFailureMillis;
    }
}
