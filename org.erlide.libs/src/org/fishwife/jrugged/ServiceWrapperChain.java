/* ServiceWrapperChain.java
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;

public class ServiceWrapperChain implements ServiceWrapper {

    private final List<ServiceWrapper> wrappers;

    public ServiceWrapperChain(final Collection<ServiceWrapper> wrappers) {
        final ArrayList<ServiceWrapper> rev = new ArrayList<>();
        for (final ServiceWrapper wrapper : wrappers) {
            rev.add(0, wrapper);
        }
        this.wrappers = rev;
    }

    private <T> Callable<T> wrap(final Callable<T> c, final ServiceWrapper wrapper) {
        return new Callable<T>() {
            @Override
            public T call() throws Exception {
                return wrapper.invoke(c);
            }
        };
    }

    private Runnable wrap(final Runnable r, final ServiceWrapper wrapper) {
        return new Runnable() {
            @Override
            public void run() {
                try {
                    wrapper.invoke(r);
                } catch (final Exception e) {
                    throw new RuntimeException(e);
                }
            }
        };
    }

    @Override
    public <T> T invoke(final Callable<T> c) throws Exception {
        Callable<T> c2 = c;
        for (final ServiceWrapper wrapper : wrappers) {
            c2 = wrap(c2, wrapper);
        }
        return c2.call();
    }

    @Override
    public void invoke(final Runnable r) throws Exception {
        Runnable r2 = r;
        for (final ServiceWrapper wrapper : wrappers) {
            r2 = wrap(r2, wrapper);
        }
        r2.run();
    }

    @Override
    public <T> T invoke(final Runnable r, final T result) throws Exception {
        invoke(r);
        return result;
    }

}
