/*******************************************************************************
 * Copyright (c) 2006 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend.console;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.erlide.backend.api.IBackend;
import org.erlide.runtime.events.ErlangEventHandler;
import org.erlide.runtime.shell.BackendShellListener;
import org.erlide.runtime.shell.IBackendShell;
import org.erlide.runtime.shell.IoRequest;
import org.erlide.runtime.shell.IoRequest.IoRequestKind;
import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.OtpErlang;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class BackendShell implements IBackendShell {

    private final IBackend backend;
    private OtpErlangPid server;
    private final String fId;

    public BackendShell(final IBackend backend, final String id,
            final OtpErlangPid server) {
        this.backend = backend;
        fId = id;
        this.server = server;
        requests = new ArrayList<IoRequest>(1000);
        listeners = new ArrayList<BackendShellListener>();

        final ErlangEventHandler handler = new ConsoleEventHandler(
                backend.getName(), this);
        backend.getRuntime().registerEventListener(handler);
    }

    @Override
    public void close() {
        if (server != null) {
            backend.getRpcSite().send(server, new OtpErlangAtom("stop"));
        }
        server = null;
    }

    @Override
    public void send(final String string) {
        if (server != null) {
            backend.getRpcSite().send(
                    server,
                    OtpErlang.mkTuple(new OtpErlangAtom("input"),
                            new OtpErlangString(string)));
        } else {
            try {
                backend.input(string);
            } catch (final IOException e) {
                ErlLogger.error(e);
            }
        }
    }

    @Override
    public String getId() {
        return fId;
    }

    private static final int MAX_REQUESTS = 5000;
    private static final int DELTA_REQUESTS = 500;

    private final List<IoRequest> requests;
    private final List<BackendShellListener> listeners;
    private int pos = 0;

    @Override
    public void input(final String s0) {
        String s = s0;
        if (!s.endsWith("\n")) {
            s += "\n";
        }
        final IoRequest req = new IoRequest(s, IoRequestKind.INPUT);
        req.setStart(pos);
        pos += req.getLength();
        synchronized (requests) {
            requests.add(req);
        }
        notifyListeners();
    }

    @Override
    public void add(final OtpErlangObject msg) {
        synchronized (requests) {
            deleteOldItems();
            final IoRequest req = doAdd(msg);
            if (req == null) {
                return;
            }
        }
        notifyListeners();
    }

    @Override
    public void add(final String text, final IoRequestKind kind) {
        if (IoRequest.RE_PROMPT.matcher(text).matches()) {
            return;
        }
        final IoRequest req = new IoRequest(text, kind);
        req.setStart(pos);
        pos += req.getLength();
        synchronized (requests) {
            // TODO this is not complete
            // IoRequest last = requests.get(requests.size() - 1);
            // if (last.getKind() == IoRequestKind.PROMPT) {
            // requests.remove(last);
            // }

            // System.out.println(req.toString());
            requests.add(req);

            // if (last.getKind() == IoRequestKind.PROMPT) {
            // requests.add(last);
            // }
        }
        notifyListeners();
    }

    private IoRequest doAdd(final OtpErlangObject msg) {
        if (!(msg instanceof OtpErlangTuple)) {
            return null;
        }
        final IoRequest req = new IoRequest((OtpErlangTuple) msg);
        req.setStart(pos);
        pos += req.getLength();

        requests.add(req);

        return req;
    }

    private void deleteOldItems() {
        // TODO use a configuration for this
        synchronized (requests) {
            if (requests.size() > MAX_REQUESTS) {
                requests.subList(0, DELTA_REQUESTS).clear();
                final IoRequest first = requests.get(0);
                final int start = first.getStart();
                for (final IoRequest areq : requests) {
                    areq.setStart(areq.getStart() - start);
                }
            }
        }
    }

    @Override
    public IoRequest findAtPos(final int thePos) {
        synchronized (requests) {
            for (final IoRequest req : requests) {
                if (req.getStart() <= thePos
                        && req.getStart() + req.getLength() > thePos) {
                    return req;
                }
            }
            return null;
        }
    }

    @Override
    public List<IoRequest> getAllFrom(final OtpErlangPid sender) {
        final List<IoRequest> result = new ArrayList<IoRequest>();
        synchronized (requests) {
            for (final IoRequest element : requests) {
                if (element.getSender().equals(sender)) {
                    result.add(element);
                }
            }
        }
        return result;
    }

    @Override
    public void add(final List<OtpErlangObject> msgs) {
        synchronized (requests) {
            deleteOldItems();
            for (final OtpErlangObject element : msgs) {
                doAdd(element);
            }
        }
        notifyListeners();
    }

    @Override
    public void dispose() {
        listeners.clear();
    }

    @Override
    public synchronized void addListener(final BackendShellListener listener) {
        synchronized (listeners) {
            if (!listeners.contains(listener)) {
                listeners.add(listener);
            }
        }
    }

    @Override
    public void removeListener(final BackendShellListener listener) {
        synchronized (listeners) {
            listeners.remove(listener);
        }
    }

    private void notifyListeners() {
        final List<BackendShellListener> listenersCopy;
        synchronized (listeners) {
            listenersCopy = Lists.newArrayList(listeners);
        }
        for (final BackendShellListener listener : listenersCopy) {
            listener.changed();
        }
    }

    @Override
    public int getTextLength() {
        int res = 0;
        synchronized (requests) {
            for (final IoRequest req : requests) {
                res += req.getLength();
            }
        }
        return res;
    }

    @Override
    public String getText() {
        final StringBuffer res = new StringBuffer();
        synchronized (requests) {
            for (final IoRequest req : requests) {
                res.append(req.getMessage());
            }
        }
        return res.toString();
    }

    @Override
    public String[] getLastMessages(final int nMessages) {
        final List<String> result = Lists.newArrayListWithCapacity(nMessages);
        synchronized (requests) {
            final int size = requests.size();
            final int n = Math.min(nMessages, size);
            for (int i = size - n; i < size; ++i) {
                result.add(requests.get(i).getMessage());
            }
        }
        return result.toArray(new String[nMessages]);
    }

}
