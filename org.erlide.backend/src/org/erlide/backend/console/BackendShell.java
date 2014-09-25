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
import org.erlide.runtime.shell.BackendShellEvent;
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

    public BackendShell(final IBackend backend, final String id, final OtpErlangPid server) {
        this.backend = backend;
        fId = id;
        this.server = server;
        requests = new ArrayList<IoRequest>(1000);
        listeners = new ArrayList<BackendShellListener>();
    }

    @Override
    public void open() {
        final ErlangEventHandler handler = new ConsoleEventHandler(backend.getName(),
                this);
        backend.getRuntime().registerEventListener(handler);
    }

    @Override
    public void close() {
        if (server != null && backend.getOtpRpc() != null) {
            backend.getOtpRpc().send(server, new OtpErlangAtom("stop"));
        }
        server = null;
    }

    @Override
    public void send(final String string) {
        if (server != null) {
            backend.getOtpRpc().send(
                    server,
                    OtpErlang.mkTuple(new OtpErlangAtom("input"), new OtpErlangString(
                            string)));
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
    private int length = 0;

    @Override
    public void input(final String s0) {
        String s = s0;
        if (!s.endsWith("\n")) {
            s += "\n";
        }
        final IoRequest request = new IoRequest(s, IoRequestKind.INPUT);
        request.setStart(length);
        final int prevLength = length;
        addRequest(request);
        notifyListeners(makeEvent(prevLength, request));
    }

    private void addRequest(final IoRequest req) {
        final int reqLength = req.getLength();
        length += reqLength;
        synchronized (requests) {
            requests.add(req);
        }
    }

    @Override
    public void add(final OtpErlangObject msg) {
        deleteOldItems();
        final int prevLength = length;
        final IoRequest request = addRequestFromTuple(msg);
        if (request != null) {
            notifyListeners(makeEvent(prevLength, request));
        }
    }

    @Override
    public void add(final String text, final IoRequestKind kind) {
        if (IoRequest.RE_PROMPT.matcher(text).matches()) {
            return;
        }
        final IoRequest request = new IoRequest(text, kind);
        request.setStart(length);
        final int prevLength = length;
        addRequest(request);
        notifyListeners(makeEvent(prevLength, request));
    }

    private BackendShellEvent makeEvent(final int prevLength, final IoRequest request) {
        return new BackendShellEvent(prevLength, 0, request.getMessage());
    }

    private IoRequest addRequestFromTuple(final OtpErlangObject msg) {
        if (!(msg instanceof OtpErlangTuple)) {
            return null;
        }
        final IoRequest request = new IoRequest((OtpErlangTuple) msg);
        request.setStart(length);
        addRequest(request);
        return request;
    }

    private void deleteOldItems() {
        final int prevLength = length;
        synchronized (requests) {
            if (requests.size() > MAX_REQUESTS) {
                requests.subList(0, DELTA_REQUESTS).clear();
                final IoRequest first = requests.get(0);
                final int start = first.getStart();
                length = 0;
                for (final IoRequest request : requests) {
                    request.setStart(request.getStart() - start);
                    length += request.getLength();
                }
            }
        }
        if (length != prevLength) {
            notifyListeners(new BackendShellEvent(0, length - prevLength, ""));
        }
    }

    @Override
    public IoRequest findAtPos(final int thePos) {
        synchronized (requests) {
            for (final IoRequest req : requests) {
                if (req.getStart() <= thePos && req.getStart() + req.getLength() > thePos) {
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
        final int prevLength = length;
        final StringBuffer text = new StringBuffer();
        synchronized (requests) {
            deleteOldItems();
            for (final OtpErlangObject element : msgs) {
                final IoRequest request = addRequestFromTuple(element);
                if (request != null) {
                    requests.add(request);
                    text.append(request.getMessage());
                }
            }
        }
        if (!requests.isEmpty()) {
            notifyListeners(new BackendShellEvent(prevLength, 0, text.toString()));
        }
    }

    @Override
    public void dispose() {
        listeners.clear();
    }

    @Override
    public synchronized void addListener(final BackendShellListener listener) {
        sendEarlierRequests(listener);
        synchronized (listeners) {
            if (!listeners.contains(listener)) {
                listeners.add(listener);
            }
        }
    }

    private void sendEarlierRequests(final BackendShellListener listener) {
        int alength = 0;
        synchronized (requests) {
            for (final IoRequest request : requests) {
                listener.changed(makeEvent(alength, request));
                alength += request.getLength();
            }
        }
    }

    @Override
    public void removeListener(final BackendShellListener listener) {
        synchronized (listeners) {
            listeners.remove(listener);
        }
    }

    private void notifyListeners(final BackendShellEvent event) {
        final List<BackendShellListener> listenersCopy;
        synchronized (listeners) {
            listenersCopy = Lists.newArrayList(listeners);
        }
        for (final BackendShellListener listener : listenersCopy) {
            listener.changed(event);
        }
    }

    @Override
    public int getTextLength() {
        return length;
    }

    @Override
    public String getText() {
        final StringBuffer result = new StringBuffer();
        synchronized (requests) {
            for (final IoRequest request : requests) {
                result.append(request.getMessage());
            }
        }
        return result.toString();
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
