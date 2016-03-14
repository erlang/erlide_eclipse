/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.util.erlang;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public final class OtpBindings implements Map<String, OtpErlangObject> {

    private final Map<String, OtpErlangObject> bindings;

    public OtpBindings() {
        bindings = new HashMap<String, OtpErlangObject>();
    }

    public OtpBindings(final OtpBindings binds) {
        this();
        merge(binds);
    }

    public void merge(final OtpBindings binds) {
        bindings.putAll(binds.getAll());
    }

    public int getInt(final String name) throws OtpErlangException {
        final OtpErlangObject r = get(name);
        if (r instanceof OtpErlangLong) {
            return ((OtpErlangLong) r).intValue();
        }
        throw new OtpErlangException("value is not an integer");
    }

    public long getLong(final String name) throws OtpErlangException {
        final OtpErlangObject r = get(name);
        if (r instanceof OtpErlangLong) {
            return ((OtpErlangLong) r).longValue();
        }
        throw new OtpErlangException("value is not an integer");
    }

    public String getAtom(final String name) throws OtpErlangException {
        final OtpErlangObject r = get(name);
        if (r instanceof OtpErlangAtom) {
            return ((OtpErlangAtom) r).atomValue();
        }
        throw new OtpErlangException("value is not an atom");
    }

    public String getQuotedAtom(final String name) throws OtpErlangException {
        final OtpErlangObject r = get(name);
        if (r instanceof OtpErlangAtom) {
            return ((OtpErlangAtom) r).toString();
        }
        throw new OtpErlangException("value is not an atom");
    }

    public String getString(final String name) throws OtpErlangException {
        final OtpErlangObject r = get(name);
        if (r instanceof OtpErlangString) {
            return ((OtpErlangString) r).stringValue();
        }
        throw new OtpErlangException("value is not a string");
    }

    public Collection<OtpErlangObject> getList(final String name)
            throws OtpErlangException {
        final OtpErlangObject r = get(name);
        if (r instanceof OtpErlangList) {
            return Lists.newArrayList(((OtpErlangList) r).elements());
        }
        throw new OtpErlangException("value is not a list");
    }

    public OtpErlangObject[] getTuple(final String name) throws OtpErlangException {
        final OtpErlangObject r = get(name);
        if (r instanceof OtpErlangTuple) {
            return ((OtpErlangTuple) r).elements();
        }
        throw new OtpErlangException("value is not a tuple");
    }

    public OtpErlangPid getPid(final String name) throws OtpErlangException {
        final OtpErlangObject r = get(name);
        if (r instanceof OtpErlangPid) {
            return (OtpErlangPid) r;
        }
        throw new OtpErlangException("value is not a pid");
    }

    @SuppressWarnings("unchecked")
    public <T> T getAs(final String name, final Class<T> cls) throws SignatureException {
        final OtpErlangObject v = get(name);
        return (T) TypeConverter.erlang2java(v, cls);
    }

    public Map<String, OtpErlangObject> getAll() {
        return Collections.unmodifiableMap(bindings);
    }

    @Override
    public String toString() {
        return bindings.toString();
    }

    public OtpErlangBinary getBinary(final String name) throws OtpErlangException {
        final OtpErlangObject r = get(name);
        if (r instanceof OtpErlangBinary) {
            return (OtpErlangBinary) r;
        }
        throw new OtpErlangException("value is not a binary");
    }

    public String getAsString(final String name) {
        final OtpErlangObject r = get(name);
        return ErlUtils.asString(r);
    }

    @Override
    public int size() {
        return bindings.size();
    }

    @Override
    public boolean isEmpty() {
        return bindings.isEmpty();
    }

    @Override
    public boolean containsKey(final Object key) {
        return bindings.containsKey(key);
    }

    @Override
    public boolean containsValue(final Object value) {
        return bindings.containsValue(value);
    }

    @Override
    public OtpErlangObject get(final Object key) {
        return bindings.get(key);
    }

    @Override
    public OtpErlangObject put(final String key, final OtpErlangObject value) {
        return bindings.put(key, value);
    }

    @Override
    public OtpErlangObject remove(final Object key) {
        return bindings.remove(key);
    }

    @Override
    public void putAll(final Map<? extends String, ? extends OtpErlangObject> m) {
        bindings.putAll(m);
    }

    @Override
    public void clear() {
        bindings.clear();
    }

    @Override
    public Set<String> keySet() {
        return bindings.keySet();
    }

    @Override
    public Collection<OtpErlangObject> values() {
        return bindings.values();
    }

    @Override
    public Set<java.util.Map.Entry<String, OtpErlangObject>> entrySet() {
        return bindings.entrySet();
    }

}
