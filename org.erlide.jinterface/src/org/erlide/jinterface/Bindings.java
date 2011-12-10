package org.erlide.jinterface;

import java.util.Collection;
import java.util.Map;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.SignatureException;

public interface Bindings {

    public abstract OtpErlangObject get(final String name);

    public abstract int getInt(final String name) throws OtpErlangException;

    public abstract long getLong(final String name) throws OtpErlangException;

    public abstract String getAtom(final String name) throws OtpErlangException;

    public abstract String getQuotedAtom(final String name)
            throws OtpErlangException;

    public abstract String getString(final String name)
            throws OtpErlangException;

    public abstract Collection<OtpErlangObject> getList(final String name)
            throws OtpErlangException;

    public abstract OtpErlangObject[] getTuple(final String name)
            throws OtpErlangException;

    public abstract <T> T getAs(final String name, final Class<T> cls)
            throws SignatureException;

    public abstract void put(final String name, final OtpErlangObject value);

    public abstract Map<String, OtpErlangObject> getAll();

    @Override
    public abstract String toString();

}
