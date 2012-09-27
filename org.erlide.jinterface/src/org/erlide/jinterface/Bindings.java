package org.erlide.jinterface;

import java.util.Collection;
import java.util.Map;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.SignatureException;

public interface Bindings {

    OtpErlangObject get(final String name);

    int getInt(final String name) throws OtpErlangException;

    long getLong(final String name) throws OtpErlangException;

    String getAtom(final String name) throws OtpErlangException;

    String getQuotedAtom(final String name) throws OtpErlangException;

    String getString(final String name) throws OtpErlangException;

    Collection<OtpErlangObject> getList(final String name)
            throws OtpErlangException;

    OtpErlangObject[] getTuple(final String name) throws OtpErlangException;

    OtpErlangPid getPid(final String name) throws OtpErlangException;

    <T> T getAs(final String name, final Class<T> cls)
            throws SignatureException;

    void put(final String name, final OtpErlangObject value);

    Map<String, OtpErlangObject> getAll();

}
