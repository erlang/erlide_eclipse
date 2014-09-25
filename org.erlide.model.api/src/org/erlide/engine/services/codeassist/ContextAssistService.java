package org.erlide.engine.services.codeassist;

import java.util.Collection;

import org.erlide.runtime.api.IOtpRpc;

import com.ericsson.otp.erlang.OtpErlangList;

public interface ContextAssistService {

    // -define(NO_RECORD, 0).
    // -define(RECORD_NAME, 1).
    // -define(RECORD_FIELD, 2).
    public static final int RECORD_NAME = 1;
    public static final int RECORD_FIELD = 2;

    Collection<String> getVariables(String src, String prefix);

    RecordCompletion checkRecordCompletion(IOtpRpc buildBackend, String substring);

    OtpErlangList getFunctionHead(String name, int arity);

}
