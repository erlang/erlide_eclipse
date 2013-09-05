package org.erlide.model.services.scanner;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface InternalScanner {

    void create(String module);

    boolean dumpLog(String scannerName, String dumpLocationFilename);

    OtpErlangObject checkAll(String module, String text, boolean getTokens);

}
