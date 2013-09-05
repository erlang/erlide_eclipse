package org.erlide.model.services.scanner;

import java.util.List;

import org.erlide.model.erlang.ErlToken;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface BasicScannerService {

    void initialScan(String module, String path, String initialText,
            boolean logging);

    void create(String module);

    void addref(String module);

    void dispose(String module);

    ErlToken getTokenAt(String module, int offset);

    void replaceText(String module, int offset, int removeLength, String newText);

    List<ErlToken> lightScanString(String string, int offset)
            throws ScannerException;

    OtpErlangObject checkAll(String module, String text, boolean getTokens);

    String getText(String scannerName);

    boolean dumpLog(String scannerName, String dumpLocationFilename);

}
