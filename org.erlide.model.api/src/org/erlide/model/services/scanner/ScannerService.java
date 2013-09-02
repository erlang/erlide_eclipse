package org.erlide.model.services.scanner;

import java.util.List;

import org.erlide.model.erlang.ErlToken;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface ScannerService {

    public abstract void initialScan(String module, String path,
            String initialText, boolean logging);

    public abstract void create(String module);

    public abstract void addref(String module);

    public abstract void dispose(String module);

    public abstract ErlToken getTokenAt(String module, int offset);

    public abstract void replaceText(String module, int offset,
            int removeLength, String newText);

    /**
     * @param string
     * @param offset
     * @return
     * @throws BackendException
     */
    public abstract List<ErlToken> lightScanString(String string, int offset)
            throws ScannerException;

    public abstract OtpErlangObject checkAll(String module, String text,
            boolean getTokens);

    public abstract String getText(String scannerName);

    public abstract boolean dumpLog(String scannerName,
            String dumpLocationFilename);

}
