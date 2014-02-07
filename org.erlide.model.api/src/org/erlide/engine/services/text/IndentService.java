package org.erlide.engine.services.text;

import java.util.Map;

import org.erlide.engine.services.ErlangService;
import org.erlide.runtime.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;

public interface IndentService extends ErlangService {

    IndentResult indentLine(String oldLine, String txt, String insertedText, int tabw,
            boolean useTabs, Map<String, String> prefs) throws RpcException,
            OtpErlangRangeException;

    OtpErlangObject indentLines(int offset, int length, String text, int tabw,
            boolean useTabs, Map<String, String> prefs) throws RpcException;

    OtpErlangObject templateIndentLines(String prefix, String text, int tabw,
            boolean useTabs, Map<String, String> prefs) throws RpcException;

}
