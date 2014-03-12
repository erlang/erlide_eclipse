package org.erlide.engine.services.parsing;

import java.util.List;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface SimpleParserService {

    List<OtpErlangObject> parse(String s);

}
