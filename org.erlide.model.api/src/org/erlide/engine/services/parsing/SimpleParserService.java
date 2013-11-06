package org.erlide.engine.services.parsing;

import java.util.Collection;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface SimpleParserService {

    Collection<OtpErlangObject> parse(String s);

}
