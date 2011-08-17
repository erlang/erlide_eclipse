package org.erlide.core.services.search;

import com.ericsson.otp.erlang.OtpErlangObject;

public class IncludePattern extends NamePattern {

    public IncludePattern(final String name, final LimitTo limitTo) {
        super(name, limitTo);
    }

    @Override
    public OtpErlangObject getSearchObject() {
        return makeIncludePatternObject(getName());
    }

    @Override
    public SearchFor getSearchFor() {
        return SearchFor.INCLUDE;
    }

    @Override
    public String labelString() {
        String s = getName();
        if (!s.startsWith("-include")) {
            s = "-include(" + s;
        }
        if (!s.endsWith(").")) {
            s = s + ").";
        }
        return s;
    }

}
