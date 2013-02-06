package org.erlide.core.services.search;

import com.ericsson.otp.erlang.OtpErlangObject;

public class RecordPattern extends NamePattern {

    public RecordPattern(final String name, final LimitTo limitTo) {
        super(name, limitTo);
    }

    @Override
    public OtpErlangObject getSearchObject() {
        return makeSPatternObject(RECORD_DEF_ATOM, RECORD_REF_ATOM, getName());
    }

    @Override
    public String toString() {
        return "RecordPattern [limitTo=" + limitTo + ", getName()=" + getName()
                + "]";
    }

    @Override
    public SearchFor getSearchFor() {
        return SearchFor.RECORD;
    }

    @Override
    public String labelString() {
        String s = getName();
        if (!s.startsWith("#")) {
            s = "#" + s;
        }
        if (!s.endsWith("{}")) {
            s = s + "{}";
        }
        return s;
    }
}
