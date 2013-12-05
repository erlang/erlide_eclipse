package org.erlide.engine.services.search;

import com.ericsson.otp.erlang.OtpErlangObject;

public class RecordFieldPattern extends ErlangSearchPattern {

    private final String record;
    private final String fieldName;

    public RecordFieldPattern(final String record, final String fieldName,
            final LimitTo limitTo) {
        super(limitTo);
        this.record = record;
        this.fieldName = fieldName;
    }

    public RecordFieldPattern(final String name, final LimitTo limitTo) {
        super(limitTo);
        final String[] parts = name.split("\\.");
        record = parts[0];
        fieldName = parts[1];
    }

    @Override
    public OtpErlangObject getSearchObject() {
        return makeSSPatternObject(RECORD_FIELD_DEF_ATOM, RECORD_FIELD_REF_ATOM, record,
                fieldName);
    }

    @Override
    public String patternString() {
        return record + "." + fieldName;
    }

    @Override
    public SearchFor getSearchFor() {
        return SearchFor.RECORD_FIELD;
    }

    @Override
    public String labelString() {
        return "#" + record + "." + fieldName;
    }

}
