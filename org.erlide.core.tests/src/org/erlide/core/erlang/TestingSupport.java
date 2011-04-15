package org.erlide.core.erlang;

import org.erlide.core.model.erlang.internal.ErlAttribute;
import org.erlide.core.model.root.api.IErlElement;
import org.erlide.core.model.root.api.IParent;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * Only support for testing, mostly creating model stuff to compare with parsing
 * etc
 * 
 * @author jakob
 */
public final class TestingSupport {
    public static IErlElement createErlAttribute(final IParent parent,
            final String name, final OtpErlangObject value, final String extra,
            final int sourceRangeOffset, final int sourceRangeLength) {
        final ErlAttribute attribute = new ErlAttribute(parent, name, value,
                extra);
        attribute.setSourceRangeOffset(sourceRangeOffset);
        attribute.setSourceRangeLength(sourceRangeLength);
        return attribute;
    }
}
