package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;

import org.erlide.engine.model.root.IErlElement;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangString;

public class IErlAttributeTests extends ErlModelTestBase {

    @Test
    public void getValue() throws Exception {
        module.open(null);
        final IErlElement element = module.getElementAtLine(0);
        final IErlAttribute attribute = (IErlAttribute) element;
        final IErlElement element2 = module.getElementAtLine(1);
        final IErlAttribute attribute2 = (IErlAttribute) element2;
        final OtpErlangAtom xx = new OtpErlangAtom("xx");
        final OtpErlangString yyHrl = new OtpErlangString("yy.hrl");
        assertEquals(xx, attribute.getValue());
        assertEquals(yyHrl, attribute2.getValue());
    }

}
