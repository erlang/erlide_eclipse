package org.erlide.core.services.text;

import org.erlide.utils.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class IndentResult {
    private int removeNext;
    private boolean addNewLine;
    private String text;

    public IndentResult(final String text, final int removeNext,
            final boolean addNewLine) {
        super();
        this.text = text;
        this.removeNext = removeNext;
        this.addNewLine = addNewLine;
    }

    public IndentResult(final OtpErlangObject o) {
        if (o instanceof OtpErlangTuple && !Util.isError(o)) {
            final OtpErlangTuple t = (OtpErlangTuple) o;
            final OtpErlangLong l1 = (OtpErlangLong) t.elementAt(1);
            final OtpErlangAtom b = (OtpErlangAtom) t.elementAt(2);
            text = Util.stringValue(t.elementAt(0));
            try {
                removeNext = l1.intValue();
            } catch (final OtpErlangRangeException e) {
                removeNext = 0;
            }
            addNewLine = b.booleanValue();
        } else {
            addNewLine = false;
            text = "";
            removeNext = 0;
        }

    }

    /**
     * @return the removeNext
     */
    public int getRemoveNext() {
        return removeNext;
    }

    /**
     * @return the addNewLine
     */
    public boolean isAddNewLine() {
        return addNewLine;
    }

    /**
     * @return the text
     */
    public String getText() {
        return text;
    }

}
