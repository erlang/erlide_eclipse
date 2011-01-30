package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlMember;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.SourceRange;
import org.erlide.jinterface.util.ErlLogger;

/**
 * 
 * @author Vlad Dumitrescu
 */
public abstract class ErlMember extends SourceRefElement implements IErlMember {

    int fNameRangeOffset, fNameRangeLength;

    public static String uptoCommaOrParen(final String s) {
        if (s == null || s.length() == 0) {
            return s;
        }
        int i = 0;
        if (s.charAt(0) == '\'') {
            i = s.indexOf('\'', 1);
        }
        if (i == -1) {
            i = 0;
        }
        int j = s.indexOf(',', i);
        if (j == 0 || j == -1) {
            j = s.length();
        }
        final int k = s.indexOf('(', i);
        if (k < j && k > 0) {
            j = k;
        }
        return s.substring(0, j);
    }

    protected ErlMember(final IParent parent, final String name) {
        super(parent, name);
    }

    public boolean isVisibleInOutline() {
        return true;
    }

    public void setNameRange(final int offset, final int length) {
        fNameRangeOffset = offset;
        fNameRangeLength = length;
    }

    public ISourceRange getNameRange() {
        if (fNameRangeOffset == 0 && fNameRangeLength == 0) {
            try {
                return getSourceRange();
            } catch (final ErlModelException e) {
                ErlLogger.error(e); // will never happen
            }
        }
        return new SourceRange(fNameRangeOffset, fNameRangeLength);
    }

}
