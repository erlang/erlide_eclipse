package org.erlide.engine.internal.model.erlang;

import org.eclipse.core.runtime.Path;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.IErlMember;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.ISourceRange;

/**
 *
 * @author Vlad Dumitrescu
 */
public abstract class ErlMember extends SourceRefElement implements IErlMember {

    int fNameRangeOffset, fNameRangeLength;

    protected ErlMember(final IParent parent, final String name) {
        super(parent, name);
    }

    @Override
    public void setNameRange(final int offset, final int length) {
        fNameRangeOffset = offset;
        fNameRangeLength = length;
    }

    @Override
    public ISourceRange getNameRange() {
        if (fNameRangeOffset == 0 && fNameRangeLength == 0) {
            return getSourceRange();
        }
        return new SourceRange(fNameRangeOffset, fNameRangeLength);
    }

    @Override
    public String getModuleName() {
        final IErlModule module = ErlangEngine.getInstance().getModelUtilService()
                .getModule(this);
        if (module != null) {
            return module.getName();
        }
        final String path = getFilePath();
        if (path != null) {
            return new Path(path).lastSegment();
        }
        return null;
    }

    protected static String uptoEndOfToken(final String s) {
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
        return s.substring(0, j).trim();
    }

}
