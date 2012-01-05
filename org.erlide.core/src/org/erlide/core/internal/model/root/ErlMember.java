package org.erlide.core.internal.model.root;

import org.eclipse.core.runtime.Path;
import org.erlide.core.internal.model.erlang.SourceRange;
import org.erlide.core.internal.model.erlang.SourceRefElement;
import org.erlide.core.model.erlang.IErlMember;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.ISourceRange;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IParent;

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
        final IErlModule module = getModule();
        if (module != null) {
            return module.getName();
        }
        final String path = getFilePath();
        if (path != null) {
            return new Path(path).lastSegment();
        }
        return null;
    }

    @Override
    public IErlModule getModule() {
        final IErlElement ancestor = getAncestorOfKind(Kind.MODULE);
        if (ancestor instanceof IErlModule) {
            return (IErlModule) ancestor;
        }
        return null;
    }

}
