package org.erlide.core.preferences;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.jinterface.util.Bindings;
import org.erlide.jinterface.util.ErlUtils;
import org.erlide.jinterface.util.ParserException;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public final class ErlProjectLayout {

    public final List<IPath> sources;
    public final List<IPath> includes;
    public final IPath output;
    public final List<IPath> docs;
    public final IPath priv;

    public final static ErlProjectLayout OTP_LAYOUT = new ErlProjectLayout();

    public ErlProjectLayout(List<IPath> sources, List<IPath> includes,
            IPath output, List<IPath> docs, IPath priv) {
        this.sources = sources;
        this.includes = includes;
        this.docs = docs;
        this.output = output;
        this.priv = priv;
    }

    private ErlProjectLayout() {
        this(Lists.newArrayList((IPath) new Path("src")), Lists
                .newArrayList((IPath) new Path("include")), new Path("ebin"),
                Lists.newArrayList((IPath) new Path("doc")), new Path("priv"));
    }

    public ErlProjectLayout(OtpErlangObject layout) throws ParserException,
            OtpErlangException {
        Bindings b = ErlUtils.match("{layout,S,I,E,D,P}", layout);
        Collection<OtpErlangObject> s = b.getList("S");
        Collection<OtpErlangObject> i = b.getList("I");
        OtpErlangObject e = b.get("E");
        Collection<OtpErlangObject> d = b.getList("D");
        OtpErlangObject p = b.get("P");

        sources = mkList(s);
        includes = mkList(i);
        output = mkPath(e);
        docs = mkList(d);
        priv = mkPath(p);
    }

    private List<IPath> mkList(Collection<OtpErlangObject> s)
            throws OtpErlangException {
        if (s == null) {
            return null;
        }
        List<IPath> result = Lists.newArrayList();
        for (OtpErlangObject o : s) {
            result.add(mkPath(o));
        }
        return result;
    }

    private IPath mkPath(OtpErlangObject p) throws OtpErlangException {
        if (p instanceof OtpErlangString) {
            return new Path(((OtpErlangString) p).stringValue());
        } else if (p instanceof OtpErlangTuple) {
            try {
                Bindings b = ErlUtils.match("{V,P}", p);
                String v = b.getAtom("V");
                String path = b.getString("P");
                return new Path(v).append(new Path(path));
            } catch (ParserException e) {
                return null;
            }
        }
        return null;
    }

    public OtpErlangObject asTerm() {
        OtpErlangObject s = listAsTerm(sources);
        OtpErlangObject i = listAsTerm(includes);
        OtpErlangObject o = pathAsTerm(output);
        OtpErlangObject d = listAsTerm(docs);
        OtpErlangObject p = pathAsTerm(priv);

        return OtpErlang.mkTuple(new OtpErlangAtom("layout"), s, i, o, d, p);
    }

    private OtpErlangObject listAsTerm(List<IPath> list) {
        List<OtpErlangObject> result = Lists.newArrayList();
        for (IPath p : list) {
            result.add(pathAsTerm(p));
        }
        return OtpErlang.mkList(result);
    }

    private OtpErlangObject pathAsTerm(IPath path) {
        char first = path.segment(0).charAt(0);
        if (first >= 'A' && first <= 'Z') {
            return OtpErlang
                    .mkTuple(new OtpErlangAtom(path.segment(0)),
                            new OtpErlangString(path.removeFirstSegments(1)
                                    .toString()));
        }
        return new OtpErlangString(path.toString());
    }
}
