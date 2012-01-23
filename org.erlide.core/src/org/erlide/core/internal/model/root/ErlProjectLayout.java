package org.erlide.core.internal.model.root;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.jinterface.Bindings;
import org.erlide.utils.ErlUtils;
import org.erlide.utils.TermParserException;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

/*
 * Note: in paths, first segment in all caps is taken as a path variable when 
 * converting to Erlang term
 */

public class ErlProjectLayout {

    private final List<IPath> sources;
    private final List<IPath> includes;
    private final IPath output;
    private final List<IPath> docs;
    private final IPath priv;

    public final static ErlProjectLayout OTP_LAYOUT = new ErlProjectLayout(
            Lists.newArrayList((IPath) new Path("src")),
            Lists.newArrayList((IPath) new Path("include")), new Path("ebin"),
            Lists.newArrayList((IPath) new Path("doc")), new Path("priv"));

    public ErlProjectLayout(final List<IPath> sources,
            final List<IPath> includes, final IPath output,
            final List<IPath> docs, final IPath priv) {
        this.sources = sources;
        this.includes = includes;
        this.output = output;
        this.docs = docs;
        this.priv = priv;
    }

    public ErlProjectLayout(final OtpErlangObject layout)
            throws TermParserException, OtpErlangException {
        final Bindings b = ErlUtils.match("{layout,S,I,E,D,P}", layout);
        final Collection<OtpErlangObject> s = b.getList("S");
        final Collection<OtpErlangObject> i = b.getList("I");
        final OtpErlangObject e = b.get("E");
        final Collection<OtpErlangObject> d = b.getList("D");
        final OtpErlangObject p = b.get("P");

        sources = mkList(s);
        includes = mkList(i);
        output = mkPath(e);
        docs = mkList(d);
        priv = mkPath(p);
    }

    public Collection<IPath> getSources() {
        return Collections.unmodifiableCollection(sources);
    }

    public Collection<IPath> getIncludes() {
        return Collections.unmodifiableCollection(includes);
    }

    public IPath getOutput() {
        return output;
    }

    public Collection<IPath> getDocs() {
        return Collections.unmodifiableCollection(docs);
    }

    public IPath getPriv() {
        return priv;
    }

    public OtpErlangObject asTerm() {
        final OtpErlangObject s = listAsTerm(sources);
        final OtpErlangObject i = listAsTerm(includes);
        final OtpErlangObject o = pathAsTerm(output);
        final OtpErlangObject d = listAsTerm(docs);
        final OtpErlangObject p = pathAsTerm(priv);

        return OtpErlang.mkTuple(new OtpErlangAtom("layout"), s, i, o, d, p);
    }

    public ErlProjectLayout setSources(final Collection<IPath> list) {
        final List<IPath> list1 = Lists.newArrayList(list);
        final ErlProjectLayout result = new ErlProjectLayout(list1, includes,
                output, docs, priv);
        return result;
    }

    public ErlProjectLayout addSource(final String string) {
        return addSource(new Path(string));
    }

    public ErlProjectLayout addSource(final IPath path) {
        final List<IPath> sources1 = addToListIfNotExisting(sources, path);
        return setSources(sources1);
    }

    public ErlProjectLayout addSources(final Collection<IPath> paths) {
        final List<IPath> sources1 = Lists.newArrayList();
        for (final IPath p : paths) {
            sources1.add(p);
        }
        return setSources(sources1);
    }

    public ErlProjectLayout setIncludes(final Collection<IPath> list) {
        final List<IPath> list1 = Lists.newArrayList(list);
        final ErlProjectLayout result = new ErlProjectLayout(sources, list1,
                output, docs, priv);
        return result;
    }

    public ErlProjectLayout addInclude(final String string) {
        return addInclude(new Path(string));
    }

    public ErlProjectLayout addInclude(final IPath path) {
        final List<IPath> list = addToListIfNotExisting(includes, path);
        return setIncludes(list);
    }

    public ErlProjectLayout setOutput(final String string) {
        return setOutput(new Path(string));
    }

    public ErlProjectLayout setOutput(final Path path) {
        final ErlProjectLayout result = new ErlProjectLayout(sources, includes,
                path, docs, priv);
        return result;
    }

    public ErlProjectLayout setDocs(final Collection<IPath> list) {
        final List<IPath> list1 = Lists.newArrayList(list);
        final ErlProjectLayout result = new ErlProjectLayout(sources, includes,
                output, list1, priv);
        return result;
    }

    public ErlProjectLayout addDoc(final String string) {
        return addDoc(new Path(string));
    }

    public ErlProjectLayout addDoc(final IPath path) {
        final List<IPath> list = addToListIfNotExisting(docs, path);
        final ErlProjectLayout result = new ErlProjectLayout(sources, includes,
                output, list, priv);
        return result;
    }

    private List<IPath> addToListIfNotExisting(final Collection<IPath> list,
            final IPath path) {
        final List<IPath> result = Lists.newArrayList(list);
        if (!result.contains(path)) {
            result.add(path);
        }
        return result;
    }

    public ErlProjectLayout setPriv(final String string) {
        return setPriv(new Path(string));
    }

    public ErlProjectLayout setPriv(final Path path) {
        final ErlProjectLayout result = new ErlProjectLayout(sources, includes,
                output, docs, path);
        return result;
    }

    private List<IPath> mkList(final Collection<OtpErlangObject> s)
            throws OtpErlangException {
        if (s == null) {
            return null;
        }
        final List<IPath> result = Lists.newArrayList();
        for (final OtpErlangObject o : s) {
            result.add(mkPath(o));
        }
        return result;
    }

    private IPath mkPath(final OtpErlangObject p) throws OtpErlangException {
        if (p instanceof OtpErlangString) {
            return new Path(((OtpErlangString) p).stringValue());
        } else if (p instanceof OtpErlangTuple) {
            try {
                final Bindings b = ErlUtils.match("{V,P}", p);
                final String v = b.getAtom("V");
                final String path = b.getString("P");
                return new Path(v).append(new Path(path));
            } catch (final TermParserException e) {
                return null;
            }
        }
        return null;
    }

    private OtpErlangObject listAsTerm(final List<IPath> list) {
        final List<OtpErlangObject> result = Lists.newArrayList();
        for (final IPath p : list) {
            result.add(pathAsTerm(p));
        }
        return OtpErlang.mkList(result);
    }

    private OtpErlangObject pathAsTerm(final IPath path) {
        final String first = path.segment(0);
        if (first.equals(first.toUpperCase())) {
            return OtpErlang
                    .mkTuple(new OtpErlangAtom(path.segment(0)),
                            new OtpErlangString(path.removeFirstSegments(1)
                                    .toString()));
        }
        return new OtpErlangString(path.toString());
    }

    public ErlProjectLayout removeSource(final String string) {
        return removeSource(new Path(string));
    }

    public ErlProjectLayout removeSource(final Path path) {
        final List<IPath> list = removeFromList(sources, path);
        final ErlProjectLayout result = new ErlProjectLayout(list, includes,
                output, docs, priv);
        return result;
    }

    public ErlProjectLayout removeInclude(final String string) {
        return removeInclude(new Path(string));
    }

    public ErlProjectLayout removeInclude(final Path path) {
        final List<IPath> list = removeFromList(includes, path);
        final ErlProjectLayout result = new ErlProjectLayout(sources, list,
                output, docs, priv);
        return result;
    }

    public ErlProjectLayout removeDoc(final String string) {
        return removeDoc(new Path(string));
    }

    public ErlProjectLayout removeDoc(final Path path) {
        final List<IPath> list = removeFromList(docs, path);
        final ErlProjectLayout result = new ErlProjectLayout(sources, includes,
                output, list, priv);
        return result;
    }

    private List<IPath> removeFromList(final List<IPath> list, final Path path) {
        final List<IPath> result = Lists.newArrayList(list);
        result.remove(path);
        return result;
    }

}
