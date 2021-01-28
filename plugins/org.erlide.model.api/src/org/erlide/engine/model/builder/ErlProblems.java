package org.erlide.engine.model.builder;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;
import org.erlide.util.erlang.OtpErlang;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.ImmutableList;

@SuppressWarnings("all")
public class ErlProblems {
    private final List<ProblemData> data = CollectionLiterals
            .<ProblemData> newArrayList();

    private final Map<String, ProblemData> tagMap = CollectionLiterals
            .<String, ProblemData> newHashMap();

    private ErlProblems() {
        load();
    }

    public void load() {
        try {
            final ClassLoader loader = this.getClass().getClassLoader();
            try (InputStream input = loader
                    .getResourceAsStream("org/erlide/engine/model/builder/errors.data")) {
                final String src = Util.getInputStreamAsString(input,
                        StandardCharsets.ISO_8859_1.name());
                try {
                    final OtpErlangObject source0 = OtpErlang.parse(src);
                    final OtpErlangList source = (OtpErlangList) source0;
                    final OtpErlangObject[] _elements = source.elements();
                    for (final OtpErlangObject item0 : _elements) {
                        {
                            final OtpErlangTuple item = (OtpErlangTuple) item0;
                            final OtpErlangObject _elementAt = item.elementAt(0);
                            final String tag = ((OtpErlangAtom) _elementAt).atomValue();
                            final OtpErlangObject _elementAt_1 = item.elementAt(1);
                            final String message = ((OtpErlangString) _elementAt_1)
                                    .stringValue().replaceAll("\\\\n", "\n");
                            final int myarity = ErlProblems.arity(message);
                            final ProblemData problemData = new ProblemData(tag, message,
                                    myarity);
                            data.add(problemData);
                            final boolean _containsKey = tagMap
                                    .containsKey(problemData.getTag());
                            if (_containsKey) {
                                final String _tag = problemData.getTag();
                                final String _plus = "duplicate problem tags are not allowed: \'"
                                        + _tag;
                                final String _plus_1 = _plus + "\'";
                                throw new IllegalStateException(_plus_1);
                            }
                            tagMap.put(problemData.getTag(), problemData);
                        }
                    }
                } catch (final Throwable _t) {
                    if (_t instanceof Exception) {
                        final Exception e = (Exception) _t;
                        ErlLogger.debug(e);
                        throw e;
                    } else {
                        throw Exceptions.sneakyThrow(_t);
                    }
                }
            }
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    public List<ProblemData> getData() {
        return ImmutableList.<ProblemData> copyOf(data);
    }

    public static int arity(final String string) {
        int result = 0;
        boolean escape = false;
        final byte[] _bytes = string.getBytes();
        for (final byte c : _bytes) {
            {
                if (!escape && c == 126) {
                    result = result + 1;
                }
                escape = c == 92;
            }
        }
        return result;
    }

    public void check() {
        final List<String> names = CollectionLiterals.<String> newArrayList();
        for (final ProblemData p : data) {
            {
                final String _tag = p.getTag();
                final String _plus = _tag + "/";
                final int _arity = p.getArity();
                final String _plus_1 = _plus + Integer.valueOf(_arity);
                final boolean _contains = names.contains(_plus_1);
                if (_contains) {
                    final String _tag_1 = p.getTag();
                    final String _plus_2 = "DOUBLE " + _tag_1;
                    final String _plus_3 = _plus_2 + "/";
                    final int _arity_1 = p.getArity();
                    final String _plus_4 = _plus_3 + Integer.valueOf(_arity_1);
                    System.out.println(_plus_4);
                }
                final String _tag_2 = p.getTag();
                final String _plus_5 = _tag_2 + "/";
                final int _arity_2 = p.getArity();
                final String _plus_6 = _plus_5 + Integer.valueOf(_arity_2);
                names.add(_plus_6);
            }
        }
    }

    private static ErlProblems instance = null;

    public static ErlProblems getInstance() {
        if (ErlProblems.instance == null) {
            final ErlProblems _erlProblems = new ErlProblems();
            ErlProblems.instance = _erlProblems;
        }
        return ErlProblems.instance;
    }

    public static ProblemData parse(final String msg) {
        for (final ProblemData p : ErlProblems.getInstance().data) {
            final boolean _matches = p.getPattern().matcher(msg).matches();
            if (_matches) {
                return p;
            }
        }
        return null;
    }

    public ProblemData getProblem(final String message) {
        for (final ProblemData d : data) {
            {
                final List<String> args = d.getMessageArgs(message);
                if (args != null) {
                    return d;
                }
            }
        }
        return null;
    }
}
