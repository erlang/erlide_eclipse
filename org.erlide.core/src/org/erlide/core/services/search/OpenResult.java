package org.erlide.core.services.search;

import org.erlide.core.model.util.ErlangFunction;
import org.erlide.jinterface.ErlLogger;
import org.erlide.utils.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class OpenResult {

    private boolean externalCall = false;
    private String name;
    private String fun;
    private int arity;
    private String path;
    // TODO rewrite this to use SearchFor
    private boolean record = false;
    private boolean macro = false;
    private boolean localCall = false;
    private boolean include = false;
    private boolean variable = false;
    private boolean define = false;
    private boolean field = false;

    public OpenResult(final OtpErlangObject res) {
        if (!(res instanceof OtpErlangTuple)) {
            return; // not a call, ignore
        }
        final OtpErlangTuple openTuple = (OtpErlangTuple) res;
        final String kind = ((OtpErlangAtom) openTuple.elementAt(0))
                .atomValue();
        try {
            if (kind.equals("external")) {
                final OtpErlangAtom element = (OtpErlangAtom) openTuple
                        .elementAt(1);
                externalCall = true;
                name = element.atomValue();
                fun = ((OtpErlangAtom) openTuple.elementAt(2)).atomValue();
                arity = ((OtpErlangLong) openTuple.elementAt(3)).intValue();
                path = null;
                if (openTuple.arity() > 4
                        && openTuple.elementAt(4) instanceof OtpErlangString) {
                    path = ((OtpErlangString) openTuple.elementAt(4))
                            .stringValue();
                }
            } else if (kind.equals("include")) {
                include = true;
                name = Util.stringValue(openTuple.elementAt(1));
            } else if (kind.equals("include_lib")) {
                include = true;
                name = Util.stringValue(openTuple.elementAt(1));
                path = Util.stringValue(openTuple.elementAt(2));
            } else if (kind.equals("local")) { // local call
                localCall = true;
                final OtpErlangAtom element = (OtpErlangAtom) openTuple
                        .elementAt(1);
                fun = element.atomValue();
                arity = ((OtpErlangLong) openTuple.elementAt(2)).intValue();
                // } else if (external.equals("variable")) {
                // final OtpErlangTuple mf = (OtpErlangTuple) tres.elementAt(1);
                // final OtpErlangAtom var = (OtpErlangAtom) mf.elementAt(0);
            } else if (kind.startsWith("record") || kind.startsWith("macro")) {
                macro = kind.startsWith("macro");
                record = kind.startsWith("record");
                define = kind.endsWith("_def");
                final OtpErlangAtom element = (OtpErlangAtom) openTuple
                        .elementAt(1);
                name = element.toString();
                if (macro) {
                    name = removeQuestionMark(name);
                }
            } else if (kind.equals("variable")) {
                variable = true;
                final OtpErlangObject o = openTuple.elementAt(1);
                if (o instanceof OtpErlangTuple) {
                    final OtpErlangTuple t = (OtpErlangTuple) o;
                    final OtpErlangAtom a = (OtpErlangAtom) t.elementAt(0);
                    name = a.atomValue();
                } else if (o instanceof OtpErlangAtom) {
                    final OtpErlangAtom a = (OtpErlangAtom) o;
                    name = a.atomValue();
                }
            } else if (kind.equals("field")) {
                field = true;
                final OtpErlangAtom recordNameA = (OtpErlangAtom) openTuple
                        .elementAt(1);
                fun = recordNameA.atomValue();
                final OtpErlangAtom fieldNameA = (OtpErlangAtom) openTuple
                        .elementAt(2);
                name = fieldNameA.atomValue();
            }
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
    }

    public static String removeQuestionMark(final String name) {
        final int i = name.indexOf('?');
        if (i == 0 || i == 1) {
            return name.substring(0, i) + name.substring(i + 1);
        }
        return name;
    }

    public boolean isExternalCall() {
        return externalCall;
    }

    public String getName() {
        return name;
    }

    public String getFun() {
        return fun;
    }

    public int getArity() {
        return arity;
    }

    public ErlangFunction getFunction() {
        return new ErlangFunction(fun, arity);
    }

    public String getPath() {
        return path;
    }

    public boolean isRecord() {
        return record;
    }

    public boolean isMacro() {
        return macro;
    }

    public boolean isLocalCall() {
        return localCall;
    }

    public boolean isInclude() {
        return include;
    }

    public boolean isVariable() {
        return variable;
    }

    public boolean isDefine() {
        return define;
    }

    @Override
    public String toString() {
        final StringBuilder b = new StringBuilder("OpenResult {");
        if (record) {
            b.append("record");
            if (define) {
                b.append("_def");
            }
            b.append(' ').append(name);
        } else if (macro) {
            b.append("macro");
            if (define) {
                b.append("_def");
            }
            b.append(' ').append(name);
        } else if (include) {
            b.append("include \"").append(name).append('"');
        } else if (record) {
            b.append("record ").append(name);
        } else if (externalCall) {
            b.append("external ");
            b.append(name).append(':').append(fun).append('/').append(arity);
        } else if (localCall) {
            b.append("local ").append(fun).append('/').append(arity);
        } else if (variable) {
            b.append("variable ").append(name);
        } else if (field) {
            b.append("record_field ").append(name).append('.').append(fun);
        }
        if (path != null && path.length() > 0) {
            b.append(" \"").append(path).append('"');
        }
        b.append('}');
        return b.toString();
    }

    public boolean isField() {
        return field;
    }
}
