package erlang;

import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class OpenResult {

    private boolean isExternalCall = false;
    private String name;
    private String fun;
    private int arity;
    private String path;
    // TODO rewrite this to use SearchFor
    private boolean isRecord = false;
    private boolean isMacro = false;
    private boolean isLocalCall = false;
    private boolean isInclude = false;
    private boolean isVariable = false;
    private boolean isDefine = false;
    private boolean isField = false;

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
                isExternalCall = true;
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
                isInclude = true;
                final OtpErlangString s = (OtpErlangString) openTuple
                        .elementAt(1);
                name = s.stringValue();
            } else if (kind.equals("local")) { // local call
                isLocalCall = true;
                final OtpErlangAtom element = (OtpErlangAtom) openTuple
                        .elementAt(1);
                fun = element.atomValue();
                arity = ((OtpErlangLong) openTuple.elementAt(2)).intValue();
                // } else if (external.equals("variable")) {
                // final OtpErlangTuple mf = (OtpErlangTuple) tres.elementAt(1);
                // final OtpErlangAtom var = (OtpErlangAtom) mf.elementAt(0);
            } else if (kind.startsWith("record") || kind.startsWith("macro")) {
                isMacro = kind.startsWith("macro");
                isRecord = kind.startsWith("record");
                isDefine = kind.endsWith("_def");
                final OtpErlangAtom element = (OtpErlangAtom) openTuple
                        .elementAt(1);
                name = element.toString();
                if (isMacro) {
                    name = removeQuestionMark(name);
                }
            } else if (kind.equals("variable")) {
                isVariable = true;
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
                isField = true;
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
        return isExternalCall;
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
        return isRecord;
    }

    public boolean isMacro() {
        return isMacro;
    }

    public boolean isLocalCall() {
        return isLocalCall;
    }

    public boolean isInclude() {
        return isInclude;
    }

    public boolean isVariable() {
        return isVariable;
    }

    public boolean isDefine() {
        return isDefine;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        final StringBuilder b = new StringBuilder("OpenResult {");
        if (isRecord) {
            b.append("record");
            if (isDefine) {
                b.append("_def");
            }
            b.append(' ').append(name);
        } else if (isMacro) {
            b.append("macro");
            if (isDefine) {
                b.append("_def");
            }
            b.append(' ').append(name);
        } else if (isInclude) {
            b.append("include \"").append(name).append('"');
        } else if (isRecord) {
            b.append("record ").append(name);
        } else if (isExternalCall) {
            b.append("external ");
            b.append(name).append(':').append(fun).append('/').append(arity);
        } else if (isLocalCall) {
            b.append("local ").append(fun).append('/').append(arity);
        } else if (isVariable) {
            b.append("variable ").append(name);
        } else if (isField) {
            b.append("record_field ").append(name).append('.').append(fun);
        }
        if (path != null && path.length() > 0) {
            b.append(" \"").append(path).append('"');
        }
        b.append('}');
        return b.toString();
    }

    public boolean isField() {
        return isField;
    }
}
