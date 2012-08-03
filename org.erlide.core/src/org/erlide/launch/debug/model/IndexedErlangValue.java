package org.erlide.launch.debug.model;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CodingErrorAction;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IIndexedValue;
import org.eclipse.debug.core.model.IVariable;
import org.erlide.backend.BackendException;
import org.erlide.core.model.erlang.IErlPreprocessorDef;
import org.erlide.core.model.erlang.IErlRecordDef;
import org.erlide.core.model.erlang.IErlRecordField;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.model.util.ModelUtils;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Charsets;
import com.google.common.collect.Lists;

public class IndexedErlangValue extends ErlangValue implements IIndexedValue {
    private static final List<IErlElement> EMPTY_LIST = Lists.newArrayList();

    // FIXME JC Maybe we should use polymorphism for records?
    protected IErlRecordDef record; // set if this value is a record
    protected OtpErlangList list; // set if this value is a string-coded list

    public IndexedErlangValue(final IDebugTarget target, final String varName,
            final OtpErlangObject value, final ErlangProcess process,
            final String moduleName) {
        super(target, varName, value, process, moduleName);
        record = checkRecord(value);
        list = checkList(value);
    }

    private OtpErlangList checkList(final OtpErlangObject theValue) {
        if (theValue instanceof OtpErlangString) {
            final OtpErlangString os = (OtpErlangString) theValue;
            final String s = os.stringValue();
            final byte[] b = s.getBytes(Charsets.ISO_8859_1);
            if (!looksLikeAscii(b)) {
                return new OtpErlangList(s);
            }
        }
        return null;
    }

    @Override
    public int getInitialOffset() {
        return 0;
    }

    @Override
    public int getSize() throws DebugException {
        int arity = getArity();
        if (record != null) {
            --arity;
        }
        return arity;
    }

    @Override
    public IVariable getVariable(final int offset) throws DebugException {
        String name;
        if (record != null) {
            try {
                final IErlRecordField recordField = (IErlRecordField) record
                        .getChildren().get(offset);
                name = recordField.getFieldName();
            } catch (final ErlModelException e) {
                name = varName + ":" + offset;
            }
        } else {
            name = varName + ":" + offset;
        }
        return new ErlangVariable(getDebugTarget(), name, true,
                getElementAt(offset), process, moduleName, -1);
    }

    @Override
    public IVariable[] getVariables(final int offset, final int length)
            throws DebugException {
        final IVariable[] result = new IVariable[length];
        for (int i = 0; i < length; ++i) {
            result[i] = getVariable(i + offset);
        }
        return result;
    }

    private IErlRecordDef checkRecord(final OtpErlangObject o) {
        if (o instanceof OtpErlangTuple) {
            final OtpErlangTuple t = (OtpErlangTuple) o;
            final OtpErlangObject h = t.elementAt(0);
            if (h instanceof OtpErlangAtom) {
                final OtpErlangAtom a = (OtpErlangAtom) h;
                final ErlangDebugTarget target = getErlangDebugTarget();
                IErlPreprocessorDef pd;
                try {
                    pd = ModelUtils.findPreprocessorDef(
                            getErlProjects(target.getProjects()), moduleName,
                            a.atomValue(), IErlElement.Kind.RECORD_DEF);
                    if (pd instanceof IErlRecordDef) {
                        final IErlRecordDef r = (IErlRecordDef) pd;
                        if (r.hasChildren()
                                && r.getChildCount() + 1 == t.arity()) {
                            return r;
                        }
                    }
                } catch (final CoreException e) {
                } catch (final BackendException e) {
                }
            }
        }
        return null;
    }

    private Collection<IErlProject> getErlProjects(
            final Collection<IProject> projects) {
        final List<IErlProject> result = Lists
                .newArrayListWithCapacity(projects.size());
        final IErlModel model = ErlModelManager.getErlangModel();
        for (final IProject project : projects) {
            final IErlElement element = model.getChildWithResource(project);
            if (element instanceof IErlProject) {
                final IErlProject erlProject = (IErlProject) element;
                result.add(erlProject);
            }
        }
        return result;
    }

    @Override
    public String getReferenceTypeName() throws DebugException {
        if (record != null) {
            return "record";
        } else {
            return super.getReferenceTypeName();
        }
    }

    @Override
    public String getValueString() throws DebugException {
        if (record != null) {
            return getRecordValueString(record, value);
        } else if (list != null) {
            return getListValueString(list);
        } else {
            return getValueString(value, false);
        }
    }

    private String getValueString(final OtpErlangObject o,
            final boolean recordCheck) throws DebugException {
        if (o instanceof OtpErlangBinary) {
            final OtpErlangBinary b = (OtpErlangBinary) o;
            return getBinaryValueString(b);
        } else if (o instanceof OtpErlangTuple) {
            if (recordCheck) {
                final IErlRecordDef r = checkRecord(o);
                if (r != null) {
                    return getRecordValueString(r, o);
                }
            }
            final OtpErlangTuple t = (OtpErlangTuple) o;
            return getTupleValueString(t);
        } else if (o instanceof OtpErlangList) {
            final OtpErlangList l = (OtpErlangList) o;
            return getListValueString(l);
        } else {
            return o.toString();
        }
    }

    private String getRecordValueString(final IErlRecordDef r,
            final OtpErlangObject o) {
        final StringBuilder b = new StringBuilder();
        List<IErlElement> children;
        try {
            children = r.getChildren();
        } catch (final ErlModelException e) {
            children = EMPTY_LIST;
        }
        final OtpErlangTuple t = (OtpErlangTuple) o;
        b.append(t.elementAt(0)).append("#{");
        final int n = children.size();
        if (n > 0) {
            for (int i = 0; i < n; i++) {
                final IErlRecordField field = (IErlRecordField) children.get(i);
                b.append(field.getFieldName()).append('=')
                        .append(t.elementAt(i + 1).toString()).append(", ");
            }
            b.setLength(b.length() - 2);
        }
        b.append('}');
        return b.toString();
    }

    private String getListValueString(final OtpErlangList l)
            throws DebugException {
        final StringBuilder b = new StringBuilder("[");
        if (l.arity() > 0) {
            for (final OtpErlangObject o : l) {
                b.append(getValueString(o, true)).append(", ");
            }
            b.setLength(b.length() - 2);
        }
        b.append(']');
        return b.toString();
    }

    private String getTupleValueString(final OtpErlangTuple t)
            throws DebugException {
        final StringBuilder b = new StringBuilder("{");
        if (t.arity() > 0) {
            for (final OtpErlangObject o : t.elements()) {
                b.append(getValueString(o, true)).append(", ");
            }
            b.setLength(b.length() - 2);
        }
        b.append('}');
        return b.toString();
    }

    private static String getBinaryValueString(final OtpErlangBinary b) {
        final StringBuilder sb = new StringBuilder("<<");
        if (b.size() > 0) {
            final byte[] bytes = b.binaryValue();
            CharBuffer cb = null;
            if (looksLikeAscii(bytes)) {
                final Charset[] css = { Charsets.UTF_8, Charsets.ISO_8859_1 };
                final Charset[] tryCharsets = css;
                for (final Charset cset : tryCharsets) {
                    final CharsetDecoder cd = cset.newDecoder();
                    cd.onMalformedInput(CodingErrorAction.REPORT);
                    cd.onUnmappableCharacter(CodingErrorAction.REPORT);
                    try {
                        cb = cd.decode(ByteBuffer.wrap(bytes));
                        break;
                    } catch (final CharacterCodingException e) {
                    }
                }
            }
            if (cb != null && cb.length() > 0) {
                sb.append('"').append(cb).append('"');
            } else {
                for (int i = 0, n = bytes.length; i < n; ++i) {
                    int j = bytes[i];
                    if (j < 0) {
                        j += 256;
                    }
                    sb.append(j);
                    if (i < n - 1) {
                        sb.append(',');
                    }
                }
            }
        }
        sb.append(">>");
        return sb.toString();
    }

    private static boolean looksLikeAscii(final byte[] bytes) {
        for (final byte b : bytes) {
            if (b < 32) {
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean hasVariables() throws DebugException {
        return getArity() != -1;
    }

    protected OtpErlangObject getElementAt(final int index) {
        if (value instanceof OtpErlangTuple) {
            final OtpErlangTuple t = (OtpErlangTuple) value;
            final int ofs = record != null ? 1 : 0;
            return t.elementAt(index + ofs);
        } else if (value instanceof OtpErlangList) {
            final OtpErlangList l = (OtpErlangList) value;
            return l.elementAt(index);
        } else if (value instanceof OtpErlangBinary) {
            final OtpErlangBinary bs = (OtpErlangBinary) value;
            int j = bs.binaryValue()[index];
            if (j < 0) {
                j += 256;
            }
            return new OtpErlangLong(j);
        } else if (list != null) {
            return list.elementAt(index);
        }
        return null;
    }

    protected int getArity() {
        if (value instanceof OtpErlangTuple) {
            final OtpErlangTuple t = (OtpErlangTuple) value;
            return t.arity();
        } else if (value instanceof OtpErlangList) {
            final OtpErlangList l = (OtpErlangList) value;
            return l.arity();
        } else if (value instanceof OtpErlangBinary) {
            final OtpErlangBinary bs = (OtpErlangBinary) value;
            return bs.size();
        } else if (list != null) {
            return list.arity();
        } else {
            return -1;
        }
    }

    @Override
    public IVariable[] getVariables() throws DebugException {
        final int arity = getArity();
        if (arity != -1) {
            return getVariables(0, getSize());
        }
        return null;
    }

}
