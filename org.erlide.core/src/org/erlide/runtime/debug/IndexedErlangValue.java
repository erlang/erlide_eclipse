package org.erlide.runtime.debug;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CodingErrorAction;
import java.util.List;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IIndexedValue;
import org.eclipse.debug.core.model.IVariable;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlRecordDef;
import org.erlide.core.erlang.util.ModelUtils;
import org.erlide.runtime.backend.Backend;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class IndexedErlangValue extends ErlangValue implements IIndexedValue {
	// FIXME JC Maybe we should use polymorphism for records?
	protected IErlRecordDef record; // set if this value is a record

	public IndexedErlangValue(final IDebugTarget target, final String varName,
			final OtpErlangObject value, final ErlangProcess process,
			final String moduleName) {
		super(target, varName, value, process, moduleName);
		record = checkRecord(value);
	}

	public int getInitialOffset() {
		return 0;
	}

	public int getSize() throws DebugException {
		int arity = getArity();
		if (record != null) {
			--arity;
		}
		return arity;
	}

	public IVariable getVariable(final int offset) throws DebugException {
		final String name = record != null ? record.getFields().get(offset)
				: varName + ":" + offset;
		return new ErlangVariable(getDebugTarget(), name, true,
				getElementAt(offset), process, moduleName, -1);
	}

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
				final ErlangDebugTarget target = (ErlangDebugTarget) getDebugTarget();
				final Backend b = target.getBackend();
				final IErlPreprocessorDef pd = ModelUtils.findPreprocessorDef(
						b, target.getProjects(), moduleName, a.atomValue(),
						IErlElement.Kind.RECORD_DEF, "");
				if (pd != null) {
					final IErlRecordDef r = (IErlRecordDef) pd;
					final List<String> fields = r.getFields();
					if (fields != null && fields.size() + 1 == t.arity()) {
						return r;
					}
				}
			}
		}
		return null;
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
		} else {
			return getValueString(value, false);
		}
	}

	private String getValueString(final OtpErlangObject o,
			final boolean recordCheck) {
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
		final List<String> fields = r.getFields();
		final OtpErlangTuple t = (OtpErlangTuple) o;
		b.append(t.elementAt(0)).append("#{");
		final int n = fields.size();
		if (n > 0) {
			for (int i = 0; i < n; i++) {
				b.append(fields.get(i)).append("=").append(
						t.elementAt(i + 1).toString()).append(", ");
			}
			b.setLength(b.length() - 2);
		}
		b.append("}");
		return b.toString();
	}

	private String getListValueString(final OtpErlangList l) {
		final StringBuilder b = new StringBuilder("[");
		if (l.arity() > 0) {
			for (final OtpErlangObject o : l) {
				b.append(getValueString(o, true)).append(", ");
			}
			b.setLength(b.length() - 2);
		}
		b.append("]");
		return b.toString();
	}

	private String getTupleValueString(final OtpErlangTuple t) {
		final StringBuilder b = new StringBuilder("{");
		if (t.arity() > 0) {
			for (final OtpErlangObject o : t.elements()) {
				b.append(getValueString(o, true)).append(", ");
			}
			b.setLength(b.length() - 2);
		}
		b.append("}");
		return b.toString();
	}

	private static String getBinaryValueString(final OtpErlangBinary b) {
		final StringBuilder sb = new StringBuilder("<<");
		if (b.size() > 0) {
			final byte[] bytes = b.binaryValue();
			// FIXME: why are the character decoders so forgiving? I'd like to
			// test
			// for UTF-16 too, but the decoders never throws on anything, and
			// looksLikeISOLatin shouldn't be needed...
			final String[] css1 = { "UTF-8", "ISO-8859-1" };
			final String[] css2 = { "UTF-8" };
			final String[] tryCharsets = looksLikeISOLatin(bytes) ? css1 : css2;
			CharBuffer cb = null;
			for (final String csName : tryCharsets) {
				final CharsetDecoder cd = Charset.forName(csName).newDecoder();
				cd.onMalformedInput(CodingErrorAction.REPORT);
				cd.onUnmappableCharacter(CodingErrorAction.REPORT);
				try {
					cb = cd.decode(ByteBuffer.wrap(bytes));
					break;
				} catch (final CharacterCodingException e) {
					cb = null;
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
						sb.append(",");
					}
				}
			}
		}
		sb.append(">>");
		return sb.toString();
	}

	private static boolean looksLikeISOLatin(final byte[] bytes) {
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
