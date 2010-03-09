package erlang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.core.search.ErlangExternalFunctionCallRef;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.ErlideBackend;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideXref {
	public static OtpErlangObject getProposalsWithDoc(final Backend b,
			final String mod, final String prefix, final String stateDir) {
		OtpErlangObject res = null;
		try {
			res = b.call("erlide_otp_doc", "get_proposals", "ass", mod, prefix,
					stateDir);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return res;
	}

	public static void addDirs(final Backend backend,
			final Collection<String> dirs) {
		try {
			backend.call("erlide_xref", "add_dirs", "ls", dirs);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
	}

	public static List<ErlangExternalFunctionCallRef> funtionUse(
			final ErlideBackend backend, final ErlangExternalFunctionCallRef ref) {
		try {
			final OtpErlangObject res = backend.call("erlide_xref",
					"function_use", "aai", ref.getModule(), ref.getFunction(),
					ref.getArity());
			if (Util.isOk(res)) {
				final OtpErlangTuple t = (OtpErlangTuple) res;
				final OtpErlangList l = (OtpErlangList) t.elementAt(1);
				final List<ErlangExternalFunctionCallRef> result = new ArrayList<ErlangExternalFunctionCallRef>(
						l.arity());
				for (final OtpErlangObject i : l) {
					final OtpErlangTuple mfa = (OtpErlangTuple) i;
					final OtpErlangAtom modA = (OtpErlangAtom) mfa.elementAt(0);
					final OtpErlangAtom funA = (OtpErlangAtom) mfa.elementAt(1);
					final OtpErlangLong arL = (OtpErlangLong) mfa.elementAt(2);
					final IErlModule module = ErlangCore.getModel().findModule(
							modA.atomValue());
					if (module != null) {
						final int ar = arL.intValue();
						final String fun = funA.atomValue();
						module.open(null);
						final IErlFunction f = module
								.findFunction(new ErlangFunction(fun, ar));
						if (f != null) {
							final ErlangExternalFunctionCallRef d = new ErlangExternalFunctionCallRef(
									module.getName(), fun, ar);
							d.setElement(f);
							final ISourceRange r = f.getSourceRange();
							d.setPos(r);
							result.add(d);
						}
					}
				}
				return result;
			}
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		} catch (final OtpErlangRangeException e) {
			ErlLogger.warn(e);
		} catch (final ErlModelException e) {
			ErlLogger.warn(e);
		}
		return null;
	}

}
