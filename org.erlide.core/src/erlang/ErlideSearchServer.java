package erlang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.erlide.core.search.ErlangSearchPattern;
import org.erlide.core.search.ModuleLineFunctionArityRef;
import org.erlide.core.text.ErlangToolkit;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideSearchServer {

	private static final int SEARCH_LONG_TIMEOUT = 50000;

	private static OtpErlangList getModulesFromScope(
			final Collection<IResource> scope) {
		OtpErlangObject result[] = new OtpErlangObject[scope.size()];
		int i = 0;
		for (IResource r : scope) {
			result[i] = new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom(ErlangToolkit
							.createScannerModuleNameFromResource(r)),
					new OtpErlangString(r.getLocation().toPortableString()) });
			i++;
		}
		return new OtpErlangList(result);
	}

	public static List<ModuleLineFunctionArityRef> findRefs(final Backend b,
			final ErlangSearchPattern ref, final Collection<IResource> scope,
			final String stateDir) {
		final List<ModuleLineFunctionArityRef> result = new ArrayList<ModuleLineFunctionArityRef>();
		try {
			final OtpErlangObject r = b.call(SEARCH_LONG_TIMEOUT,
					"erlide_search_server", "find_refs", "xxs", ref
							.getSearchObject(), getModulesFromScope(scope),
					stateDir);
			if (Util.isOk(r)) {
				addSearchResult(result, r);
			}
		} catch (final Exception e) {
			ErlLogger.error(e); // TODO report error
		}
		return result;
	}

	private static void addSearchResult(
			final List<ModuleLineFunctionArityRef> result,
			final OtpErlangObject r) throws OtpErlangRangeException {
		final OtpErlangTuple t = (OtpErlangTuple) r;
		final OtpErlangList l = (OtpErlangList) t.elementAt(1);
		for (final OtpErlangObject i : l) {
			/*
			 * find_data([#ref{function=F, arity=A, clause=C, data=D, offset=O,
			 * length=L, sub_clause=S} | Rest], Data, M, Acc) -> case D of Data
			 * -> find_data(Rest, Data, M, [{M, F, A, C, S, O, L} | Acc]); _ ->
			 * find_data(Rest, Data, M, Acc) end.
			 */
			final OtpErlangTuple modLineT = (OtpErlangTuple) i;
			String modName = Util.stringValue(modLineT.elementAt(0));
			OtpErlangObject nameO = modLineT.elementAt(1);
			final OtpErlangLong arityL = (OtpErlangLong) modLineT.elementAt(2);
			int arity = arityL.intValue();
			String clauseHead = Util.stringValue(modLineT.elementAt(3));
			OtpErlangAtom subClause = (OtpErlangAtom) modLineT.elementAt(4);
			OtpErlangLong offsetL = (OtpErlangLong) modLineT.elementAt(5);
			OtpErlangLong lengthL = (OtpErlangLong) modLineT.elementAt(6);
			String name;
			if (nameO instanceof OtpErlangAtom) {
				OtpErlangAtom nameA = (OtpErlangAtom) nameO;
				name = nameA.atomValue();
			} else {
				name = Util.stringValue(nameO);
			}
			result.add(new ModuleLineFunctionArityRef(modName, offsetL
					.intValue(), lengthL.intValue(), name, arity, clauseHead,
					"true".equals(subClause.atomValue())));
		}
	}

	// public static List<ModuleLineFunctionArityRef> functionUse(
	// final ErlideBackend backend,
	// final ErlangExternalFunctionCallRef searchRef,
	// final List<IResource> scope) {
	// return functionUse(backend, searchRef.getModule(), searchRef
	// .getFunction(), searchRef.getArity(), scope);
	// }

}
