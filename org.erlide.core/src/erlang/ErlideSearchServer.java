package erlang;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.erlide.core.erlang.util.ErlangFunction;
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

	private static OtpErlangList getModulesFromScope(final List<IResource> scope) {
		OtpErlangObject result[] = new OtpErlangObject[scope.size()];
		for (int i = 0, n = result.length; i < n; ++i) {
			IResource r = scope.get(i);
			result[i] = new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom(ErlangToolkit
							.createScannerModuleNameFromResource(r)),
					new OtpErlangString(r.getLocation().toPortableString()) });
		}
		return new OtpErlangList(result);
	}

	public static List<ModuleLineFunctionArityRef> functionUse(final Backend b,
			final FunctionRef ref, final List<IResource> scope,
			final String stateDir) {
		return functionUse(b, ref.module, ref.function, ref.arity, scope,
				stateDir);
	}

	public static List<ModuleLineFunctionArityRef> macroOrRecordUse(
			final Backend b, final String macroOrRecord, final String name,
			final List<IResource> scope, final String stateDir) {
		final List<ModuleLineFunctionArityRef> result = new ArrayList<ModuleLineFunctionArityRef>();
		try {
			final OtpErlangObject r = b.call("erlide_search_server",
					"find_refs", "aaxs", macroOrRecord, name,
					getModulesFromScope(scope), stateDir);
			if (Util.isOk(r)) {
				addSearchResult(result, r);
			}
		} catch (final Exception e) {
			ErlLogger.error(e); // TODO report error
		}
		return result;
	}

	public static List<ModuleLineFunctionArityRef> includeUse(final Backend b,
			final String name, final List<IResource> scope,
			final String stateDir) {
		final List<ModuleLineFunctionArityRef> result = new ArrayList<ModuleLineFunctionArityRef>();
		try {
			final OtpErlangObject r = b.call("erlide_search_server",
					"find_refs", "asxs", "include", name,
					getModulesFromScope(scope), stateDir);
			if (Util.isOk(r)) {
				addSearchResult(result, r);
			}
		} catch (final Exception e) {
			ErlLogger.error(e); // TODO report error
		}
		return result;
	}

	public static List<ModuleLineFunctionArityRef> functionUse(final Backend b,
			final String mod, final String fun, final int arity,
			final List<IResource> scope, final String stateDir) {
		final List<ModuleLineFunctionArityRef> result = new ArrayList<ModuleLineFunctionArityRef>();
		try {
			final OtpErlangObject r = b.call("erlide_search_server",
					"find_refs", "aaixs", mod, fun, arity,
					getModulesFromScope(scope), stateDir);
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
			OtpErlangObject functionNameOrAttr = modLineT.elementAt(1);
			final OtpErlangLong arityL = (OtpErlangLong) modLineT.elementAt(2);
			int arity = arityL.intValue();
			String clauseHead = Util.stringValue(modLineT.elementAt(3));
			OtpErlangAtom subClause = (OtpErlangAtom) modLineT.elementAt(4);
			OtpErlangLong offsetL = (OtpErlangLong) modLineT.elementAt(5);
			OtpErlangLong lengthL = (OtpErlangLong) modLineT.elementAt(6);
			ErlangFunction function;
			String attribute;
			if (functionNameOrAttr instanceof OtpErlangAtom) {
				OtpErlangAtom functionA = (OtpErlangAtom) functionNameOrAttr;
				function = new ErlangFunction(functionA.atomValue(), arity);
				attribute = null;
			} else {
				function = null;
				attribute = Util.stringValue(functionNameOrAttr);
			}
			result.add(new ModuleLineFunctionArityRef(modName, offsetL
					.intValue(), lengthL.intValue(), function, clauseHead,
					"true".equals(subClause.atomValue()), attribute));
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
