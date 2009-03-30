package erlang;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.core.erlang.IErlProject;
import org.erlide.jinterface.Bindings;
import org.erlide.jinterface.ErlUtils;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.Backend;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlangXref {

	private ErlangXref() {
	}

	/*
	 * add_project(ProjectDir) -> xref:add_application(erlide, ProjectDir).
	 * 
	 * update() -> xref:update(erlide).
	 * 
	 * analyze(Module) when is_atom(Module) -> xref:m(Module); analyze(Dir) when
	 * is_list(Dir) -> xref:d(Dir).
	 * 
	 * module_use(Module) when is_atom(Module) -> xref:analyze(erlide,
	 * {module_use, Module}).
	 * 
	 * module_call(Module) when is_atom(Module) -> xref:analyze(erlide,
	 * {module_call, Module}).
	 * 
	 * function_use({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
	 * xref:analyze(erlide, {use, {M, F, A}}).
	 * 
	 * function_call({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
	 * xref:analyze(erlide, {call, {M, F, A}}).
	 */

	public static void start(Backend b) {
		try {
			b.call("erlide_xref", "start", "");
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}

	}

	public static void stop(Backend b) {
		try {
			b.call("erlide_xref", "stop", "");
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}

	}

	public static void addProject(Backend b, IErlProject project) {
		try {
			IPath outputLocation = project.getProject().getFolder(
					project.getOutputLocation()).getLocation();
			b
					.call("erlide_xref", "add_project", "s", outputLocation
							.toString());
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}

	}

	public static void update(Backend b) {
		try {
			b.call("erlide_xref", "update", "");
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}

	}

	@SuppressWarnings("boxing")
	public static FunctionRef[] functionUse(Backend b, String mod, String fun,
			int arity) {
		try {
			OtpErlangObject r = b.call(10000, "erlide_xref", "function_use",
					"aai", mod, fun, arity);
			Bindings bind = ErlUtils.match("{ok, L}", r);
			if (bind == null) {
				return null;
			}
			OtpErlangList l = (OtpErlangList) bind.get("L");
			List<FunctionRef> result = new ArrayList<FunctionRef>();
			for (OtpErlangObject e : l.elements()) {
				result.add(new FunctionRef(e));
			}
			return result.toArray(new FunctionRef[0]);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
		return null;
	}

}
