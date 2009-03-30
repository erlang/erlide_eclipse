package erlang;

import java.util.List;

import org.eclipse.core.resources.IProject;
import org.erlide.core.builder.BuilderUtils;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlideBuilder {

	public static OtpErlangObject compileYrl(final Backend backend,
			final String fn, final String output) {
		try {
			final OtpErlangObject r = backend.callx("erlide_builder",
					"compile_yrl", 30000, "ss", fn, output);
			if (BuilderUtils.isDebugging()) {
				ErlLogger.debug("!!! r== " + r);
			}
			return r;
		} catch (final Exception e) {
			ErlLogger.debug(e);
			return null;
		}
	}

	public static OtpErlangObject compileErl(final Backend backend,
			final String fn, final String outputdir,
			final List<String> includedirs) {
		try {
			// FIXME add an option for the compiler options
			return backend.callx("erlide_builder", "compile", 30000, "sslsla",
					fn, outputdir, includedirs, new String[] { "debug_info" });
		} catch (final Exception e) {
			ErlLogger.debug(e);
			return null;
		}
	}

	public static OtpErlangList getSourceClashes(final Backend backend,
			final String[] dirList) throws ErlangRpcException,
			BackendException, RpcException {
		final OtpErlangList res = (OtpErlangList) backend.callx(
				"erlide_builder", "source_clash", "ls", (Object) dirList);
		return res;
	}

	public static OtpErlangList getCodeClashes(final Backend b)
			throws ErlangRpcException, BackendException, RpcException {
		final OtpErlangList res = (OtpErlangList) b.callx("erlide_builder",
				"code_clash", null);
		return res;
	}

	public static void loadModule(final IProject project, final String module) {
		try {
			for (Backend b : ErlangCore.getBackendManager()
					.getExecutionBackends(project)) {
				ErlLogger.debug(":: loading %s in %s", module, b.getInfo()
						.toString());
				b.callx("erlide_builder", "load", "a", module);
			}
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

}
