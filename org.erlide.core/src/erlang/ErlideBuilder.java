package erlang;

import java.util.List;

import org.eclipse.core.resources.IProject;
import org.erlide.core.builder.BuilderUtils;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlideBuilder {

	public static OtpErlangObject compileYrl(final Backend backend,
			final String fn, final String output) {
		try {
			final OtpErlangObject r = backend.call(30000, "erlide_builder",
					"compile_yrl", "ss", fn, output);
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
			return backend.call(30000, "erlide_builder", "compile", "sslsla",
					fn, outputdir, includedirs, new String[] { "debug_info" });
		} catch (final Exception e) {
			ErlLogger.debug(e);
			return null;
		}
	}

	public static OtpErlangList getSourceClashes(final Backend backend,
			final String[] dirList) throws BackendException {
		final OtpErlangList res = (OtpErlangList) backend.call(
				"erlide_builder", "source_clash", "ls", (Object) dirList);
		return res;
	}

	public static OtpErlangList getCodeClashes(final Backend b)
			throws BackendException {
		final OtpErlangList res = (OtpErlangList) b.call("erlide_builder",
				"code_clash", null);
		return res;
	}

	public static void loadModule(final IProject project, final String module) {
		try {
			for (final Backend b : ErlangCore.getBackendManager()
					.getExecutionBackends(project)) {
				ErlLogger.debug(":: loading %s in %s", module, b.getInfo()
						.toString());
				b.call("erlide_builder", "load", "a", module);
			}
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

}
