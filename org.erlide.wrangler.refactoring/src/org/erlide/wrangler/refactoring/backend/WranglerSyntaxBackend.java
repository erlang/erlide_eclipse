package org.erlide.wrangler.refactoring.backend;

import org.eclipse.core.resources.IFile;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.wrangler.refactoring.backend.SyntaxInfo.Type;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Wranglers syntax backend, which is for determining about a selection its type
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class WranglerSyntaxBackend implements IWranglerBackend {
	protected Backend backend;
	protected static String MODULE = "refac_util";
	protected static String PARSE_FUNCTION = "parse_annotate_file";
	protected static String VAR_FUNCTION = "pos_to_var_name";

	/**
	 * @param backend
	 *            Backend object
	 */
	public WranglerSyntaxBackend(final Backend backend) {
		this.backend = backend;
	}

	protected OtpErlangTuple parseFile(final IFile f) {
		final String filePath = f.getLocation().toOSString();
		final RpcResult res = backend.call_noexception(MODULE, PARSE_FUNCTION,
				"sax", filePath, "true", GlobalParameters
						.getWranglerSelection().getSearchPath());
		return parseParserResult(res.getValue());
	}

	protected OtpErlangTuple parseParserResult(final OtpErlangObject value) {
		final OtpErlangTuple backendResult = (OtpErlangTuple) value;
		if (!((OtpErlangAtom) backendResult.elementAt(0)).atomValue().equals(
				"ok"))
			return null;
		final OtpErlangTuple wranglerResult = (OtpErlangTuple) backendResult
				.elementAt(1);

		return (OtpErlangTuple) wranglerResult.elementAt(0);

	}

	protected SyntaxInfo varToPos(final OtpErlangTuple syntaxTree,
			final int line, final int col) {
		final OtpErlangInt[] position = new OtpErlangInt[2];
		position[0] = new OtpErlangInt(line);
		position[1] = new OtpErlangInt(col);
		final RpcResult res = backend.call_noexception(MODULE, VAR_FUNCTION,
				"xx", syntaxTree, new OtpErlangTuple(position));
		return parseVarInfo(res.getValue());
	}

	private SyntaxInfo parseVarInfo(final OtpErlangObject value) {
		try {
			final OtpErlangTuple result = (OtpErlangTuple) value;
			if (!((OtpErlangAtom) result.elementAt(0)).atomValue().equals("ok")) {
				return new SyntaxInfo(Type.NONE, -1, -1);
			}
			SyntaxInfo ret;
			final OtpErlangTuple res = (OtpErlangTuple) result.elementAt(1);
			final OtpErlangTuple position = (OtpErlangTuple) ((OtpErlangList) res
					.elementAt(1)).elementAt(0);
			OtpErlangLong line, col;
			line = (OtpErlangLong) position.elementAt(0);
			col = (OtpErlangLong) position.elementAt(0);
			ret = new SyntaxInfo(Type.VARIABLE, line.intValue(), col.intValue());
			return ret;
		} catch (final Exception e) {
			ErlLogger.debug(e);
			return null;
		}
	}

	/**
	 * Reutrns syntax information about a selection
	 * 
	 * @param f
	 *            selected file
	 * @param line
	 *            selected line
	 * @param pos
	 *            selected position
	 * @return syntax information
	 */
	public SyntaxInfo getSyntaxInfo(final IFile f, final int line, final int pos) {
		return varToPos(parseFile(f), line, pos);
	}

	// TODO:: implement expression checking
}
