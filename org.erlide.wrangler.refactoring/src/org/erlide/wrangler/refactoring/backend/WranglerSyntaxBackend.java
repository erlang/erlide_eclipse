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

public class WranglerSyntaxBackend implements IWranglerBackend {
	protected Backend backend;
	protected static String MODULE = "refac_util";
	protected static String PARSE_FUNCTION = "parse_annotate_file";
	protected static String VAR_FUNCTION = "pos_to_var_name";

	public WranglerSyntaxBackend(Backend backend) {
		this.backend = backend;
	}

	protected OtpErlangTuple parseFile(IFile f) {
		String filePath = f.getLocation().toOSString();
		RpcResult res = backend.call_noexception(MODULE, PARSE_FUNCTION, "sax",
				filePath, "true", GlobalParameters.getWranglerSelection()
						.getSearchPath());
		return parseParserResult(res.getValue());
	}

	protected OtpErlangTuple parseParserResult(OtpErlangObject value) {
		OtpErlangTuple backendResult = (OtpErlangTuple) value;
		if (!((OtpErlangAtom) backendResult.elementAt(0)).atomValue().equals(
				"ok"))
			return null;
		OtpErlangTuple wranglerResult = (OtpErlangTuple) backendResult
				.elementAt(1);

		return (OtpErlangTuple) wranglerResult.elementAt(0);

	}

	protected SyntaxInfo varToPos(OtpErlangTuple syntaxTree, int line, int col) {
		OtpErlangInt[] position = new OtpErlangInt[2];
		position[0] = new OtpErlangInt(line);
		position[1] = new OtpErlangInt(col);
		RpcResult res = backend.call_noexception(MODULE, VAR_FUNCTION, "xx",
				syntaxTree, new OtpErlangTuple(position));
		return parseVarInfo(res.getValue());
	}

	private SyntaxInfo parseVarInfo(OtpErlangObject value) {
		try {
			OtpErlangTuple result = (OtpErlangTuple) value;
			if (!((OtpErlangAtom) result.elementAt(0)).atomValue().equals("ok")) {
				return new SyntaxInfo(Type.NONE, -1, -1);
			}
			SyntaxInfo ret;
			OtpErlangTuple res = (OtpErlangTuple) result.elementAt(1);
			OtpErlangTuple position = (OtpErlangTuple) ((OtpErlangList) res
					.elementAt(1)).elementAt(0);
			OtpErlangLong line, col;
			line = (OtpErlangLong) position.elementAt(0);
			col = (OtpErlangLong) position.elementAt(0);
			ret = new SyntaxInfo(Type.VARIABLE, line.intValue(), col.intValue());
			return ret;
		} catch (Exception e) {
			ErlLogger.debug(e);
			return null;
		}
	}

	public SyntaxInfo getSyntaxInfo(IFile f, int line, int pos) {
		return varToPos(parseFile(f), line, pos);
	}

	// TODO:: implement expression checking
}
