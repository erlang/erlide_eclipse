package erlang;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IErlMember;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.search.ErlangExternalFunctionCallRef;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.IdeBackend;
import org.erlide.runtime.backend.exceptions.BackendException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideNoparse {

	private static final String ERLIDE_NOPARSE = "erlide_noparse";

	public static OtpErlangTuple initialParse(final IdeBackend b,
			final String scannerModuleName, final String moduleFileName,
			final String initialText, final String stateDir,
			final String erlidePath) {
		OtpErlangTuple res = null;
		try {
			res = (OtpErlangTuple) b.rpcx(ERLIDE_NOPARSE, "initial_parse",
					"assss", scannerModuleName, moduleFileName, initialText,
					stateDir, erlidePath);
		} catch (final RpcException e) {
			e.printStackTrace();
		} catch (final BackendException e) {
			e.printStackTrace();
		}
		return res;
	}

	public static OtpErlangTuple reparse(final IdeBackend b,
			final String scannerModuleName) {
		OtpErlangTuple res = null;
		try {
			res = (OtpErlangTuple) b.rpcx(ERLIDE_NOPARSE, "reparse", "a",
					scannerModuleName);
		} catch (final RpcException e) {
			e.printStackTrace();
		} catch (final BackendException e) {
			e.printStackTrace();
		}
		return res;
	}

	public static void destroy(final IdeBackend b, final String module) {
		try {
			b.rpcx(ERLIDE_NOPARSE, "destroy", "a", module);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	public static IErlModule getModule(final IFile file) {
		final String prj = file.getProject().getName();
		final IErlModel mdl = ErlangCore.getModel();

		try {
			mdl.open(null);
			return mdl.getErlangProject(prj).getModule(file.getName());
		} catch (final ErlModelException e) {
		}
		return null;
	}

	public static IErlFunction getFunction(final IErlModule module,
			final String name, final int arity) {
		try {
			for (final IErlElement e : module.getChildren()) {
				if (e instanceof IErlFunction) {
					final IErlFunction function = (IErlFunction) e;
					if (function.getName().equals(name)
							&& function.getArity() == arity) {
						return function;
					}
				}
			}
		} catch (final ErlModelException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	public static List<ErlangExternalFunctionCallRef> find(final IdeBackend b,
			final ErlangExternalFunctionCallRef ref) {
		try {
			final OtpErlangList res = (OtpErlangList) b.rpcx(ERLIDE_NOPARSE,
					"find", "aai", ref.getModule(), ref.getFunction(), ref
							.getArity());
			final IWorkspace workspace = ResourcesPlugin.getWorkspace();
			final IWorkspaceRoot root = workspace.getRoot();
			final List<ErlangExternalFunctionCallRef> result = new ArrayList<ErlangExternalFunctionCallRef>();
			for (final OtpErlangObject i : res.elements()) {
				// [{modpath, [{{fun, arity}, Result}]}...]
				// Result = {clause, [Tokens]] | Tokens
				final OtpErlangTuple t = (OtpErlangTuple) i;
				final OtpErlangString mod = (OtpErlangString) t.elementAt(0);
				final OtpErlangList funs = (OtpErlangList) t.elementAt(1);
				final IFile file = root.getFile(new Path(mod.stringValue()));
				if (file == null) {
					continue;
				}
				final IErlModule module = getModule(file);
				for (final OtpErlangObject o : funs.elements()) {
					final OtpErlangTuple t2 = (OtpErlangTuple) o;
					final OtpErlangTuple ft = (OtpErlangTuple) t2.elementAt(0);
					final OtpErlangAtom fun = (OtpErlangAtom) ft.elementAt(0);
					final OtpErlangLong ar = (OtpErlangLong) ft.elementAt(1);
					try {
						final IErlFunction f = getFunction(module, fun
								.atomValue(), ar.intValue());
						final OtpErlangList l = (OtpErlangList) t2.elementAt(1);
						for (final OtpErlangObject j : l.elements()) {
							if (j instanceof OtpErlangList) {
								addToList(result, f, (OtpErlangList) j);
							} else if (j instanceof OtpErlangTuple) {
								final OtpErlangTuple ct = (OtpErlangTuple) j;
								final OtpErlangString cl = (OtpErlangString) ct
										.elementAt(0);
								final OtpErlangList ll = (OtpErlangList) ct
										.elementAt(1);
								final IErlFunctionClause c = getClause(f, cl
										.stringValue());
								addToList(result, c, ll);
							}
						}
					} catch (final OtpErlangRangeException e1) {
						continue;
					}
				}
			}
			return result;
		} catch (final RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	private static IErlFunctionClause getClause(final IErlFunction f,
			final String clauseHead) {
		try {
			for (final IErlElement i : f.getChildren()) {
				if (i instanceof IErlFunctionClause) {
					final IErlFunctionClause c = (IErlFunctionClause) i;
					if (c.getHead().equals(clauseHead)) {
						return c;
					}
				}
			}
		} catch (final ErlModelException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		// TODO Auto-generated method stub
		return null;
	}

	private static void addToList(
			final List<ErlangExternalFunctionCallRef> result,
			final IErlMember c, final OtpErlangList l) {
		for (final OtpErlangObject o : l.elements()) {
			final ErlangExternalFunctionCallRef ref = new ErlangExternalFunctionCallRef(
					(OtpErlangTuple) o);
			ref.setParent(c);
			result.add(ref);
		}
	}
}
