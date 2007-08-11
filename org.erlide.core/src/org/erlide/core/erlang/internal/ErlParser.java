/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.erlang.internal;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.erlide.basiccore.ErlLogger;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlToken;
import org.erlide.core.erlang.IErlComment;
import org.erlide.core.erlang.IErlMember;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlScanner;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlParser {

	public boolean parse_new(IErlModule module) {
		final IErlScanner scanner = module.getScanner();
		if (scanner == null) {
			return false;
		}
		final ErlToken[] tokens = scanner.getTokens();

		if (tokens == null) {
			return false;
		}

		final ErlModule mm = (ErlModule) module;
		mm.reset();
		// mm.setParseTree(forms);

		final List<ErlToken[]> sforms = splitForms(tokens);
		for (final Iterator<ErlToken[]> iter = sforms.iterator(); iter
				.hasNext();) {
			final ErlToken[] element = iter.next();
			final OtpErlangObject form = parseForm(element);
			final IErlMember elem = create(module, (OtpErlangTuple) form);
			if (elem != null) {
				mm.addMember(elem);
			}
		}

		final OtpErlangList comments = null;

		if (comments != null) {
			for (int i = 0; i < comments.arity(); i++) {
				final IErlComment c = createComment(module,
						(OtpErlangTuple) comments.elementAt(i));
				if (c != null) {
					mm.addComment(c);
				}
			}
		}
		mm.fixExportedFunctions();

		return true;
	}

	private OtpErlangObject parseForm(ErlToken[] element) {
		return null;
	}

	private List<ErlToken[]> splitForms(ErlToken[] tokens) {
		final List<ErlToken[]> result = new ArrayList<ErlToken[]>(10);
		List<ErlToken> tmp = new ArrayList<ErlToken>(50);
		for (final ErlToken token : tokens) {
			tmp.add(token);
			if ("dot".equals(token.getKind())) {
				final ErlToken[] tmpar = new ErlToken[tmp.size()];
				result.add(tmp.toArray(tmpar));
				tmp = new ArrayList<ErlToken>(50);
			}
		}
		return result;
	}

	/* NOT USED */
	/*
	 * private List splitFunction(ErlToken[] tokens) { // match "^ atom (" and ")
	 * ->" to split
	 * 
	 * final List result = new ArrayList(10); List tmp = new ArrayList(50); for
	 * (int i = 0; i < tokens.length; i++) { final ErlToken token = tokens[i];
	 * tmp.add(token); if (token.getKind().equals("dot")) { final ErlToken[]
	 * tmpar = new ErlToken[tmp.size()]; result.add(tmp.toArray(tmpar)); tmp =
	 * new ArrayList(50); } } return result; }
	 */

	public boolean parse(IErlModule module) {
		try {
			final String doc = module.getBuffer().getContents();
			final IErlScanner scanner = module.getScanner();

			if (doc == null) {
				return true;
			}
			if (doc.length() == 0) {
				return true;
			}

			final IBackend b = BackendManager.getDefault().getIdeBackend();

			OtpErlangList forms = null, comments = null;
			try {
				final OtpErlangString source = new OtpErlangString(doc);

				OtpErlangTuple res = (OtpErlangTuple) BackendUtil.checkRpc(b
						.rpc("erlide_model", "parse", source,
								new OtpErlangString(module.getElementName())));
				if (((OtpErlangAtom) res.elementAt(0)).atomValue().compareTo(
						"ok") == 0) {
					forms = (OtpErlangList) res.elementAt(1);
				} else {
					ErlLogger.log("rpc err:: " + res);
				}

				ErlToken[] t = scanner.getTokens();
				StringBuffer sb = new StringBuffer();
				for (ErlToken tk : t) {
					sb.append(" " + tk.toString());
				}
				ErlLogger.log(sb);
				ErlLogger.log(forms);
				ErlLogger.log("-----------------------");

				res = (OtpErlangTuple) BackendUtil.checkRpc(b.rpc(
						"erlide_model", "comments", source));
				if (((OtpErlangAtom) res.elementAt(0)).atomValue().compareTo(
						"ok") == 0) {
					comments = (OtpErlangList) res.elementAt(1);
				} else {
					ErlLogger.log("rpc err:: " + res);
				}
			} catch (final BackendException e1) {
				e1.printStackTrace();
			}
			final ErlModule mm = (ErlModule) module;
			mm.reset();
			mm.setParseTree(forms);
			if (forms == null) {
				return true;
			}

			for (int i = 0; i < forms.arity(); i++) {
				final IErlMember elem = create(module, (OtpErlangTuple) forms
						.elementAt(i));
				if (elem != null) {
					mm.addMember(elem);
				}
			}
			if (comments != null) {
				for (int i = 0; i < comments.arity(); i++) {
					final IErlComment c = createComment(module,
							(OtpErlangTuple) comments.elementAt(i));
					if (c != null) {
						mm.addComment(c);
					}
				}
			}
			mm.fixExportedFunctions();
		} catch (final ErlModelException e) {
			e.printStackTrace();
		}

		return true;
	}

	/**
	 * create a IErlComment from a tuple
	 * 
	 * @param IErlModule
	 *            parent
	 * @param OtpErlangTuple
	 *            c
	 * @return IErlComment
	 */
	private IErlComment createComment(IErlModule parent, OtpErlangTuple c) {
		final OtpErlangTuple pos = (OtpErlangTuple) c.elementAt(0);
		final OtpErlangTuple lineOffs = (OtpErlangTuple) pos.elementAt(0);
		final OtpErlangLong line = (OtpErlangLong) lineOffs.elementAt(0);
		final OtpErlangString s = (OtpErlangString) c.elementAt(1);
		int l;
		try {
			l = line.intValue();
		} catch (final OtpErlangRangeException x) {
			l = 0;
		}
		final ErlComment comment = new ErlComment(parent, s.stringValue(),
				false, l == 1);
		try {
			final OtpErlangTuple tpos = (OtpErlangTuple) pos.elementAt(0);
			final int ofs = ((OtpErlangLong) (tpos.elementAt(1))).intValue();
			final int len = ((OtpErlangLong) (pos.elementAt(1))).intValue();
			setPos(comment, ofs + 1, len - 1);
		} catch (final OtpErlangRangeException e) {
			return null;
		}
		return comment;
	}

	/**
	 * create a IErlMember from a erl_syntax tree
	 * 
	 * @param el
	 * @return
	 */
	private IErlMember create(IErlModule parent, OtpErlangTuple el) {
		// ErlLogger.log("#! " + el.toString());
		final OtpErlangAtom type = (OtpErlangAtom) el.elementAt(0);
		if ("error".equals(type.atomValue())) {
			final OtpErlangTuple er = (OtpErlangTuple) el.elementAt(1);

			final String msg = format_error(er);

			final ErlError e = new ErlError(parent, msg);
			// TODO sometimes the pos looks different than expected
			setPos(e, er.elementAt(0));
			e.setParseTree(el);
			return e;
		} else if ("tree".equals(type.atomValue())) {
			final OtpErlangTuple atr = (OtpErlangTuple) el.elementAt(3);
			final OtpErlangObject pos = ((OtpErlangTuple) el.elementAt(2))
					.elementAt(1);

			final OtpErlangTuple name = (OtpErlangTuple) atr.elementAt(1);
			final OtpErlangAtom n = (OtpErlangAtom) concreteTerm(name);
			// ErlLogger.log("@nam " + name.toString());
			final OtpErlangObject val = atr.elementAt(2);
			// ErlLogger.log("@val " + val.toString());
			return addAttribute(parent, pos, n, val);
		} else if ("attribute".equals(type.atomValue())) {
			final OtpErlangObject pos = el.elementAt(1);
			// ErlLogger.log("@pos " + pos.toString());
			final OtpErlangAtom name = (OtpErlangAtom) el.elementAt(2);
			// ErlLogger.log("@nam " + name.toString());
			final OtpErlangObject val = el.elementAt(3);
			// ErlLogger.log("@val " + val.toString());
			return addAttribute(parent, pos, name, val);
		} else if ("function".equals(type.atomValue())) {
			OtpErlangObject pos = el.elementAt(1);
			final OtpErlangAtom name = (OtpErlangAtom) el.elementAt(2);
			final OtpErlangLong arity = (OtpErlangLong) el.elementAt(3);
			final OtpErlangList clauses = (OtpErlangList) el.elementAt(4);

			// try
			// {
			// IErlAttribute exp = parent.getAttribute("export");
			//
			// } catch (ErlModelException e1)
			// {
			// }

			ErlFunction f = null;
			try {
				f = new ErlFunction((ErlElement) parent, name.atomValue(),
						arity.intValue());
				setPos(f, pos);
				final ErlFunctionClause[] cls = new ErlFunctionClause[clauses
						.arity()];
				for (int i = 0; i < clauses.arity(); i++) {
					// ErlLogger.log(" clause: " + clauses.elementAt(i));
					cls[i] = new ErlFunctionClause(f, "#" + i);
					pos = ((OtpErlangTuple) (clauses.elementAt(i)))
							.elementAt(1);
					// OtpErlangList args = (OtpErlangList)
					// ((OtpErlangTuple)(clauses.elementAt(i))).elementAt(2);
					cls[i].setParseTree(clauses.elementAt(i));

					setPos(cls[i], pos);
				}
				f.setChildren(cls);
				f.setParseTree(el);
			} catch (final OtpErlangRangeException e) {
				e.printStackTrace();
			}
			return f;
		} else {
			ErlLogger.log("unknown: " + el);
		}
		return null;
	}

	/**
	 * @param parent
	 * @param el
	 * @param pos
	 * @param name
	 * @param val
	 * @return
	 */
	private IErlMember addAttribute(IErlModule parent, OtpErlangObject pos,
			OtpErlangAtom name, OtpErlangObject val) {
		if ("import".equals(name.atomValue())) {
			if (val instanceof OtpErlangTuple) {
				final OtpErlangTuple t = (OtpErlangTuple) val;
				final OtpErlangAtom importModule = (OtpErlangAtom) t
						.elementAt(0);
				final OtpErlangList functionList = (OtpErlangList) t
						.elementAt(1);
				final ErlImport imp = new ErlImport(parent, importModule
						.atomValue(), functionList);
				setPos(imp, pos);
				imp.setParseTree(val);
				return imp;
			}
			// ErlImport imp = new ErlImport(val);
		} else if ("export".equals(name.atomValue())) {
			// OtpErlangList exportList = (OtpErlangList) val;
			final ErlExport ex = new ErlExport(parent);
			// ErlExportFunction[] funs = new
			// ErlExportFunction[exportList.arity()];
			// for (int i = 0; i < exportList.arity(); i++)
			// {
			// ErlLogger.log(" exportFun: " + exportList.elementAt(i));
			// OtpErlangTuple xf = (OtpErlangTuple)exportList.elementAt(i);
			// String funName = ((OtpErlangAtom)xf.elementAt(0)).atomValue();
			// try {
			// int funArity = ((OtpErlangLong)xf.elementAt(1)).intValue();
			// funs[i] = new ErlExportFunction(ex, funName, funArity);
			// //pos = ((OtpErlangTuple) (clauses.elementAt(i))).elementAt(1);
			// // OtpErlangList args = (OtpErlangList)
			// // ((OtpErlangTuple)(clauses.elementAt(i))).elementAt(2);
			// funs[i].setParseTree(exportList.elementAt(i));
			// setPos(funs[i], pos);
			// } catch (OtpErlangRangeException e) {
			// e.printStackTrace();
			// }
			// }
			// ex.setChildren(funs);
			setPos(ex, pos);
			ex.setParseTree(val);
			return ex;
		} else if ("record".equals(name.atomValue())) {
			if (val instanceof OtpErlangTuple) {
				final OtpErlangTuple recordTuple = (OtpErlangTuple) val;
				final String recordName = ((OtpErlangAtom) recordTuple
						.elementAt(0)).atomValue();
				final ErlRecordDef r = new ErlRecordDef(parent, recordName);
				setPos(r, pos);
				r.setParseTree(val);
				return r;
			}
		} else if ("define".equals(name.atomValue())) {
			if (val instanceof OtpErlangList) {
				final OtpErlangList macroList = (OtpErlangList) val;
				final OtpErlangTuple macroNameTuple = (OtpErlangTuple) macroList
						.elementAt(0);
				OtpErlangObject o = macroNameTuple.elementAt(2);
				if (o instanceof OtpErlangTuple) {
					o = ((OtpErlangTuple) o).elementAt(2);
				}
				ErlMacroDef r;
				if (o instanceof OtpErlangAtom) {
					final String macroName = ((OtpErlangAtom) o).atomValue();
					r = new ErlMacroDef(parent, macroName);
				} else {
					// what do we do here? the define isn't correct Erlang...
					r = new ErlMacroDef(parent, o.toString());
				}
				setPos(r, pos);
				r.setParseTree(val);
				return r;
			}
		}

		// user-defined attribute
		final OtpErlangObject val1 = concreteTerm(val);

		final ErlAttribute a = new ErlAttribute((ErlElement) parent, name
				.atomValue(), val1);
		setPos(a, pos);
		a.setParseTree(val);
		return a;

	}

	private String format_error(OtpErlangObject object) {
		final OtpErlangTuple err = (OtpErlangTuple) object;
		OtpErlangAtom mod = (OtpErlangAtom) err.elementAt(1);
		OtpErlangObject arg = err.elementAt(2);

		String res;
		try {
			RpcResult r = BackendManager.getDefault().getIdeBackend().rpc(
					mod.atomValue(), "format_error", arg);
			r = BackendManager.getDefault().getIdeBackend().rpc("lists",
					"flatten", r.getValue());
			res = ((OtpErlangString) r.getValue()).stringValue();
		} catch (ErlangRpcException e) {
			e.printStackTrace();
			res = err.toString();
		}
		return res;
	}

	private boolean setPos(SourceRefElement e, OtpErlangObject pos) {
		if (!(pos instanceof OtpErlangTuple)) {
			System.out.println("!> expecting pos tuple, got " + pos);
			return false;
		}
		try {
			final OtpErlangTuple tpos = (OtpErlangTuple) pos;
			final OtpErlangTuple tpos1 = (OtpErlangTuple) tpos.elementAt(0);
			final int ofs = ((OtpErlangLong) (tpos1.elementAt(1))).intValue();
			final int len = ((OtpErlangLong) (tpos.elementAt(1))).intValue();
			setPos(e, ofs, len);
			return true;
		} catch (final OtpErlangRangeException ex) {
			return false;
		}

	}

	private void setPos(SourceRefElement e, int ofs, int len) {
		e.setSourceRangeStart(ofs - 1);
		e.setSourceRangeEnd(ofs + len - 2);
	}

	private OtpErlangObject concreteTerm(OtpErlangObject val) {
		if (val instanceof OtpErlangList) {
			final OtpErlangList ll = (OtpErlangList) val;
			final OtpErlangObject[] res = new OtpErlangObject[ll.arity()];
			for (int i = 0; i < ll.arity(); i++) {
				res[i] = concreteTerm(ll.elementAt(i));
			}
			return new OtpErlangList(res);
		} else {
			try {
				return BackendUtil.checkRpc(BackendManager.getDefault()
						.getIdeBackend().rpc("erlide_syntax", "concrete", val));
			} catch (final BackendException e) {
				return val;
			}
		}
	}

}
