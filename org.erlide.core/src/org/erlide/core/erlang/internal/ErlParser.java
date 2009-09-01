/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.erlang.internal;

import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlScanner;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlComment;
import org.erlide.core.erlang.IErlMember;
import org.erlide.core.erlang.IErlModule;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.ErlBackend;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.Bindings;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.jinterface.util.ErlUtils;
import org.erlide.jinterface.util.ParserException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideNoparse;

public final class ErlParser {

	private ErlParser() {

	}

	/**
	 * @param module
	 *            module to parse
	 * @param initialText
	 *            the initial text
	 * @param initialParse
	 *            true if first time parse
	 * @param erlidePath
	 *            path to resource in eclipse
	 * @param updateCaches
	 *            update the the caches
	 * @return -record(model, {forms, comments}).
	 */
	public static boolean parse(final IErlModule module,
			final String initialText, final boolean initialParse,
			final String moduleFilePath, final String erlidePath,
			final boolean updateCaches) {
		final Backend b = ErlangCore.getBackendManager().getIdeBackend();
		if (b == null || module == null) {
			return false;
		}
		OtpErlangList forms = null;
		OtpErlangList comments = null;
		final String scannerModuleName = ErlScanner
				.createScannerModuleName(module);
		OtpErlangTuple res = null;
		ErlLogger.debug("parse="
				+ (module.getResource() == null ? module.getName() : module
						.getResource().getFullPath()) + " init_len="
				+ initialText.length() + " initialParse=" + initialParse);
		if (initialParse) {
			final String stateDir = ErlangPlugin.getDefault()
					.getStateLocation().toString();
			res = ErlideNoparse.initialParse(b, scannerModuleName,
					moduleFilePath, initialText, stateDir, erlidePath,
					updateCaches);
		} else {
			res = ErlideNoparse.reparse(b, scannerModuleName);
		}
		if (Util.isOk(res)) {
			Bindings bindings = null;
			try {
				bindings = ErlUtils.match("{ok, {_, Forms, Comments}}", res);
			} catch (final ParserException e) {
				e.printStackTrace();
			}
			if (bindings != null) {
				forms = (OtpErlangList) bindings.get("Forms");
				comments = (OtpErlangList) bindings.get("Comments");
			} else {
				ErlLogger.error("parser for %s got: %s", module.getName(), res);
			}
		} else {
			ErlLogger.error("rpc error when parsing %s: %s", module.getName(),
					res);
		}
		final ErlModule mm = (ErlModule) module;
		mm.removeChildren();
		// mm.setParseTree(forms);
		if (forms == null) {
			return true;
		}

		for (final OtpErlangObject form : forms) {
			final IErlMember elem = create(module, (OtpErlangTuple) form);
			if (elem != null) {
				mm.addMember(elem);
			}
		}
		if (comments != null) {
			for (final OtpErlangObject comment : comments) {
				final IErlComment c = createComment(module,
						(OtpErlangTuple) comment);
				if (c != null) {
					mm.addComment(c);
				}
			}
		}
		mm.fixExportedFunctions();

		return true;
	}

	/**
	 * create an IErlComment from a token record
	 * 
	 * @param IErlModule
	 *            module containing comment
	 * @param OtpErlangTuple
	 *            token record from noparse
	 * @return IErlComment
	 */
	public static IErlComment createComment(final IErlModule parent,
			final OtpErlangTuple c) {
		// from erlide_scanner.hrl:
		// -record(token, {kind, line = {Line, LastLine}, offset, length, value,
		// text}).
		final OtpErlangLong lineL = (OtpErlangLong) c.elementAt(2);
		final OtpErlangObject s = c.elementAt(5);

		int line;
		int lastLine;
		try {
			line = lineL.intValue();
		} catch (final OtpErlangRangeException x) {
			line = 0;
		}
		lastLine = line;
		try {
			if (c.elementAt(5) instanceof OtpErlangLong) {
				final OtpErlangLong lastLineL = (OtpErlangLong) c.elementAt(7);
				lastLine = lastLineL.intValue();
			}
		} catch (final OtpErlangRangeException e1) {
		}
		final ErlComment comment = new ErlComment(parent, Util.stringValue(s),
				false, line == 0 || line == 1);
		try {
			final int ofs = ((OtpErlangLong) c.elementAt(3)).intValue();
			final int len = ((OtpErlangLong) c.elementAt(4)).intValue();
			setPos(comment, line, lastLine, ofs + 1, len - 1);
		} catch (final OtpErlangRangeException e) {
			return null;
		}
		return comment;
	}

	/**
	 * create an IErlMember from a tuple from noparse
	 * 
	 * @param el
	 *            the tuple, either function or attribute
	 * @return
	 */
	private static IErlMember create(final IErlModule parent,
			final OtpErlangTuple el) {
		final OtpErlangAtom type = (OtpErlangAtom) el.elementAt(0);
		final String typeS = type.atomValue();
		if ("error".equals(typeS)) {
			final OtpErlangTuple er = (OtpErlangTuple) el.elementAt(1);

			final String msg = ErlBackend.format_error(ErlangCore
					.getBackendManager().getIdeBackend(), er);

			final ErlMessage e = new ErlMessage(parent,
					ErlMessage.MessageKind.ERROR, msg);
			setPos(e, er.elementAt(0));
			return e;
		} else if ("tree".equals(typeS)) {
			final OtpErlangTuple atr = (OtpErlangTuple) el.elementAt(3);
			final OtpErlangObject pos = ((OtpErlangTuple) el.elementAt(2))
					.elementAt(1);
			final OtpErlangTuple name = (OtpErlangTuple) atr.elementAt(1);
			final OtpErlangAtom n = (OtpErlangAtom) concreteTerm(name);
			final OtpErlangObject val = atr.elementAt(2);
			final OtpErlangObject extra = el.arity() > 4 ? el.elementAt(4)
					: null;
			return addAttribute(parent, pos, n, val, extra);
		} else if ("attribute".equals(typeS)) {
			final OtpErlangObject pos = el.elementAt(1);
			final OtpErlangAtom name = (OtpErlangAtom) el.elementAt(2);
			final OtpErlangObject val = el.elementAt(3);
			final OtpErlangObject extra = el.arity() > 4 ? el.elementAt(4)
					: null;
			return addAttribute(parent, pos, name, val, extra);
		} else if ("function".equals(typeS)) {
			final ErlFunction f = makeErlFunction(parent, el);
			final OtpErlangList clauses = (OtpErlangList) el.elementAt(6);
			final ErlFunctionClause[] cls = new ErlFunctionClause[clauses
					.arity()];
			for (int i = 0; i < clauses.arity(); i++) {
				final OtpErlangTuple clause = (OtpErlangTuple) clauses
						.elementAt(i);
				final ErlFunctionClause cl = makeErlFunctionClause(f, i, clause);
				cls[i] = cl;
			}
			f.setChildren(cls);
			return f;
		} else {
			ErlLogger.debug("unknown: " + el);
		}
		return null;
	}

	/**
	 * @param parent
	 *            module
	 * @param el
	 *            -record(function, {pos, name, arity, args, head, clauses,
	 *            name_pos, code, external_refs, comment}).
	 * @return ErlFunction
	 */
	private static ErlFunction makeErlFunction(final IErlModule parent,
			final OtpErlangTuple el) {
		final OtpErlangTuple pos = (OtpErlangTuple) el.elementAt(1);
		final OtpErlangAtom name = (OtpErlangAtom) el.elementAt(2);
		final OtpErlangLong arity = (OtpErlangLong) el.elementAt(3);
		final OtpErlangObject head = el.elementAt(5);
		final OtpErlangTuple namePos = (OtpErlangTuple) el.elementAt(7);
		ErlFunction f = null;
		try {
			String comment = Util.stringValue(el.elementAt(10));
			if (comment != null) {
				comment = comment.replaceAll("\n", "<br/>");
			}
			f = new ErlFunction((ErlElement) parent, name.atomValue(), arity
					.intValue(), Util.stringValue(head), comment);
		} catch (final OtpErlangRangeException e) {
			return f;
		}
		setPos(f, pos);
		try {
			setNamePos(f, namePos);
		} catch (final OtpErlangRangeException e) {
			return f;
		}
		return f;
	}

	/**
	 * @param f
	 *            function
	 * @param i
	 *            clause number
	 * @param clause
	 *            -record(clause, {pos, name, args, head, code, name_pos}).
	 * @return ErlFunctionClause
	 * 
	 * 
	 */
	private static ErlFunctionClause makeErlFunctionClause(final ErlFunction f,
			final int i, final OtpErlangTuple clause) {
		final OtpErlangTuple cpos = (OtpErlangTuple) clause.elementAt(1);
		final OtpErlangObject head = clause.elementAt(4);
		final OtpErlangTuple cnamePos = (OtpErlangTuple) clause.elementAt(6);
		final ErlFunctionClause cl = new ErlFunctionClause(f, "#" + i, Util
				.stringValue(head));
		try {
			setNamePos(cl, cnamePos);
		} catch (final OtpErlangRangeException e) {
			ErlLogger.warn(e);
		}
		setPos(cl, cpos);
		return cl;
	}

	private static void setNamePos(final ErlMember f,
			final OtpErlangTuple namePos) throws OtpErlangRangeException {
		final OtpErlangTuple tpos = namePos;
		final OtpErlangTuple tpos1 = (OtpErlangTuple) tpos.elementAt(0);
		final int ofs = ((OtpErlangLong) tpos1.elementAt(1)).intValue();
		final int len = ((OtpErlangLong) tpos.elementAt(1)).intValue();
		f.setNameRangeStartEnd(ofs, ofs + len);
	}

	/**
	 * @param parent
	 * @param pos
	 * @param name
	 * @param val
	 * @param extra
	 *            TODO
	 * @param el
	 * @return
	 */
	private static IErlMember addAttribute(final IErlModule parent,
			final OtpErlangObject pos, final OtpErlangAtom name,
			final OtpErlangObject val, final OtpErlangObject extra) {
		final String nameS = name.atomValue();
		if ("module".equals(nameS)) {
			if (val instanceof OtpErlangAtom) {
				final OtpErlangAtom o = (OtpErlangAtom) val;
				final String s = Util.stringValue(extra);
				final ErlAttribute r = new ErlAttribute(parent, nameS, o, s);
				setPos(r, pos);
				// r.setParseTree(val);
				return r;
			}
		} else if ("import".equals(nameS)) {
			if (val instanceof OtpErlangTuple) {
				final OtpErlangTuple t = (OtpErlangTuple) val;
				if (t.elementAt(0) instanceof OtpErlangAtom
						&& t.elementAt(1) instanceof OtpErlangList) {
					final OtpErlangAtom importModule = (OtpErlangAtom) t
							.elementAt(0);
					final OtpErlangList functionList = (OtpErlangList) t
							.elementAt(1);
					final ErlImport imp = new ErlImport(parent, importModule
							.atomValue(), functionList);
					setPos(imp, pos);
					// imp.setParseTree(val);
					return imp;
				}
			}
		} else if ("export".equals(nameS)) {
			final OtpErlangList functionList = (OtpErlangList) val;
			final ErlExport ex = new ErlExport(parent, functionList);
			setPos(ex, pos);
			// ex.setParseTree(val);
			return ex;
		} else if ("record".equals(nameS)) {
			if (val instanceof OtpErlangTuple) {
				final OtpErlangTuple recordTuple = (OtpErlangTuple) val;
				if (recordTuple.elementAt(0) instanceof OtpErlangAtom) {
					final String recordName = ((OtpErlangAtom) recordTuple
							.elementAt(0)).toString();
					final String s = extra instanceof OtpErlangString ? ((OtpErlangString) extra)
							.stringValue()
							: null;
					final OtpErlangList l = (OtpErlangList) recordTuple
							.elementAt(1);
					final ErlRecordDef r = new ErlRecordDef(parent, recordName,
							s, l);
					setPos(r, pos);
					// r.setParseTree(val);
					return r;
				}
			}
			if (val instanceof OtpErlangAtom) {
				final OtpErlangAtom nameA = (OtpErlangAtom) val;
				final String recordName = nameA.atomValue();
				final String s = extra instanceof OtpErlangString ? ((OtpErlangString) extra)
						.stringValue()
						: null;
				final ErlRecordDef r = new ErlRecordDef(parent, recordName, s);
				setPos(r, pos);
				// r.setParseTree(val);
				return r;
			}
		} else if ("type".equals(nameS) || "spec".equals(nameS)) {
			final String s = Util.stringValue(extra);
			final int p = s.indexOf('(');
			final String typeName = p < 0 ? s : s.substring(0, p);
			final ErlTypespec a = new ErlTypespec((ErlElement) parent,
					typeName, null, s);
			setPos(a, pos);
			return a;

		} else if ("define".equals(nameS)) {
			if (val instanceof OtpErlangAtom) {
				// final OtpErlangAtom o = (OtpErlangAtom) val;
				final String s = Util.stringValue(extra);
				// final ErlMacroDef r = new ErlMacroDef(parent, o.toString(),
				// s);
				final ErlMacroDef r = new ErlMacroDef(parent, s);
				setPos(r, pos);
				// r.setParseTree(val);
				return r;
			}
			if (val instanceof OtpErlangList) {
				final OtpErlangList macroList = (OtpErlangList) val;
				if (macroList.elementAt(0) instanceof OtpErlangTuple) {
					final OtpErlangTuple macroNameTuple = (OtpErlangTuple) macroList
							.elementAt(0);
					OtpErlangObject o = macroNameTuple.elementAt(2);
					if (o instanceof OtpErlangTuple) {
						o = ((OtpErlangTuple) o).elementAt(2);
					}
					ErlMacroDef r;
					if (o instanceof OtpErlangAtom) {
						final String macroName = ((OtpErlangAtom) o)
								.atomValue();
						r = new ErlMacroDef(parent, macroName, null);
					} else {
						// what do we do here? the define isn't correct
						// Erlang...
						ErlLogger.warn("Strange macro definition in %s: %s",
								parent.getName(), o.toString());
						r = new ErlMacroDef(parent, o.toString(), null);
					}
					setPos(r, pos);
					// r.setParseTree(val);
					return r;
				}
			}
		}

		// user-defined attribute? or maybe if else endif...
		// OtpErlangObject val1 = concreteTerm(val);
		// if (val instanceof OtpErlangList) {
		// final OtpErlangList list = (OtpErlangList) val;
		// if (list.arity() == 0) {
		// val1 = null;
		// }
		// }

		// final ErlAttribute a = new ErlAttribute(parent, nameS, val1, null);
		final ErlAttribute a = new ErlAttribute(parent, nameS, val, null);
		setPos(a, pos);
		// a.setParseTree(val);
		return a;

	}

	private static boolean setPos(final SourceRefElement e,
			final OtpErlangObject pos) {
		if (!(pos instanceof OtpErlangTuple)) {
			if (pos instanceof OtpErlangLong) {
				int ipos = 999999;
				try {
					ipos = ((OtpErlangLong) pos).intValue();
				} catch (final OtpErlangRangeException e1) {
				}
				setPos(e, 0, 0, ipos, 0);
				return true;
			}
			ErlLogger.debug("!> expecting pos tuple, got " + pos);
			return false;
		}
		try {
			// pos={{Line, LastLine, Offset}, PosLength} or {{Line, Offset},
			// PosLength}
			final OtpErlangTuple tpos = (OtpErlangTuple) pos;
			final OtpErlangTuple tpos1 = (OtpErlangTuple) tpos.elementAt(0);
			final int line = ((OtpErlangLong) tpos1.elementAt(0)).intValue();
			int lastLine = ((OtpErlangLong) tpos1.elementAt(1)).intValue();
			int ofs;
			if (tpos1.arity() > 2) {
				ofs = ((OtpErlangLong) tpos1.elementAt(2)).intValue();
			} else {
				ofs = lastLine;
				lastLine = line;
			}
			final int len = ((OtpErlangLong) tpos.elementAt(1)).intValue();
			setPos(e, line, lastLine, ofs, len);
			return true;
		} catch (final OtpErlangRangeException ex) {
			return false;
		}

	}

	private static void setPos(final SourceRefElement e, final int line,
			final int lastLine, final int ofs, final int len) {
		e.setSourceRangeStart(ofs);
		e.setSourceRangeEnd(ofs + len - 1);
		// FIXME: why is the token 1 char too long?
		e.setLineStart(line);
		e.setLineEnd(lastLine);
	}

	private static OtpErlangObject concreteTerm(final OtpErlangObject val) {
		if (val instanceof OtpErlangList) {
			final OtpErlangList ll = (OtpErlangList) val;
			final OtpErlangObject[] res = new OtpErlangObject[ll.arity()];
			for (int i = 0; i < ll.arity(); i++) {
				res[i] = concreteTerm(ll.elementAt(i));
			}
			return new OtpErlangList(res);
		}
		try {
			return ErlBackend.concreteSyntax(ErlangCore.getBackendManager()
					.getIdeBackend(), val);
		} catch (final Exception e) {
			return val;
		}
	}

}
