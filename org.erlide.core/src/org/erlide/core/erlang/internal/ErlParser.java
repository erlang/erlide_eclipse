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

import org.eclipse.core.resources.IResource;
import org.erlide.basiccore.ErlLogger;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlScanner;
import org.erlide.core.erlang.IErlComment;
import org.erlide.core.erlang.IErlMember;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.util.Util;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideBackend;
import erlang.ErlideNoparse;

public class ErlParser {

	/**
	 * @param module
	 *            module to parse
	 * @param initialText
	 *            the initial text
	 * @param initialParse
	 *            true if first time parse
	 * @return -record(model, {forms, comments}).
	 */
	public boolean parse(final IErlModule module, final String initialText,
			final boolean initialParse) {
		final IBackend b = BackendManager.getDefault().getIdeBackend();
		OtpErlangList forms = null, comments = null;
		final String scannerModuleName = ErlScanner
				.createScannerModuleName(module);
		OtpErlangTuple res = null;
		ErlLogger.debug("parse " + module.getName() + " init len "
				+ initialText.length() + " initialParse " + initialParse);
		if (initialParse) {
			final IResource resource = module.getResource();
			final String moduleFileName = resource.getLocation().toString();
			final String stateDir = ErlangPlugin.getDefault()
					.getStateLocation().toString();
			res = ErlideNoparse.initialParse(b, scannerModuleName,
					moduleFileName, initialText, stateDir);
		} else {
			res = ErlideNoparse.reparse(b, scannerModuleName);
		}
		if (res != null
				&& ((OtpErlangAtom) res.elementAt(0)).atomValue().compareTo(
						"ok") == 0) {
			final OtpErlangTuple t = (OtpErlangTuple) res.elementAt(1);
			forms = (OtpErlangList) t.elementAt(1);
			comments = (OtpErlangList) t.elementAt(2);
		} else {
			ErlLogger.debug("rpc err:: " + res);
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
	public IErlComment createComment(final IErlModule parent,
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
				false, line == 1);
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
	 * create a IErlMember from a erl_syntax tree
	 * 
	 * @param el
	 * @return
	 */
	private IErlMember create(final IErlModule parent, final OtpErlangTuple el) {
		// ErlLogger.debug("#! " + el.toString());
		final OtpErlangAtom type = (OtpErlangAtom) el.elementAt(0);
		if ("error".equals(type.atomValue())) {
			final OtpErlangTuple er = (OtpErlangTuple) el.elementAt(1);

			final String msg = ErlideBackend.format_error(er);

			final ErlMessage e = new ErlMessage(parent,
					ErlMessage.MessageKind.ERROR, msg);
			setPos(e, er.elementAt(0));
			// e.setParseTree(el);
			return e;
		} else if ("tree".equals(type.atomValue())) {
			final OtpErlangTuple atr = (OtpErlangTuple) el.elementAt(3);
			final OtpErlangObject pos = ((OtpErlangTuple) el.elementAt(2))
					.elementAt(1);
			final OtpErlangTuple name = (OtpErlangTuple) atr.elementAt(1);
			final OtpErlangAtom n = (OtpErlangAtom) concreteTerm(name);
			final OtpErlangObject val = atr.elementAt(2);
			return addAttribute(parent, pos, n, val);
		} else if ("attribute".equals(type.atomValue())) {
			final OtpErlangObject pos = el.elementAt(1);
			final OtpErlangAtom name = (OtpErlangAtom) el.elementAt(2);
			final OtpErlangObject val = el.elementAt(3);
			return addAttribute(parent, pos, name, val);
		} else if ("function".equals(type.atomValue())) {
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
	 *            name_pos}).
	 * @return ErlFunction
	 */
	private ErlFunction makeErlFunction(final IErlModule parent,
			final OtpErlangTuple el) {
		final OtpErlangTuple pos = (OtpErlangTuple) el.elementAt(1);
		final OtpErlangAtom name = (OtpErlangAtom) el.elementAt(2);
		final OtpErlangLong arity = (OtpErlangLong) el.elementAt(3);
		final OtpErlangObject head = el.elementAt(5);
		final OtpErlangTuple namePos = (OtpErlangTuple) el.elementAt(7);
		ErlFunction f = null;
		try {
			f = new ErlFunction((ErlElement) parent, name.atomValue(), arity
					.intValue(), Util.stringValue(head));
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
	private ErlFunctionClause makeErlFunctionClause(final ErlFunction f,
			final int i, final OtpErlangTuple clause) {
		final OtpErlangTuple cpos = (OtpErlangTuple) clause.elementAt(1);
		final OtpErlangObject head = clause.elementAt(4);
		final OtpErlangTuple cnamePos = (OtpErlangTuple) clause.elementAt(6);
		final ErlFunctionClause cl = new ErlFunctionClause(f, "#" + i, Util
				.stringValue(head));
		try {
			setNamePos(cl, cnamePos);
		} catch (final OtpErlangRangeException e) {
			e.printStackTrace();
		}
		setPos(cl, cpos);
		return cl;
	}

	private void setNamePos(final ErlMember f, final OtpErlangTuple namePos)
			throws OtpErlangRangeException {
		final OtpErlangTuple tpos = namePos;
		final OtpErlangTuple tpos1 = (OtpErlangTuple) tpos.elementAt(0);
		final int ofs = ((OtpErlangLong) tpos1.elementAt(1)).intValue();
		final int len = ((OtpErlangLong) tpos.elementAt(1)).intValue();
		if (ErlScanner.UseScanner2) {
			f.setNameRangeStartEnd(ofs, ofs + len);
		} else {
			f.setNameRangeStartEnd(ofs - 1, ofs + len - 1);
		}
	}

	/**
	 * @param parent
	 * @param el
	 * @param pos
	 * @param name
	 * @param val
	 * @return
	 */
	private IErlMember addAttribute(final IErlModule parent,
			final OtpErlangObject pos, final OtpErlangAtom name,
			final OtpErlangObject val) {
		if ("import".equals(name.atomValue())) {
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
			// ErlImport imp = new ErlImport(val);
		} else if ("export".equals(name.atomValue())) {
			// OtpErlangList exportList = (OtpErlangList) val;
			final ErlExport ex = new ErlExport(parent);
			// ErlExportFunction[] funs = new
			// ErlExportFunction[exportList.arity()];
			// for (int i = 0; i < exportList.arity(); i++)
			// {
			// ErlLogger.debug(" exportFun: " + exportList.elementAt(i));
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
			// ex.setParseTree(val);
			return ex;
		} else if ("record".equals(name.atomValue())) {
			if (val instanceof OtpErlangTuple) {
				final OtpErlangTuple recordTuple = (OtpErlangTuple) val;
				if (recordTuple.elementAt(0) instanceof OtpErlangAtom) {
					final String recordName = ((OtpErlangAtom) recordTuple
							.elementAt(0)).atomValue();
					final ErlRecordDef r = new ErlRecordDef(parent, recordName);
					setPos(r, pos);
					// r.setParseTree(val);
					return r;
				}
			}
			if (val instanceof OtpErlangAtom) {
				final OtpErlangAtom nameA = (OtpErlangAtom) val;
				final String recordName = nameA.atomValue();
				final ErlRecordDef r = new ErlRecordDef(parent, recordName);
				setPos(r, pos);
				// r.setParseTree(val);
				return r;
			}
		} else if ("define".equals(name.atomValue())) {
			if (val instanceof OtpErlangAtom) {
				final OtpErlangAtom o = (OtpErlangAtom) val;
				final ErlMacroDef r = new ErlMacroDef(parent, o.atomValue());
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
						r = new ErlMacroDef(parent, macroName);
					} else {
						// what do we do here? the define isn't correct
						// Erlang...
						ErlLogger.warn("Strange macro definition in %s: %s",
								parent.getName(), o.toString());
						r = new ErlMacroDef(parent, o.toString());
					}
					setPos(r, pos);
					// r.setParseTree(val);
					return r;
				}
			}
		}

		// user-defined attribute? or maybe if else endif...
		OtpErlangObject val1 = concreteTerm(val);
		if (val instanceof OtpErlangList) {
			final OtpErlangList list = (OtpErlangList) val;
			if (list.arity() == 0) {
				val1 = null;
			}
		}

		final ErlAttribute a = new ErlAttribute((ErlElement) parent, name
				.atomValue(), val1);
		setPos(a, pos);
		// a.setParseTree(val);
		return a;

	}

	private boolean setPos(final SourceRefElement e, final OtpErlangObject pos) {
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

	private void setPos(final SourceRefElement e, final int line,
			final int lastLine, final int ofs, final int len) {
		if (ErlScanner.UseScanner2) {
			e.setSourceRangeStart(ofs);
			e.setSourceRangeEnd(ofs + len - 1); // FIXME: why is the token 1
			// char too long?
			e.setLineStart(line);
			e.setLineEnd(lastLine);
		} else {
			e.setSourceRangeStart(ofs - 1);
			e.setSourceRangeEnd(ofs + len - 2);
			e.setLineStart(line);
			e.setLineEnd(lastLine);
			// parser (noparse)
		}
	}

	private OtpErlangObject concreteTerm(final OtpErlangObject val) {
		if (val instanceof OtpErlangList) {
			final OtpErlangList ll = (OtpErlangList) val;
			final OtpErlangObject[] res = new OtpErlangObject[ll.arity()];
			for (int i = 0; i < ll.arity(); i++) {
				res[i] = concreteTerm(ll.elementAt(i));
			}
			return new OtpErlangList(res);
		}
		try {
			return ErlideBackend.concreteSyntax(val);
		} catch (final Exception e) {
			return val;
		}
	}

}
