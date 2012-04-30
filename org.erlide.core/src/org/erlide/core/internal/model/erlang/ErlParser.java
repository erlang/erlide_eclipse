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
package org.erlide.core.internal.model.erlang;

import java.util.ArrayList;
import java.util.List;

import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendHelper;
import org.erlide.backend.IBackend;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.internal.model.root.ErlMember;
import org.erlide.core.model.erlang.IErlAttribute;
import org.erlide.core.model.erlang.IErlComment;
import org.erlide.core.model.erlang.IErlImport;
import org.erlide.core.model.erlang.IErlMember;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlParser;
import org.erlide.core.model.erlang.IErlRecordDef;
import org.erlide.core.model.root.IErlElement;
import org.erlide.jinterface.Bindings;
import org.erlide.jinterface.ErlLogger;
import org.erlide.utils.ErlUtils;
import org.erlide.utils.TermParserException;
import org.erlide.utils.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

/**
 * @author jakob
 * 
 */
public final class ErlParser implements IErlParser {

    public ErlParser() {
    }

    @Override
    public boolean parse(final IErlModule module, final String scannerName,
            final boolean initialParse, final String path,
            final boolean useCaches) {
        final IBackend b = BackendCore.getBackendManager().getIdeBackend();
        if (b == null || module == null) {
            return false;
        }
        OtpErlangList forms = null;
        OtpErlangList comments = null;
        OtpErlangTuple res = null;
        if (initialParse) {
            // ErlLogger.debug("initialParse %s", path);
            final String stateDir = ErlangPlugin.getDefault()
                    .getStateLocation().toString();
            res = ErlideNoparse.initialParse(b, scannerName, path, stateDir,
                    useCaches, true);
        } else {
            res = ErlideNoparse.reparse(b, scannerName);
        }
        if (Util.isOk(res)) {
            Bindings bindings = null;
            try {
                bindings = ErlUtils.match("{ok, {_, Forms, Comments}, _}", res);
            } catch (final TermParserException e) {
                e.printStackTrace();
            }
            if (bindings != null) {
                forms = (OtpErlangList) bindings.get("Forms");
                comments = (OtpErlangList) bindings.get("Comments");
            } else {
                ErlLogger.error("parser for %s got: %s", path, res);
            }
        } else {
            ErlLogger.error("rpc error when parsing %s: %s", path, res);
        }
        // mm.setParseTree(forms);
        if (forms == null) {
            module.setChildren(null);
        } else {
            final List<IErlElement> children = Lists
                    .newArrayListWithCapacity(forms.arity());
            for (final OtpErlangObject form : forms) {
                final IErlMember elem = create(module, (OtpErlangTuple) form);
                if (elem != null) {
                    children.add(elem);
                }
            }
            module.setChildren(children);
        }
        if (comments == null) {
            module.setComments(null);
        } else {
            final List<IErlComment> moduleComments = Lists
                    .newArrayListWithCapacity(comments.arity());
            for (final OtpErlangObject comment : comments) {
                final IErlComment c = createComment(module,
                        (OtpErlangTuple) comment);
                if (c != null) {
                    moduleComments.add(c);
                }
            }
            module.setComments(moduleComments);
        }
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
    private IErlComment createComment(final IErlModule module,
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
        final ErlComment comment = new ErlComment(module, Util.stringValue(s),
                line == 0 || line == 1);
        try {
            final int ofs = ((OtpErlangLong) c.elementAt(3)).intValue();
            final int len = ((OtpErlangLong) c.elementAt(4)).intValue();
            setPos(comment, line, lastLine, ofs + 1, len, true);
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
    private IErlMember create(final IErlModule module, final OtpErlangTuple el) {
        final OtpErlangAtom type = (OtpErlangAtom) el.elementAt(0);
        final String typeS = type.atomValue();
        if ("error".equals(typeS)) {
            final OtpErlangTuple er = (OtpErlangTuple) el.elementAt(1);

            final String msg = BackendHelper.format_error(BackendCore
                    .getBackendManager().getIdeBackend(), er);

            final ErlMessage e = new ErlMessage(module,
                    ErlMessage.MessageKind.ERROR, msg);
            setPos(e, er.elementAt(0), false);
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
            return addAttribute(module, pos, n, val, extra);
        } else if ("attribute".equals(typeS)) {
            final OtpErlangObject pos = el.elementAt(1);
            final OtpErlangAtom name = (OtpErlangAtom) el.elementAt(2);
            final OtpErlangObject val = el.elementAt(3);
            final OtpErlangObject extra = el.arity() > 4 ? el.elementAt(4)
                    : null;
            return addAttribute(module, pos, name, val, extra);
        } else if ("function".equals(typeS)) {
            final ErlFunction f = makeErlFunction(module, el);
            final OtpErlangList clauses = (OtpErlangList) el.elementAt(6);
            final List<ErlFunctionClause> cls = Lists
                    .newArrayListWithCapacity(clauses.arity());
            for (int i = 0; i < clauses.arity(); i++) {
                final OtpErlangTuple clause = (OtpErlangTuple) clauses
                        .elementAt(i);
                final ErlFunctionClause cl = makeErlFunctionClause(f, i, clause);
                cls.add(cl);
            }
            f.setChildren(cls);
            return f;
        } else {
            ErlLogger.debug("unknown: " + el);
        }
        return null;
    }

    /**
     * @param module
     *            module
     * @param el
     *            -record(function, {pos, name, arity, args, head, clauses,
     *            name_pos, comment, exported}).
     * @return ErlFunction
     */
    private ErlFunction makeErlFunction(final IErlModule module,
            final OtpErlangTuple el) {
        final OtpErlangTuple pos = (OtpErlangTuple) el.elementAt(1);
        final OtpErlangAtom name = (OtpErlangAtom) el.elementAt(2);
        final OtpErlangLong arity = (OtpErlangLong) el.elementAt(3);
        final OtpErlangList parameters = (OtpErlangList) el.elementAt(4);
        final OtpErlangObject head = el.elementAt(5);
        final OtpErlangTuple namePos = (OtpErlangTuple) el.elementAt(7);
        ErlFunction f = null;
        final OtpErlangObject commentO = el.elementAt(8);
        final OtpErlangAtom exportedA = (OtpErlangAtom) el.elementAt(9);
        final boolean exported = Boolean.parseBoolean(exportedA.atomValue());
        try {
            String comment = Util.stringValue(commentO);
            if (comment != null) {
                comment = comment.replaceAll("\n", "<br/>");
            }
            f = new ErlFunction(module, name.atomValue(), arity.intValue(),
                    Util.stringValue(head), comment, exported, parameters);
        } catch (final OtpErlangRangeException e) {
            return f;
        }
        setPos(f, pos, true);
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
        final OtpErlangList parameters = (OtpErlangList) clause.elementAt(3);
        final OtpErlangObject head = clause.elementAt(4);
        final OtpErlangTuple cnamePos = (OtpErlangTuple) clause.elementAt(5);
        final ErlFunctionClause cl = new ErlFunctionClause(f, "#" + i,
                Util.stringValue(head), parameters);
        try {
            setNamePos(cl, cnamePos);
        } catch (final OtpErlangRangeException e) {
            ErlLogger.warn(e);
        }
        setPos(cl, cpos, true);
        return cl;
    }

    private void setNamePos(final ErlMember f, final OtpErlangTuple namePos)
            throws OtpErlangRangeException {
        final OtpErlangTuple tpos1 = (OtpErlangTuple) namePos.elementAt(0);
        final int ofs = ((OtpErlangLong) tpos1.elementAt(1)).intValue();
        final int len = ((OtpErlangLong) namePos.elementAt(1)).intValue();
        f.setNameRange(ofs, len);
    }

    private IErlMember addAttribute(final IErlModule module,
            final OtpErlangObject pos, final OtpErlangAtom name,
            final OtpErlangObject val, final OtpErlangObject extra) {
        final String nameS = name.atomValue();
        if ("module".equals(nameS) && val instanceof OtpErlangAtom) {
            return addModuleAttribute(module, pos, (OtpErlangAtom) val, extra,
                    nameS);
        } else if ("import".equals(nameS)) {
            if (val instanceof OtpErlangTuple) {
                return addImportAttribute(module, pos, val);
            }
        } else if ("export".equals(nameS)) {
            return addExportAttribute(module, pos, val, extra);
        } else if ("record".equals(nameS)) {
            return addRecordDef(module, pos, val, extra);
        } else if ("type".equals(nameS) || "spec".equals(nameS)
                || "opaque".equals(nameS)) {
            return addTypespec(module, pos, extra);
        } else if ("define".equals(nameS)) {
            return addMacroDef(module, pos, val, extra, nameS);
        }
        return addOtherAttribute(module, pos, val, extra, nameS);
    }

    private IErlMember addExportAttribute(final IErlModule module,
            final OtpErlangObject pos, final OtpErlangObject val,
            final OtpErlangObject extra) {
        final OtpErlangList functionList = (OtpErlangList) val;
        final ErlExport ex = new ErlExport(module, functionList,
                Util.stringValue(extra));
        setPos(ex, pos, false);
        return ex;
    }

    private IErlMember addMacroDef(final IErlModule module,
            final OtpErlangObject pos, final OtpErlangObject val,
            final OtpErlangObject extra, final String nameS) {
        if (val instanceof OtpErlangAtom) {
            // final OtpErlangAtom o = (OtpErlangAtom) val;
            final String s = Util.stringValue(extra);
            // final ErlMacroDef r = new ErlMacroDef(parent, o.toString(),
            // s);
            final ErlMember r = new ErlMacroDef(module, s);
            setPos(r, pos, false);
            // r.setParseTree(val);
            return r;
        } else if (val instanceof OtpErlangList) {
            final OtpErlangList macroList = (OtpErlangList) val;
            if (macroList.elementAt(0) instanceof OtpErlangTuple) {
                final OtpErlangTuple macroNameTuple = (OtpErlangTuple) macroList
                        .elementAt(0);
                OtpErlangObject o = macroNameTuple.elementAt(2);
                if (o instanceof OtpErlangTuple) {
                    o = ((OtpErlangTuple) o).elementAt(2);
                }
                ErlMember r;
                if (o instanceof OtpErlangAtom) {
                    final String macroName = ((OtpErlangAtom) o).atomValue();
                    r = new ErlMacroDef(module, macroName, null);
                } else {
                    // what do we do here? the define isn't correct
                    // Erlang...
                    ErlLogger.warn("Strange macro definition in %s: %s",
                            module.getName(), o.toString());
                    r = new ErlMacroDef(module, o.toString(), null);
                }
                setPos(r, pos, false);
                // r.setParseTree(val);
                return r;
            }
        }
        return addOtherAttribute(module, pos, val, extra, nameS);
    }

    private IErlAttribute addOtherAttribute(final IErlModule module,
            final OtpErlangObject pos, final OtpErlangObject val,
            final OtpErlangObject extra, final String nameS) {
        // user-defined attribute? or maybe if else endif...
        // OtpErlangObject val1 = concreteTerm(val);
        // if (val instanceof OtpErlangList) {
        // final OtpErlangList list = (OtpErlangList) val;
        // if (list.arity() == 0) {
        // val1 = null;
        // }
        // }

        // final ErlAttribute a = new ErlAttribute(parent, nameS, val1, null);
        OtpErlangObject o = val;
        if (o instanceof OtpErlangAtom) {
            final OtpErlangAtom u = (OtpErlangAtom) o;
            if (u.atomValue().equals("u")) {
                o = null;
            }
        }
        final ErlAttribute a = new ErlAttribute(module, nameS, o,
                Util.stringValue(extra));
        setPos(a, pos, false);
        // a.setParseTree(val);
        return a;
    }

    private IErlRecordDef addRecordDef(final IErlModule module,
            final OtpErlangObject pos, final OtpErlangObject val,
            final OtpErlangObject extra) {
        if (val instanceof OtpErlangTuple) {
            final OtpErlangTuple recordTuple = (OtpErlangTuple) val;
            if (recordTuple.elementAt(0) instanceof OtpErlangAtom) {
                final String s = extra instanceof OtpErlangString ? ((OtpErlangString) extra)
                        .stringValue() : null;
                final OtpErlangList fields = (OtpErlangList) recordTuple
                        .elementAt(1);
                final ErlRecordDef r = new ErlRecordDef(module, s);
                setPos(r, pos, false);
                if (fields != null) {
                    final List<ErlRecordField> children = Lists
                            .newArrayListWithCapacity(fields.arity());
                    for (final OtpErlangObject o : fields) {
                        if (o instanceof OtpErlangTuple) {
                            final OtpErlangTuple fieldTuple = (OtpErlangTuple) o;
                            final OtpErlangAtom fieldNameAtom = (OtpErlangAtom) fieldTuple
                                    .elementAt(0);
                            final String fieldName = fieldNameAtom.atomValue();
                            final ErlRecordField field = new ErlRecordField(r,
                                    fieldName);
                            final OtpErlangTuple posTuple = (OtpErlangTuple) fieldTuple
                                    .elementAt(1);
                            if (fieldTuple.arity() > 2) {
                                final OtpErlangObject fieldExtra = fieldTuple
                                        .elementAt(2);
                                field.setExtra(Util.stringValue(fieldExtra));
                            }
                            setPos(field, posTuple, false);
                            children.add(field);
                        } else {
                            ErlLogger.error("bad record def: %s", o);
                        }
                    }
                    r.setChildren(children);
                } else {
                    r.setChildren(new ArrayList<IErlElement>());
                }
                return r;
            }
        }
        if (val instanceof OtpErlangAtom) {
            final String s = extra instanceof OtpErlangString ? ((OtpErlangString) extra)
                    .stringValue() : null;
            final ErlRecordDef r = new ErlRecordDef(module, s);
            setPos(r, pos, false);
            return r;
        }
        return null;
    }

    private IErlMember addTypespec(final IErlModule module,
            final OtpErlangObject pos, final OtpErlangObject extra) {
        final String s = Util.stringValue(extra);
        final int p = s.indexOf('(');
        final String typeName = p < 0 ? s : s.substring(0, p);
        final ErlTypespec a = new ErlTypespec(module, typeName, s);
        setPos(a, pos, false);
        return a;
    }

    private IErlImport addImportAttribute(final IErlModule module,
            final OtpErlangObject pos, final OtpErlangObject val) {
        final OtpErlangTuple t = (OtpErlangTuple) val;
        if (t.elementAt(0) instanceof OtpErlangAtom
                && t.elementAt(1) instanceof OtpErlangList) {
            final OtpErlangAtom importModule = (OtpErlangAtom) t.elementAt(0);
            final OtpErlangList functionList = (OtpErlangList) t.elementAt(1);
            final ErlImport imp = new ErlImport(module,
                    importModule.atomValue(), functionList);
            setPos(imp, pos, false);
            return imp;
        }
        return null;
    }

    private IErlAttribute addModuleAttribute(final IErlModule module,
            final OtpErlangObject pos, final OtpErlangAtom value,
            final OtpErlangObject extra, final String nameS) {
        final String s = Util.stringValue(extra);
        final ErlAttribute r = new ErlAttribute(module, nameS, value, s);
        setPos(r, pos, false);
        return r;
    }

    private boolean setPos(final SourceRefElement e, final OtpErlangObject pos,
            final boolean minusOne) {
        if (!(pos instanceof OtpErlangTuple)) {
            if (pos instanceof OtpErlangLong) {
                int ipos = 999999;
                try {
                    ipos = ((OtpErlangLong) pos).intValue();
                } catch (final OtpErlangRangeException e1) {
                }
                setPos(e, 0, 0, ipos, 0, false);
                return true;
            }
            ErlLogger.debug("!> expecting pos tuple, got " + pos);
            return false;
        }
        try {
            // pos=
            // {{Line, LastLine, Offset}, PosLength} or
            // {{Line, Offset}, PosLength}
            final OtpErlangTuple tpos = (OtpErlangTuple) pos;
            final OtpErlangTuple tpos1 = (OtpErlangTuple) tpos.elementAt(0);
            final OtpErlangLong lenL = (OtpErlangLong) tpos.elementAt(1);
            final OtpErlangLong lineL = (OtpErlangLong) tpos1.elementAt(0);
            final OtpErlangLong lastLineL = (OtpErlangLong) tpos1.elementAt(1);
            final int line = lineL.intValue();
            int lastLine = lastLineL.intValue();
            int ofs;
            if (tpos1.arity() > 2) {
                ofs = ((OtpErlangLong) tpos1.elementAt(2)).intValue();
            } else {
                ofs = lastLine;
                lastLine = line;
            }
            final int len = lenL.intValue();
            setPos(e, line, lastLine, ofs, len, minusOne);
            return true;
        } catch (final OtpErlangRangeException ex) {
            return false;
        }

    }

    private void setPos(final SourceRefElement e, final int line,
            final int lastLine, final int ofs, final int len,
            final boolean minusOne) {
        e.setSourceRangeOffset(ofs);
        e.setSourceRangeLength(len - (minusOne ? 1 : 0));
        e.setLineStart(line);
        e.setLineEnd(lastLine);
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
            return BackendHelper.concreteSyntax(BackendCore.getBackendManager()
                    .getIdeBackend(), val);
        } catch (final Exception e) {
            return val;
        }
    }

}
