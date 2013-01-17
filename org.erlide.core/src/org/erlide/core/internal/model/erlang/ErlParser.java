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
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendHelper;
import org.erlide.backend.IBackend;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.internal.model.root.ErlMember;
import org.erlide.core.model.ErlModelException;
import org.erlide.core.model.erlang.IErlAttribute;
import org.erlide.core.model.erlang.IErlComment;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlImport;
import org.erlide.core.model.erlang.IErlMember;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlParser;
import org.erlide.core.model.erlang.IErlRecordDef;
import org.erlide.core.model.erlang.IErlTypespec;
import org.erlide.core.model.erlang.ISourceReference;
import org.erlide.core.model.root.IErlElement;
import org.erlide.utils.ErlLogger;
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

    private final class SourceOffsetComparator implements
            Comparator<ISourceReference> {
        @Override
        public int compare(final ISourceReference o1, final ISourceReference o2) {
            final int offset1 = o1.getSourceRange().getOffset();
            final int offset2 = o2.getSourceRange().getOffset();
            if (offset1 < offset2) {
                return -1;
            } else if (offset1 > offset2) {
                return 1;
            }
            return 0;
        }
    }

    private static final int FUNCTION_COMMENT_THRESHOLD = 3;
    private static final int MODULE_HEADER_COMMENT_THRESHOLD = 1;
    private final BackendHelper helper;

    public ErlParser() {
        helper = new BackendHelper();
    }

    @Override
    public boolean parse(final IErlModule module, final String scannerName,
            final boolean initialParse, final String path,
            final boolean updateSearchServer) {
        if (module == null) {
            return false;
        }
        OtpErlangList forms = null;
        OtpErlangList comments = null;
        OtpErlangTuple res = null;
        final IBackend backend = BackendCore.getBackendManager()
                .getIdeBackend();
        if (initialParse) {
            final String stateDir = ErlangPlugin.getDefault()
                    .getStateLocation().toString();
            res = ErlideNoparse.initialParse(backend, scannerName, path,
                    stateDir, updateSearchServer);
        } else {
            res = ErlideNoparse.reparse(backend, scannerName,
                    updateSearchServer);
        }
        if (Util.isOk(res)) {
            final OtpErlangTuple t = (OtpErlangTuple) res.elementAt(1);
            forms = (OtpErlangList) t.elementAt(1);
            comments = (OtpErlangList) t.elementAt(2);
        } else {
            ErlLogger.error("error when parsing %s: %s", path, res);
        }
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
        fixFunctionComments(module);
        return forms != null && comments != null;
    }

    /**
     * fix function documentation with heuristics: if a comment is within 3
     * lines before function, or a sequence of comment, -spec, comment, then
     * they should be added to function documentation
     * 
     * TODO: check that -spec is actually relevant to the function
     * 
     * @param module
     */
    private void fixFunctionComments(final IErlModule module) {
        // TODO rewrite in Erlang? would be so much less code...
        try {
            final Collection<IErlComment> comments = module.getComments();
            final List<IErlElement> children = module.getChildren();
            final List<IErlMember> all = Lists
                    .newArrayListWithCapacity(children.size() + comments.size());
            all.addAll(comments);
            for (final IErlElement element : children) {
                if (element instanceof IErlMember) {
                    all.add((IErlMember) element);
                }
            }
            Collections.sort(all, new SourceOffsetComparator());
            for (int i = 1; i < all.size(); i++) {
                checkForComment(all, i);
            }
        } catch (final ErlModelException e) {
            ErlLogger.warn(e);
        }
    }

    private void checkForComment(final List<IErlMember> all, final int i) {
        final IErlMember m = all.get(i);
        if (m instanceof IErlFunction) {
            final IErlFunction function = (IErlFunction) m;
            final LinkedList<IErlMember> comments = Lists.newLinkedList();
            int j = considerPrevious(i, all, comments);
            j = considerPrevious(j, all, comments);
            j = considerPrevious(j, all, comments);
            if (!comments.isEmpty()) {
                function.setComments(comments);
            }
        }
    }

    private int considerPrevious(final int i, final List<IErlMember> all,
            final LinkedList<IErlMember> comments) {
        final int i_1 = i - 1;
        if (i_1 > 0) {
            final IErlMember member = all.get(i);
            final IErlMember member_1 = all.get(i_1);
            if (member_1 instanceof IErlComment
                    || member_1 instanceof IErlTypespec) {
                if (member_1.getLineEnd() + FUNCTION_COMMENT_THRESHOLD >= member
                        .getLineStart()) {
                    comments.addFirst(member_1);
                }
            } else {
                return -1;
            }
        }
        return i_1;
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
            if (c.elementAt(7) instanceof OtpErlangLong) {
                final OtpErlangLong lastLineL = (OtpErlangLong) c.elementAt(7);
                lastLine = lastLineL.intValue();
            }
        } catch (final OtpErlangRangeException e1) {
            lastLine = line;
        }
        final ErlComment comment = new ErlComment(module, Util.stringValue(s),
                line <= MODULE_HEADER_COMMENT_THRESHOLD);
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
            final String msg = helper.format_error(er);
            final ErlParserProblem e = ErlParserProblem.newError(module, msg);
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
            return addAttribute(module, pos, n, val, extra, null);
        } else if ("attribute".equals(typeS)) {
            final OtpErlangObject pos = el.elementAt(1);
            final OtpErlangAtom name = (OtpErlangAtom) el.elementAt(2);
            final OtpErlangObject val = el.elementAt(3);
            final OtpErlangObject extra = el.arity() > 4 ? el.elementAt(4)
                    : null;
            final OtpErlangObject arity = el.arity() > 5 ? el.elementAt(5)
                    : null;
            return addAttribute(module, pos, name, val, extra, arity);
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
        final OtpErlangAtom exportedA = (OtpErlangAtom) el.elementAt(8);
        final boolean exported = Boolean.parseBoolean(exportedA.atomValue());
        try {
            f = new ErlFunction(module, name.atomValue(), arity.intValue(),
                    Util.stringValue(head), exported, parameters);
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
            final OtpErlangObject val, final OtpErlangObject extra,
            final OtpErlangObject arity) {
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
            return addTypespec(module, pos, arity, extra);
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
            final OtpErlangObject pos, final OtpErlangObject arityL,
            final OtpErlangObject extra) {
        final String s = Util.stringValue(extra);
        final int p = s.indexOf('(');
        final String typeName = p < 0 ? s : s.substring(0, p);
        final int arity = Util.getIntegerValue(arityL, -1);
        final ErlTypespec a = new ErlTypespec(module, typeName, arity, s);
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
            return helper.concreteSyntax(val);
        } catch (final Exception e) {
            return val;
        }
    }

}
