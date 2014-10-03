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
package org.erlide.engine.internal.services.parsing;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.model.erlang.ErlAttribute;
import org.erlide.engine.internal.model.erlang.ErlComment;
import org.erlide.engine.internal.model.erlang.ErlExport;
import org.erlide.engine.internal.model.erlang.ErlFunction;
import org.erlide.engine.internal.model.erlang.ErlFunctionClause;
import org.erlide.engine.internal.model.erlang.ErlImport;
import org.erlide.engine.internal.model.erlang.ErlMacroDef;
import org.erlide.engine.internal.model.erlang.ErlMember;
import org.erlide.engine.internal.model.erlang.ErlRecordDef;
import org.erlide.engine.internal.model.erlang.ErlRecordField;
import org.erlide.engine.internal.model.erlang.ErlTypespec;
import org.erlide.engine.internal.model.erlang.SourceRefElement;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.IErlAttribute;
import org.erlide.engine.model.erlang.IErlComment;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlImport;
import org.erlide.engine.model.erlang.IErlMember;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.IErlRecordDef;
import org.erlide.engine.model.erlang.IErlTypespec;
import org.erlide.engine.model.erlang.ISourceReference;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.services.parsing.ParserService;
import org.erlide.engine.services.parsing.RuntimeHelper;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

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
public final class ErlParser implements ParserService {

    private static final class SourceOffsetComparator implements
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
    private static final boolean TRACE = false;

    private final RuntimeHelper helper;
    private final IOtpRpc backend;

    public ErlParser(final IOtpRpc backend) {
        this.backend = backend;
        helper = new RuntimeHelper(backend);
    }

    @Override
    public boolean parse(final IErlModule module, final String scannerName,
            final boolean initialParse, final String path, final String initialText,
            final boolean updateSearchServer) {
        if (module == null) {
            return false;
        }
        OtpErlangList forms = null;
        OtpErlangList comments = null;
        OtpErlangTuple res = null;
        if (initialParse) {
            final String stateDir = ErlangEngine.getInstance().getStateDir();
            final String pathNotNull = path == null ? "" : path;
            res = ErlideNoparse.initialParse(backend, scannerName, pathNotNull,
                    initialText, stateDir, updateSearchServer);
        } else {
            res = ErlideNoparse.reparse(backend, scannerName, updateSearchServer);
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
            final List<IErlElement> children = createForms(module, forms);
            module.setChildren(children);
        }
        if (comments == null) {
            module.setComments(null);
        } else {
            final List<IErlComment> moduleComments = createComments(module, comments);
            module.setComments(moduleComments);
        }
        attachFunctionComments(module);
        String cached = "reparsed";
        if (res != null && res.arity() > 2) {
            final OtpErlangObject res2 = res.elementAt(2);
            if (res2 instanceof OtpErlangAtom) {
                final OtpErlangAtom atom = (OtpErlangAtom) res2;
                cached = atom.atomValue();
            }
        }
        if (TRACE) {
            ErlLogger.debug("Parsed %d forms and %d comments (%s)",
                    forms != null ? forms.arity() : 0,
                    comments != null ? comments.arity() : 0, cached);
        }
        return forms != null && comments != null;
    }

    private List<IErlComment> createComments(final IErlModule module,
            final OtpErlangList comments) {
        final List<IErlComment> moduleComments = Lists.newArrayListWithCapacity(comments
                .arity());
        for (final OtpErlangObject comment : comments) {
            final IErlComment c = createComment(module, (OtpErlangTuple) comment);
            if (c != null) {
                moduleComments.add(c);
            }
        }
        return moduleComments;
    }

    private List<IErlElement> createForms(final IErlModule module,
            final OtpErlangList forms) {
        final List<IErlElement> children = Lists.newArrayListWithCapacity(forms.arity());
        for (final OtpErlangObject form : forms) {
            final IErlMember elem = create(module, (OtpErlangTuple) form);
            if (elem != null) {
                children.add(elem);
            }
        }
        return children;
    }

    /**
     * attach local function documentation with heuristics: if a comment is
     * within 3 lines before function, or a sequence of comment, -spec, comment,
     * then they should be added to function documentation
     *
     * If any typespec is available for the function (wherever it is located),
     * then it should be attached too.
     *
     * @param module
     */
    private void attachFunctionComments(final IErlModule module) {
        // TODO rewrite in Erlang? would be so much less code...
        try {
            final Collection<IErlComment> comments = module.getComments();
            final List<IErlElement> children = module.getChildren();
            final List<IErlMember> all = Lists.newArrayListWithCapacity(children.size()
                    + comments.size());
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
            final LinkedList<IErlComment> comments = Lists.newLinkedList();
            int j = considerPrevious(i, all, comments, function);
            j = considerPrevious(j, all, comments, function);
            j = considerPrevious(j, all, comments, function);
            if (!comments.isEmpty()) {
                function.setComments(comments);
            }
        }
    }

    private int considerPrevious(final int i, final List<IErlMember> all,
            final LinkedList<IErlComment> comments, final IErlFunction function) {
        final int j = i - 1;
        if (j > 0) {
            final IErlMember member = all.get(i);
            final IErlMember prevMember = all.get(j);
            if (prevMember instanceof IErlComment) {
                if (prevMember.getLineEnd() + FUNCTION_COMMENT_THRESHOLD >= member
                        .getLineStart()) {
                    comments.addFirst((IErlComment) prevMember);
                }
            } else if (prevMember instanceof IErlTypespec) {
                final IErlTypespec spec = (IErlTypespec) prevMember;

                if (spec.getName().equals(function.getName())
                        && spec.getArity() == function.getArity()
                        && prevMember.getLineEnd() + FUNCTION_COMMENT_THRESHOLD >= member
                                .getLineStart()) {
                    function.setTypespec(spec);
                }
            } else {
                return -1;
            }
        }
        return j;
    }

    // -record(token, {kind, line, offset, length, value, text,
    // last_line, column}).
    // indexes of the token fields
    final static int KIND = 1;
    final static int LINE = 2;
    final static int OFFSET = 3;
    final static int LENGTH = 4;
    final static int VALUE = 5;
    final static int TEXT = 6;
    final static int LAST_LINE = 7;
    final static int COLUMN = 8;

    /**
     * create an IErlComment from a token record
     *
     * @param IErlModule
     *            module containing comment
     * @param OtpErlangTuple
     *            token record from noparse
     * @return IErlComment
     */
    private IErlComment createComment(final IErlModule module, final OtpErlangTuple c) {
        final OtpErlangLong lineL = (OtpErlangLong) c.elementAt(LINE);
        final OtpErlangObject s = c.elementAt(TEXT);

        int line;
        int lastLine;
        try {
            line = lineL.intValue();
        } catch (final OtpErlangRangeException x) {
            line = 0;
        }
        lastLine = line;
        try {
            if (c.elementAt(LAST_LINE) instanceof OtpErlangLong) {
                final OtpErlangLong lastLineL = (OtpErlangLong) c.elementAt(LAST_LINE);
                lastLine = lastLineL.intValue();
            }
        } catch (final OtpErlangRangeException e1) {
            lastLine = line;
        }
        final ErlComment comment = new ErlComment(module, Util.stringValue(s),
                line <= MODULE_HEADER_COMMENT_THRESHOLD);
        try {
            final int ofs = ((OtpErlangLong) c.elementAt(OFFSET)).intValue();
            final int len = ((OtpErlangLong) c.elementAt(LENGTH)).intValue();
            setPos(comment, line, lastLine, ofs + 1, len);
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
            final String msg = helper.formatError(er);
            final ErlParserProblem e = ErlParserProblem.newError(module, msg);
            setPos(e, er.elementAt(0));
            return e;
        } else if ("tree".equals(typeS)) {
            final OtpErlangTuple atr = (OtpErlangTuple) el.elementAt(3);
            final OtpErlangObject pos = ((OtpErlangTuple) el.elementAt(2)).elementAt(1);
            final OtpErlangTuple name = (OtpErlangTuple) atr.elementAt(1);
            final OtpErlangAtom n = (OtpErlangAtom) concreteTerm(name);
            final OtpErlangObject val = atr.elementAt(2);
            final OtpErlangObject extra = el.arity() > 4 ? el.elementAt(4) : null;
            return addAttribute(module, pos, n, val, extra, null);
        } else if ("attribute".equals(typeS)) {
            final OtpErlangObject pos = el.elementAt(1);
            final OtpErlangAtom name = (OtpErlangAtom) el.elementAt(2);
            final OtpErlangObject val = el.elementAt(3);
            final OtpErlangObject extra = el.arity() > 4 ? el.elementAt(4) : null;
            final OtpErlangObject arity = el.arity() > 5 ? el.elementAt(5) : null;
            return addAttribute(module, pos, name, val, extra, arity);
        } else if ("function".equals(typeS)) {
            final ErlFunction f = makeErlFunction(module, el);
            final OtpErlangList clauses = (OtpErlangList) el.elementAt(6);
            final List<ErlFunctionClause> cls = Lists.newArrayListWithCapacity(clauses
                    .arity());
            for (int i = 0; i < clauses.arity(); i++) {
                final OtpErlangTuple clause = (OtpErlangTuple) clauses.elementAt(i);
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
    private ErlFunction makeErlFunction(final IErlModule module, final OtpErlangTuple el) {
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
    private ErlFunctionClause makeErlFunctionClause(final ErlFunction f, final int i,
            final OtpErlangTuple clause) {
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
        setPos(cl, cpos);
        return cl;
    }

    private void setNamePos(final ErlMember f, final OtpErlangTuple namePos)
            throws OtpErlangRangeException {
        final OtpErlangTuple tpos1 = (OtpErlangTuple) namePos.elementAt(0);
        final int ofs = ((OtpErlangLong) tpos1.elementAt(1)).intValue();
        final int len = ((OtpErlangLong) namePos.elementAt(1)).intValue();
        f.setNameRange(ofs, len);
    }

    private IErlMember addAttribute(final IErlModule module, final OtpErlangObject pos,
            final OtpErlangAtom name, final OtpErlangObject val,
            final OtpErlangObject extra, final OtpErlangObject arity) {
        final String nameS = name.atomValue();
        if ("module".equals(nameS) && val instanceof OtpErlangAtom) {
            return addModuleAttribute(module, pos, (OtpErlangAtom) val, extra, nameS);
        } else if ("import".equals(nameS)) {
            if (val instanceof OtpErlangTuple) {
                return addImportAttribute(module, pos, val);
            }
        } else if ("export".equals(nameS)) {
            return addExportAttribute(module, pos, val, extra);
        } else if ("record".equals(nameS)) {
            return addRecordDef(module, pos, val, extra);
        } else if ("type".equals(nameS) || "spec".equals(nameS) || "opaque".equals(nameS)) {
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
        final ErlExport ex = new ErlExport(module, functionList, Util.stringValue(extra));
        setPos(ex, pos);
        return ex;
    }

    private IErlMember addMacroDef(final IErlModule module, final OtpErlangObject pos,
            final OtpErlangObject val, final OtpErlangObject extra, final String nameS) {
        if (val instanceof OtpErlangAtom) {
            final String s = Util.stringValue(extra);
            final ErlMember r = new ErlMacroDef(module,
                    ((OtpErlangAtom) val).atomValue(), s);
            setPos(r, pos);
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
                setPos(r, pos);
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
            if ("u".equals(u.atomValue())) {
                o = null;
            }
        }
        final ErlAttribute a = new ErlAttribute(module, nameS, o, Util.stringValue(extra));
        setPos(a, pos);
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
                final OtpErlangList fields = (OtpErlangList) recordTuple.elementAt(1);
                final ErlRecordDef r = new ErlRecordDef(module, null, s);
                setPos(r, pos);
                if (fields != null) {
                    final List<ErlRecordField> children = Lists
                            .newArrayListWithCapacity(fields.arity());
                    for (final OtpErlangObject o : fields) {
                        if (o instanceof OtpErlangTuple) {
                            final OtpErlangTuple fieldTuple = (OtpErlangTuple) o;
                            final OtpErlangAtom fieldNameAtom = (OtpErlangAtom) fieldTuple
                                    .elementAt(0);
                            final String fieldName = fieldNameAtom.atomValue();
                            final ErlRecordField field = new ErlRecordField(r, fieldName);
                            final OtpErlangTuple posTuple = (OtpErlangTuple) fieldTuple
                                    .elementAt(1);
                            if (fieldTuple.arity() > 2) {
                                final OtpErlangObject fieldExtra = fieldTuple
                                        .elementAt(2);
                                field.setExtra(Util.stringValue(fieldExtra));
                            }
                            setPos(field, posTuple);
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
            final ErlRecordDef r = new ErlRecordDef(module, null, s);
            setPos(r, pos);
            return r;
        }
        return null;
    }

    private IErlMember addTypespec(final IErlModule module, final OtpErlangObject pos,
            final OtpErlangObject extra) {
        final String s = Util.stringValue(extra);
        final int p = s.indexOf('(');
        final String ref = p < 0 ? s : s.substring(0, p);
        final int arity = getTypeArity(s);
        final String[] refs = ref.split(":");
        String moduleName;
        String typeName;
        if (refs.length == 1) {
            moduleName = module.getName();
            typeName = refs[0];
        } else {
            moduleName = refs[0];
            typeName = refs[1];
        }
        final ErlTypespec a = new ErlTypespec(module, moduleName, typeName, arity, s);
        setPos(a, pos);
        return a;
    }

    private int getTypeArity(final String s) {
        final int p = s.indexOf('(');
        int paras = 1;
        int result = 0;
        for (int i = p + 1; i < s.length(); i++) {
            final char crt = s.charAt(i);
            if (crt == '(') {
                paras += 1;
            }
            if (crt == ')') {
                paras -= 1;
            }
            if (paras == 0) {
                break;
            }
            if (crt == ',' && paras == 1) {
                result += 1;
            }
            if (result == 0 && crt != ')') {
                result += 1;
            }
        }
        return result;
    }

    private IErlImport addImportAttribute(final IErlModule module,
            final OtpErlangObject pos, final OtpErlangObject val) {
        final OtpErlangTuple t = (OtpErlangTuple) val;
        if (t.elementAt(0) instanceof OtpErlangAtom
                && t.elementAt(1) instanceof OtpErlangList) {
            final OtpErlangAtom importModule = (OtpErlangAtom) t.elementAt(0);
            final OtpErlangList functionList = (OtpErlangList) t.elementAt(1);
            final ErlImport imp = new ErlImport(module, importModule.atomValue(),
                    functionList);
            setPos(imp, pos);
            return imp;
        }
        return null;
    }

    private IErlAttribute addModuleAttribute(final IErlModule module,
            final OtpErlangObject pos, final OtpErlangAtom value,
            final OtpErlangObject extra, final String nameS) {
        final String s = Util.stringValue(extra);
        final ErlAttribute r = new ErlAttribute(module, nameS, value, s);
        setPos(r, pos);
        return r;
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
            setPos(e, line, lastLine, ofs, len);
            return true;
        } catch (final OtpErlangRangeException ex) {
            return false;
        }

    }

    private void setPos(final SourceRefElement e, final int line, final int lastLine,
            final int ofs, final int len) {
        e.setSourceRangeOffset(ofs);
        e.setSourceRangeLength(len);
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
