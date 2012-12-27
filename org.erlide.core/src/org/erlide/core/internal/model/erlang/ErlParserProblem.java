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

import org.erlide.core.internal.model.root.ErlMember;
import org.erlide.core.model.IParent;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlParserProblem;
import org.erlide.core.model.erlang.ISourceRange;

public class ErlParserProblem extends ErlMember implements IErlParserProblem {

    enum ProblemKind {
        INFO, WARNING, ERROR
    }

    private final String message;
    private final ProblemKind fKind;

    ErlParserProblem(final IParent parent, final ProblemKind kind,
            final String name) {
        super(parent, name);
        fKind = kind;
        message = name;
    }

    @Override
    public Kind getKind() {
        return Kind.PROBLEM;
    }

    public ProblemKind getProblemKind() {
        return fKind;
    }

    @Override
    public String getMessage() {
        return message;
    }

    @Override
    public String getData() {
        return null;
    }

    // @Override
    // public OtpErlangObject getParseTree() {
    // return null;
    // }

    @Override
    public ISourceRange getNameRange() {
        return null;
    }

    @Override
    public String toString() {
        return "ERR: " + getMessage();
    }

    public static ErlParserProblem newError(final IErlModule module,
            final String msg) {
        return new ErlParserProblem(module, ProblemKind.ERROR, msg);
    }

    public static ErlParserProblem newWarning(final IErlModule module,
            final String msg) {
        return new ErlParserProblem(module, ProblemKind.WARNING, msg);
    }

    public static ErlParserProblem newInfo(final IErlModule module,
            final String msg) {
        return new ErlParserProblem(module, ProblemKind.INFO, msg);
    }
}
