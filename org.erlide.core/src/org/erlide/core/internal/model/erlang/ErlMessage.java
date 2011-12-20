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
import org.erlide.core.model.erlang.IErlMessage;
import org.erlide.core.model.erlang.ISourceRange;
import org.erlide.core.model.root.IParent;

public class ErlMessage extends ErlMember implements IErlMessage {

    private final String message;
    private final MessageKind fKind;

    public ErlMessage(final IParent parent, final MessageKind kind,
            final String name) {
        super(parent, name);
        fKind = kind;
        message = name;
    }

    @Override
    public Kind getKind() {
        return Kind.ERROR;
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

    @Override
    public MessageKind getMessageKind() {
        return fKind;
    }
}
