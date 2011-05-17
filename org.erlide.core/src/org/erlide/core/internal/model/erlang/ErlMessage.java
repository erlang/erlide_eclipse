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
import org.erlide.core.model.root.IParent;
import org.erlide.core.model.root.ISourceRange;

public class ErlMessage extends ErlMember implements IErlMessage {

    private final String message;
    private final MessageKind fKind;

    public ErlMessage(final IParent parent, final MessageKind kind,
            final String name) {
        super(parent, name);
        fKind = kind;
        message = name;
    }

    public Kind getKind() {
        return Kind.ERROR;
    }

    public String getMessage() {
        return message;
    }

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

    public MessageKind getMessageKind() {
        return fKind;
    }
}
