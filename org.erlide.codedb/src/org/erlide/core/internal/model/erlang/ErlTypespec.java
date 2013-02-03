/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.internal.model.erlang;

import org.erlide.core.internal.model.root.ErlMember;
import org.erlide.core.model.ErlModelException;
import org.erlide.core.model.IParent;
import org.erlide.core.model.erlang.IErlTypespec;

/**
 * 
 * 
 * @author Vlad Dumitrescu
 */
public class ErlTypespec extends ErlMember implements IErlTypespec {

    private final String fSource;
    private final int arity;

    /**
     * @param parent
     * @param name
     */
    public ErlTypespec(final IParent parent, final String name,
            final int arity, final String source) {
        super(parent, name);
        this.arity = arity;
        fSource = source;
    }

    /**
     * @see org.erlide.core.model.root.IErlElement#getKind()
     */
    @Override
    public Kind getKind() {
        return Kind.TYPESPEC;
    }

    @Override
    public String toString() {
        if (fSource != null) {
            return fSource;
        }
        return getName() + "/" + getArity();
    }

    public int getArity() {
        return arity;
    }

    @Override
    public String getSource() throws ErlModelException {
        if (fSource != null) {
            return fSource;
        }
        return super.getSource();
    }
}
