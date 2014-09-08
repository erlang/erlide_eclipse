/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.engine.internal.model.erlang;

import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.IErlTypespec;
import org.erlide.engine.model.root.ErlElementKind;

/**
 *
 *
 * @author Vlad Dumitrescu
 */
public class ErlTypespec extends ErlMember implements IErlTypespec {

    private final String module;
    private final String fSource;
    private final int arity;

    /**
     * @param parent
     * @param name
     */
    public ErlTypespec(final IParent parent, final String module, final String name,
            final int arity, final String source) {
        super(parent, name);
        this.module = module;
        this.arity = arity;
        fSource = source;
    }

    /**
     * @see org.erlide.engine.model.root.IErlElement#getKind()
     */
    @Override
    public ErlElementKind getKind() {
        return ErlElementKind.TYPESPEC;
    }

    @Override
    public String toString() {
        if (fSource != null) {
            return fSource;
        }
        return getName() + "/" + getArity();
    }

    @Override
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

    @Override
    public String getModule() {
        return module;
    }
}
