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
import org.erlide.core.model.erlang.IErlTypespec;
import org.erlide.core.model.root.IParent;

/**
 * 
 * 
 * @author Vlad Dumitrescu
 */
public class ErlTypespec extends ErlMember implements IErlTypespec {

    private final String fExtra;

    /**
     * @param parent
     * @param name
     */
    public ErlTypespec(final IParent parent, final String name,
            final String extra) {
        super(parent, name);
        fExtra = extra;
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
        if (fExtra != null) {
            return fExtra;
        }
        return getName();
    }
}
