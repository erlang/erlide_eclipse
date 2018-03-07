/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.engine.model.erlang;

import java.util.Collection;
import java.util.List;

import org.erlide.engine.model.IParent;

/**
 *
 *
 * @author Vlad Dumitrescu
 */
public interface IErlFunction extends IErlFunctionClause, IParent {
    boolean isExported();

    List<IErlFunctionClause> getClauses();

    ErlangFunction getFunction();

    String getNameWithArity();

    /**
     * @return the function name with _ for each parameter, used for completion
     */
    String getNameWithParameters();

    Collection<IErlComment> getComments();

    void setComments(Collection<IErlComment> comments);

    void setTypespec(IErlTypespec spec);

    IErlTypespec getTypespec();

}
