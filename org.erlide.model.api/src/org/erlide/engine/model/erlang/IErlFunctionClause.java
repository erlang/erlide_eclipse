/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.engine.model.erlang;

import java.util.List;

/**
 *
 * @author Vlad
 *
 * @noimplement This interface is not intended to be implemented by clients.
 *
 */
public interface IErlFunctionClause extends IErlMember {

    public String getHead();

    public String getFunctionName();

    public List<String> getParameters();

    int getArity();

}
