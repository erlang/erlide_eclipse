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
package org.erlide.launch.debug.model;

import org.eclipse.debug.core.model.DebugElement;
import org.eclipse.debug.core.model.IDebugTarget;
import org.erlide.launch.debug.ErlDebugConstants;
import org.erlide.launch.debug.IErlangDebugNode;

public class ErlangDebugElement extends DebugElement {

    public ErlangDebugElement(final IDebugTarget target) {
        super(target);
    }

    @Override
    public String getModelIdentifier() {
        return ErlDebugConstants.ID_ERLANG_DEBUG_MODEL;
    }

    public ErlangDebugTarget getErlangDebugTarget() {
        final IErlangDebugNode edn = (IErlangDebugNode) getDebugTarget();
        return edn.getErlangDebugTarget();
    }

    // @SuppressWarnings("unchecked")
    // @Override
    // public Object getAdapter(Class adapter) {
    // if (adapter == IDebugElement.class) {
    // return this;
    // }
    //
    // return super.getAdapter(adapter);
    // }

}
