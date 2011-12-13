/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.selection.internal;

import org.eclipse.core.resources.IFile;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.IErlElement;

/**
 * Represents an Erlang module which is selected by the user
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class ErlModuleSelection extends AbstractErlSelection {
    protected IErlModule module;

    /**
     * Constructor
     * 
     * @param module
     *            Erlang module - Erlide representation
     * @param file
     *            file, which contains the module
     */
    public ErlModuleSelection(final IErlModule module, final IFile file) {
        this.module = module;
        this.file = file;
    }

    @Override
    public IErlElement getErlElement() {
        return module;
    }

    @Override
    public SelectionKind getDetailedKind() {
        return getKind();
    }

    @Override
    public SelectionKind getKind() {
        return SelectionKind.MODULE;
    }

    @Override
    public IErlModule getErlModule() {
        return module;
    }

}
