/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface.backend;

public class NoBackendException extends BackendException {

    private static final long serialVersionUID = 1L;

    public NoBackendException(final Exception e) {
        super("The Erlang backend is not running. Functionality is limited.", e);
    }

}
