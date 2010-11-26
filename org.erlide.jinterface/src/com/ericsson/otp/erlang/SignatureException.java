/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package com.ericsson.otp.erlang;

public class SignatureException extends Exception {

    private static final long serialVersionUID = -5143914414424005061L;

    public SignatureException(final Exception e) {
        super(e);
    }

    public SignatureException(final String string) {
        super(string);
    }

}
