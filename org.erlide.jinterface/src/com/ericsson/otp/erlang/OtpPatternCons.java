/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package com.ericsson.otp.erlang;

/**
 * Provides a Java representation of a cons cell (works for both lists and
 * tuples!). A list or tuple pattern can end with " | Variable", where the
 * variable is matched against the remaining elements.
 * <p>
 * <b>!!! These are to NOT to be sent to an Erlang node !!!!</b> Their use is in
 * formatting only.
 */
public class OtpPatternCons extends OtpErlangObject {

    private static final long serialVersionUID = -1L;

    public OtpPatternCons() {
    }

    @Override
    public String toString() {
        return "|";
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof OtpPatternCons)) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        return "|".hashCode();
    }

    @Override
    public void encode(final OtpOutputStream arg0) {
        // throw new NotImplementedException();
    }

}
