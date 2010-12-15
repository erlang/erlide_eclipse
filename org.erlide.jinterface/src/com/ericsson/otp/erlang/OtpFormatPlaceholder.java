/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package com.ericsson.otp.erlang;

/**
 * Provides a Java representation of Erlang format placeholders.
 * <p>
 * <b>!!! These are to NOT to be sent to an Erlang node !!!!</b> Their use is in
 * formatting only.
 */
public class OtpFormatPlaceholder extends OtpErlangObject {

    private static final long serialVersionUID = -1L;

    private final String name;

    public OtpFormatPlaceholder(final String n) {
        name = n;
    }

    public String getName() {
        return name;
    }

    @Override
    public String toString() {
        return "~" + name;
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof OtpFormatPlaceholder)) {
            return false;
        }

        final OtpFormatPlaceholder l = (OtpFormatPlaceholder) o;
        return name.equals(l.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public void encode(final OtpOutputStream arg0) {
        // throw new NotImplementedException();
    }

}
