/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others. All rights reserved. This program and
 * the accompanying materials are made available under the terms of the Eclipse Public
 * License v1.0 which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend.debug;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public final class BackendEvalResult {

    private boolean ok;

    private OtpErlangObject value;

    private OtpErlangObject bindings;

    private OtpErlangObject errorReason;

    /**
     * @return Returns the errorReason.
     */
    public OtpErlangObject getErrorReason() {
        return errorReason;
    }

    /**
     * @return Returns the bindings.
     */
    public OtpErlangObject getBindings() {
        return bindings;
    }

    /**
     * @return Returns the ok.
     */
    public boolean isOk() {
        return ok;
    }

    /**
     * @return Returns the value.
     */
    public OtpErlangObject getValue() {
        return value;
    }

    public void setError(final String r) {
        setError(new OtpErlangString(r));
    }

    public void setError(final OtpErlangObject r) {
        ok = false;
        errorReason = r;
        value = null;
        bindings = null;
    }

    public void setValue(final OtpErlangObject r, final OtpErlangObject b) {
        ok = true;
        value = r;
        bindings = b;
        errorReason = null;
    }
}
