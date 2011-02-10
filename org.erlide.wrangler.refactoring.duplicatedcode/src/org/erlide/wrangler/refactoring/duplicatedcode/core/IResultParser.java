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
package org.erlide.wrangler.refactoring.duplicatedcode.core;

import java.util.List;

import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * Interface for parsing duplicated RPC messages from Wrangler.
 * 
 * @author Gyorgy Orosz
 * 
 */
public interface IResultParser {
    /**
     * Indicates whether the RPC was successful
     * 
     * @return true if RPC is successful
     */
    public boolean isSuccessful();

    /**
     * Gets the error message if there's any.
     * 
     * @return error message string
     */
    public String getErrorMessage();

    /**
     * Parses the RPC
     * 
     * @param object
     *            rpc result
     */
    public void parse(OtpErlangObject object);

    /**
     * Returns with the duplicates
     * 
     * @return list of duplicates
     */
    public List<DuplicatedCodeElement> getDuplicates();
}
