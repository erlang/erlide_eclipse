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
package org.erlide.wrangler.refactoring.ui.warning;

import java.util.Date;

/**
 * Data type for representing the corresponding data for warning messages
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class WarningMessage {
    protected Date timestamp;
    protected String message;

    /**
     * Constructor
     * 
     * @param timestamp
     *            time when the warning message was got
     * @param message
     *            warning message
     */
    public WarningMessage(final Date timestamp, final String message) {
        this.timestamp = timestamp;
        this.message = message;
    }

    /**
     * Constructor
     * 
     * @param message
     *            warning message
     */
    public WarningMessage(final String message) {
        this(new Date(), message);
    }

    /**
     * @return the timestamp
     */
    public Date getTimestamp() {
        return timestamp;
    }

    /**
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * @param timestamp
     *            the timestamp to set
     */
    public void setTimestamp(final Date timestamp) {
        this.timestamp = timestamp;
    }

    /**
     * @param message
     *            the message to set
     */
    public void setMessage(final String message) {
        this.message = message;
    }

    // /**
    // * Returns the elements in an array
    // *
    // * @return elements array
    // */
    // public Object[] toArray() {
    // Object[] ret = new Object[2];
    // ret[0] = timestamp;
    // ret[1] = message;
    // return ret;
    // }
}
