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

/**
 * Interface class for adding and removing warning messages
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public interface IWarningHandler {
    /**
     * Adds a warning message to the view.
     * 
     * @param message
     *            warning message
     */
    public void addMessage(String message);

    /**
     * Removes all warning messages
     */
    public void removeAll();

    /**
     * Refreshes the displayed warning messages.
     */
    public void refresh();

}
