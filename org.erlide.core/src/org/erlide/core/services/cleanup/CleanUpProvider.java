/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Alain O'Dea
 *******************************************************************************/
package org.erlide.core.services.cleanup;

/**
 * <p>
 * The {@link CleanUpProvider} interface defines the responsibilities of an
 * implementation of Erlang code clean up.
 * </p>
 * 
 * @author Alain O'Dea [alain dot odea at gmail dot com]
 */
public interface CleanUpProvider {

    /**
     * <p>
     * Perform clean up.
     * </p>
     */
    public void cleanUp() throws Exception;

}
