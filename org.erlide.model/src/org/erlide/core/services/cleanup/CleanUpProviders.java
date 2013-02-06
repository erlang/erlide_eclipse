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

import org.eclipse.core.resources.IResource;

/**
 * <p>
 * The {@link CleanUpProviders} class provides {@link CleanUpProvider}s.
 * </p>
 * 
 * @author Alain O'Dea [alain dot odea at gmail dot com]
 */
public class CleanUpProviders {

    /**
     * <p>
     * Construct a {@link CleanUpProvider} appropriate for a particular
     * {@link IResource}.
     * </p>
     * 
     * @param resource
     *            {@link IResource} for the Erlang module to clean up
     * 
     * @return {@link CleanUpProvider} appropriate for the supplied
     *         {@link IResource}
     */
    public static CleanUpProvider createForIResource(final IResource resource) {
        return new ErlTidyCleanupProvider(resource);
    }

}
