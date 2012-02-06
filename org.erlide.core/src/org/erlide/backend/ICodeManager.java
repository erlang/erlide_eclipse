/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.backend;

import org.osgi.framework.Bundle;

public interface ICodeManager {

    void addPath(final boolean usePathZ, final String path);

    void removePath(final String path);

    void reRegisterBundles();

    void register(final ICodeBundle b);

    void unregister(final Bundle b);

}
