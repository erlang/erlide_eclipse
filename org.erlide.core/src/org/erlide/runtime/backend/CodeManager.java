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
package org.erlide.runtime.backend;

import org.osgi.framework.Bundle;

public interface CodeManager {

	void register(final Bundle p);

	void unregister(final Bundle p);

	void addPath(final boolean usePathZ, final String path);

	void removePath(final String path);

	void registerBundles();

}