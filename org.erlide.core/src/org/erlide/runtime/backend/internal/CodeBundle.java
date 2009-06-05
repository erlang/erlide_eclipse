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
package org.erlide.runtime.backend.internal;

import org.erlide.core.erlang.util.ErlideUtil;
import org.osgi.framework.Bundle;

public class CodeBundle {

	private Bundle bundle;
	private String ebin;

	public CodeBundle(Bundle b, String ebin) {
		this.bundle = b;
		this.ebin = ebin;
	}

	public Bundle getBundle() {
		return bundle;
	}

	public String getEbinDir() {
		return ErlideUtil.getPath(ebin, bundle);
	}

}
