/*******************************************************************************
 * Copyright (c) 2006 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.erlide.runtime.backend.internal.AbstractBackend;

public class BackendType {

	private String fName;

	private String fClass;

	private String fId;

	private IConfigurationElement fElement;

	public BackendType(String name, String id, String clss,
			IConfigurationElement element) {
		super();
		fElement = element;
		fName = name;
		fId = id;
		fClass = clss;
	}

	public String getName() {
		return fName;
	}

	public String getID() {
		return fId;
	}

	public String getClss() {
		return fClass;
	}

	public AbstractBackend create() {
		AbstractBackend backend = null;
		try {
			backend = (AbstractBackend) fElement
					.createExecutableExtension("class");
		} catch (final CoreException exception) {
			exception.printStackTrace();
		}
		return backend;
	}
}
