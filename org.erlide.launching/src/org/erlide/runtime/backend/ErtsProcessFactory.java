/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/

package org.erlide.runtime.backend;

import java.util.Map;

import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.IProcessFactory;
import org.eclipse.debug.core.model.IProcess;

public class ErtsProcessFactory implements IProcessFactory {

	public static final String ID = "org.erlide.core.backend.ertsprocessfactory";

	public IProcess newProcess(ILaunch launch, Process process, String label,
			Map attributes) {
		// ErlLogger.log("#* ProcFact: " + label + " " + attributes);

		return new ErtsProcess(launch, process, label, attributes);
	}

}
