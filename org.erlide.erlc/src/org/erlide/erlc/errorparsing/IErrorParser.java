/*******************************************************************************
 * Copyright (c) 2000, 2004 QNX Software Systems and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     QNX Software Systems - Initial API and implementation
 *******************************************************************************/

package org.erlide.erlc.errorparsing;

import org.eclipse.core.resources.IProject;
import org.erlide.erlc.core.ErrorParserManager;

public interface IErrorParser {

	/**
	 * Finds error or warnings on the given line
	 * 
	 * @param project
	 *            TODO
	 */
	boolean processLines(String lines, ErrorParserManager eoParser,
			IProject project);

}
