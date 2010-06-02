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
package org.erlide.wrangler.refactoring.duplicatedcode.core;

import java.util.List;

import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface IResultParser {
	public boolean isSuccessful();

	public String getErrorMessage();

	public void parse(OtpErlangObject object);

	public List<DuplicatedCodeElement> getDuplicates();
}
