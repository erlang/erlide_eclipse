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
package org.erlide.wrangler.refactoring.duplicatedcode.ui.elements;

public class DuplicatedCodeElement extends AbstractResultTreeParent {

	String codePartId;
	DuplicatedCodeInstanceElement defaultCodePart;

	public DuplicatedCodeElement(DuplicatedCodeInstanceElement defaultInstance) {
		this.defaultCodePart = defaultInstance;
	}

	@Override
	public String getName() {
		return "\"" + defaultCodePart.getCodePartString() + "\"";
	}

	public DuplicatedCodeInstanceElement getCodePart() {
		return defaultCodePart;
	}
}
