/*******************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.editors.scratch;

/**
 * A listener interested in scratch evaluation state changes.
 */
public interface IScratchStateChangedListener {

	/**
	 * Informs about the changed scratch evaluation state
	 */
	void scratchStateChanged(ScratchEditor editor);
}
