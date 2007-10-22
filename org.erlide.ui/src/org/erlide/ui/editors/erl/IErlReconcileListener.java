/*******************************************************************************
 * Copyright (c) 2007 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import org.eclipse.jface.text.DocumentEvent;

public interface IErlReconcileListener {

	/**
	 * The manipulation described by the document event will be performed.
	 * 
	 * @param event
	 *            the document event describing the document change
	 */
	void modelAboutToBeReconciled(DocumentEvent event);

	/**
	 * The manipulation described by the document event has been performed.
	 * 
	 * @param event
	 *            the document event describing the document change
	 */
	void modelReconciled(DocumentEvent event);

}
