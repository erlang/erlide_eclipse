/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import org.eclipse.jface.text.IDocument;

public interface IErlangPartitions {

    String ERLANG_PARTITIONING = "___erlang_partitioning"; //$NON-NLS-1$

    String[] LEGAL_PARTITIONS = new String[] { IDocument.DEFAULT_CONTENT_TYPE };

}
