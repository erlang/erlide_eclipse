/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others. All rights reserved.
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors: IBM Corporation - initial API and implementation
 ******************************************************************************/
package org.erlide.core.erlang.util;

public interface ISuffixConstants {

	String EXTENSION_beam = "beam"; //$NON-NLS-1$

	String EXTENSION_BEAM = "BEAM"; //$NON-NLS-1$

	String EXTENSION_erl = "erl"; //$NON-NLS-1$

	String EXTENSION_ERL = "ERL"; //$NON-NLS-1$

	String SUFFIX_STRING_beam = "." + EXTENSION_beam; //$NON-NLS-1$

	String SUFFIX_STRING_BEAM = "." + EXTENSION_BEAM; //$NON-NLS-1$

	String SUFFIX_STRING_erl = "." + EXTENSION_erl; //$NON-NLS-1$

	String SUFFIX_STRING_ERL = "." + EXTENSION_ERL; //$NON-NLS-1$

	char[] SUFFIX_beam = SUFFIX_STRING_beam.toCharArray();

	char[] SUFFIX_BEAM = SUFFIX_STRING_BEAM.toCharArray();

	char[] SUFFIX_erl = SUFFIX_STRING_erl.toCharArray();

	char[] SUFFIX_ERL = SUFFIX_STRING_ERL.toCharArray();

}