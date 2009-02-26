/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.gunit.internal.launcher;

import java.util.Comparator;

public class ContainerComparator implements Comparator {

	public int compare(Object o1, Object o2) {
		String container1 = (String) o1;
		String container2 = (String) o2;
		if (container1 == null)
			container1 = ""; //$NON-NLS-1$
		if (container2 == null)
			container2 = ""; //$NON-NLS-1$
		return container1.compareTo(container2);
	}
}
