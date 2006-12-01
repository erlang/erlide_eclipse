/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/
package org.erlide.core.util;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class ErlidePrefConverter {

	/**
	 * Converts a list to a string that can be stored in the preferences.
	 * 
	 * @param objects
	 *            The objects to make use of
	 * @return the string rep
	 */
	public static String convert(List objects) {
		final Iterator iter = objects.iterator();

		final UStringBuffer buffer = new UStringBuffer();
		while (iter.hasNext()) {
			buffer.append(iter.next().toString());

			if (iter.hasNext()) {
				buffer.append(":");
			}
		}

		return buffer.toString();
	}

	/**
	 * Convert the string to the list of strings
	 * 
	 * @param obj
	 *            the string to convert
	 * @return the resulting list
	 */
	public static List<String> convertToList(String obj) {
		final char[] dta = obj.toCharArray();

		final List<String> lst = new LinkedList<String>();

		int last = 0;

		for (int i = 0; i < dta.length; i++) {
			if (dta[i] == ':') {
				lst.add(obj.substring(last, i));
				last = i + 1;
			}
		}

		lst.add(obj.substring(last));

		return lst;
	}

}
