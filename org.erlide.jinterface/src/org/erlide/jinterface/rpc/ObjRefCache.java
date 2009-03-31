/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface.rpc;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import com.ericsson.otp.erlang.OtpErlangRef;

public class ObjRefCache {

	private static Map<OtpErlangRef, WeakReference<Object>> objects = new HashMap<OtpErlangRef, WeakReference<Object>>();
	private static int refid0 = 0;
	private static int refid1 = 0;
	private static int refid2 = 0;

	public static OtpErlangRef registerTarget(Object obj) {
		if (obj == null) {
			return new OtpErlangRef(JRpcUtil.REF_NODE, new int[] { 0, 0, 0 }, 0);
		}

		Set<Entry<OtpErlangRef, WeakReference<Object>>> entries = objects
				.entrySet();
		for (Entry<OtpErlangRef, WeakReference<Object>> entry : entries) {
			if (entry.getValue().get() == obj) {
				return entry.getKey();
			}
		}
		OtpErlangRef ref = mkref();
		objects.put(ref, new WeakReference<Object>(obj));
		return ref;
	}

	public static Object getTarget(OtpErlangRef ref) {
		return objects.get(ref).get();
	}

	public static void unregisterTarget(OtpErlangRef ref) {
		objects.remove(ref);
	}

	static OtpErlangRef mkref() {
		final int max = 0x7fffffff;
		if (refid2 < max) {
			refid2++;
		} else {
			if (refid1 < max) {
				refid1++;
			} else {
				refid0++;
				refid1 = 0;
			}
			refid2 = 0;
		}
		return new OtpErlangRef(JRpcUtil.REF_NODE, new int[] { refid0, refid1,
				refid2 }, 0);
	}

}
