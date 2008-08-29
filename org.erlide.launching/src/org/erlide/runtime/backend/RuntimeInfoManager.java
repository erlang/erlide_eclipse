/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.WeakHashMap;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.osgi.service.prefs.BackingStoreException;

public class RuntimeInfoManager {

	private static final String OLD_NAME = "erts";

	private static RuntimeInfoManager manager;
	private RuntimeInfo erlideRuntime;

	private RuntimeInfoManager() {
		load();
	}

	public static RuntimeInfoManager getDefault() {
		if (manager == null) {
			manager = new RuntimeInfoManager();
		}
		return manager;
	}

	protected final Map<String, RuntimeInfo> fRuntimes = new WeakHashMap<String, RuntimeInfo>();
	private String defaultRuntime = "";

	public Collection<RuntimeInfo> getRuntimes() {
		return new ArrayList<RuntimeInfo>(fRuntimes.values());
	}

	public void store() {
		IEclipsePreferences root = getRootPreferenceNode();
		String[] children;
		try {
			children = root.childrenNames();
			for (String name : children) {
				root.node(name).removeNode();
			}
			for (RuntimeInfo rt : fRuntimes.values()) {
				rt.store(root);
			}
			if (defaultRuntime != null) {
				root.put("default", defaultRuntime);
			}
			if (erlideRuntime != null) {
				root.put("erlide", erlideRuntime.getName());
			}
			try {
				root.flush();
			} catch (BackingStoreException e) {
			}
			root.flush();
		} catch (BackingStoreException e) {
			e.printStackTrace();
		}
	}

	public void load() {
		fRuntimes.clear();
		IEclipsePreferences root = getRootPreferenceNode();
		defaultRuntime = root.get("default", null);

		String[] children;
		try {
			children = root.childrenNames();
			for (String name : children) {
				RuntimeInfo rt = new RuntimeInfo();
				rt.load(root.node(name));
				fRuntimes.put(name, rt);
			}
			if (defaultRuntime == null && children.length > 0) {
				defaultRuntime = children[0];
			}
		} catch (BackingStoreException e) {
			e.printStackTrace();
		}

		IEclipsePreferences old = new InstanceScope()
				.getNode("org.erlide.basic/");
		String oldVal = old.get("otp_home", null);
		if (oldVal != null) {
			ErlLogger.debug("** converting old workspace Erlang settings");

			RuntimeInfo rt = new RuntimeInfo();
			rt.setOtpHome(oldVal);
			rt.setName(OLD_NAME);
			rt.setNodeName(rt.getName());
			addRuntime(rt);
			setDefaultRuntime(OLD_NAME);
			setErlideRuntime(getDefaultRuntime());
			old.remove("otp_home");
			try {
				old.flush();
			} catch (BackingStoreException e) {
				e.printStackTrace();
			}
			store();
		}

		setErlideRuntime(getRuntime(root.get("erlide", null)));
	}

	protected IEclipsePreferences getRootPreferenceNode() {
		return new InstanceScope().getNode(ErlangLaunchPlugin.PLUGIN_ID
				+ "/runtimes");
	}

	public void setRuntimes(Collection<RuntimeInfo> elements) {
		fRuntimes.clear();
		for (RuntimeInfo rt : elements) {
			fRuntimes.put(rt.getName(), rt);
		}
		store();
	}

	public void addRuntime(RuntimeInfo rt) {
		fRuntimes.put(rt.getName(), rt);
		store();
	}

	public Collection<String> getRuntimeNames() {
		return fRuntimes.keySet();
	}

	public boolean isDuplicateName(String name) {
		for (RuntimeInfo vm : fRuntimes.values()) {
			if (vm.getName().equals(name)) {
				return true;
			}
		}
		return false;
	}

	public RuntimeInfo getRuntime(String name) {
		final RuntimeInfo rt = fRuntimes.get(name);
		return rt;
	}

	public void removeRuntime(String name) {
		fRuntimes.remove(name);
	}

	public String getDefaultRuntimeName() {
		return this.defaultRuntime;
	}

	public void setDefaultRuntime(String selectedKey) {
		this.defaultRuntime = selectedKey;
	}

	public void setErlideRuntime(RuntimeInfo runtime) {
		if (runtime != null) {
			runtime.setNodeName("erlide");
		}
		RuntimeInfo old = this.erlideRuntime;
		if (old == null || !old.equals(runtime)) {
			this.erlideRuntime = runtime;
			// this creates infinite recursion!
			// BackendManager.getDefault().getIdeBackend().stop();
		}
	}

	public RuntimeInfo getErlideRuntime() {
		return this.erlideRuntime;
	}

	public RuntimeInfo getDefaultRuntime() {
		return getRuntime(getDefaultRuntimeName());
	}

}
