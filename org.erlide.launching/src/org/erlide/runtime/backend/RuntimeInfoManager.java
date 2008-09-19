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
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.erlide.runtime.PreferencesUtils;
import org.osgi.service.prefs.BackingStoreException;

public class RuntimeInfoManager implements IPreferenceChangeListener {

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
	private String defaultRuntimeName = "";

	private List<RuntimeInfoListener> fListeners = new ArrayList<RuntimeInfoListener>();

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
			if (defaultRuntimeName != null) {
				root.put("default", defaultRuntimeName);
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

	public synchronized void load() {
		fRuntimes.clear();

		IEclipsePreferences root = getRootPreferenceNode();
		defaultRuntimeName = root.get("default", null);

		String[] children;
		try {
			children = root.childrenNames();
			for (String name : children) {
				RuntimeInfo rt = new RuntimeInfo();
				rt.load(root.node(name));
				fRuntimes.put(name, rt);
			}
			if (defaultRuntimeName == null && children.length > 0) {
				defaultRuntimeName = children[0];
			}
		} catch (BackingStoreException e) {
			e.printStackTrace();
		}

		IPreferencesService ps = Platform.getPreferencesService();
		String defName = ps.getString(ErlangLaunchPlugin.PLUGIN_ID,
				"default_name", null, null);
		final RuntimeInfo runtime = getRuntime(defName);
		if (defName != null && runtime == null) {
			System.out.println("!!! " + defName);
			RuntimeInfo rt = new RuntimeInfo();
			rt.setName(defName);
			String path = ps.getString(ErlangLaunchPlugin.PLUGIN_ID, "default_"
					+ RuntimeInfo.CODE_PATH, "", null);
			rt.setCodePath(PreferencesUtils.unpackList(path));
			rt.setOtpHome(ps.getString(ErlangLaunchPlugin.PLUGIN_ID, "default_"
					+ RuntimeInfo.HOME_DIR, "", null));
			rt.setArgs(ps.getString(ErlangLaunchPlugin.PLUGIN_ID, "default_"
					+ RuntimeInfo.ARGS, "", null));
			String wd = ps.getString(ErlangLaunchPlugin.PLUGIN_ID, "default_"
					+ RuntimeInfo.WORKING_DIR, "", null);
			if (wd.length() != 0) {
				rt.setWorkingDir(wd);
			}
			rt.setManaged(ps.getBoolean(ErlangLaunchPlugin.PLUGIN_ID,
					"default_" + RuntimeInfo.MANAGED, true, null));
			addRuntime(rt);
			setDefaultRuntime(defName);
			setErlideRuntime(getDefaultRuntime());
		} else if (defName != null) {
			setDefaultRuntime(defName);
			setErlideRuntime(getDefaultRuntime());
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

		notifyListeners();
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
		notifyListeners();
	}

	public void addRuntime(RuntimeInfo rt) {
		fRuntimes.put(rt.getName(), rt);
		notifyListeners();
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
		notifyListeners();
	}

	public String getDefaultRuntimeName() {
		return this.defaultRuntimeName;
	}

	public void setDefaultRuntime(String name) {
		this.defaultRuntimeName = name;
		notifyListeners();
	}

	public void setErlideRuntime(RuntimeInfo runtime) {
		if (runtime != null) {
			runtime.setNodeName("erlide");
		}
		RuntimeInfo old = this.erlideRuntime;
		if (old == null || !old.equals(runtime)) {
			this.erlideRuntime = runtime;
			notifyListeners();
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

	public void preferenceChange(PreferenceChangeEvent event) {
		if (event.getNode().absolutePath().contains("org.erlide")) {
			load();
		}
	}

	public void addListener(RuntimeInfoListener listener) {
		if (!fListeners.contains(listener)) {
			fListeners.add(listener);
		}
	}

	public void removeListener(RuntimeInfoListener listener) {
		fListeners.remove(listener);
	}

	private void notifyListeners() {
		for (RuntimeInfoListener listener : fListeners) {
			listener.infoChanged();
		}
	}
}
