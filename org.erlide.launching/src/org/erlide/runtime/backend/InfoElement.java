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
import java.util.Collections;
import java.util.List;

import org.erlide.basiccore.PreferencesUtils;
import org.osgi.service.prefs.Preferences;

public abstract class InfoElement {
	private static final String CODE_PATH = "codePath";

	private String name;
	private List<String> codePath;

	public void store(Preferences root) {
		Preferences node = root.node(getName());
		String code = PreferencesUtils.packList(getCodePath());
		node.put(CODE_PATH, code);
	}

	public void load(Preferences node) {
		setName(node.name());
		String path = node.get(CODE_PATH, "");
		setCodePath(PreferencesUtils.unpackList(path));
	}

	public InfoElement() {
		codePath = new ArrayList<String>(5);
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public List<String> getCodePath() {
		return codePath;
	}

	public void setCodePath(List<String> path) {
		codePath = path;
	}

	public abstract List<String> getPathA();

	public abstract List<String> getPathZ();

	protected List<String> getPathA(String marker) {
		if (codePath != null) {
			List<String> list = codePath;
			int i = list.indexOf(marker);
			if (i < 0) {
				return list;
			}
			return list.subList(0, i);
		}
		return Collections.emptyList();
	}

	protected List<String> getPathZ(String marker) {
		if (codePath != null) {
			List<String> list = codePath;
			int i = list.indexOf(marker);
			if (i < 0) {
				return Collections.emptyList();
			}
			return list.subList(i + 1, codePath.size());
		}
		return Collections.emptyList();
	}

	protected static String cvt(Collection<String> path) {
		String result = "";
		for (String s : path) {
			if (s.length() > 0) {
				if (s.contains(" ")) {
					s = "\"" + s + "\"";
				}
				result += s + ";";
			}
		}
		return result;
	}

	public String getCmdLine() {
		return null;
	}

}
