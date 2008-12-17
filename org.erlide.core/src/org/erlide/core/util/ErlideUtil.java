/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *     Jakob C
 *******************************************************************************/
package org.erlide.core.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Enumeration;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.RegistryFactory;
import org.eclipse.osgi.framework.internal.core.BundleURLConnection;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.IErlModule.ModuleKind;
import org.erlide.jinterface.ICodeBundle;
import org.erlide.jinterface.InterfacePlugin;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.runtime.PreferencesUtils;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

@SuppressWarnings("restriction")
public class ErlideUtil {

	public static boolean isAccessible(final IBackend backend,
			final String localDir) {
		try {
			final OtpErlangObject r = backend.rpcx("file", "read_file_info",
					"s", localDir);
			final OtpErlangTuple result = (OtpErlangTuple) r;
			final String tag = ((OtpErlangAtom) result.elementAt(0))
					.atomValue();
			if ("ok".equals(tag)) {
				final OtpErlangTuple info = (OtpErlangTuple) result
						.elementAt(1);
				final String access = info.elementAt(3).toString();
				final int mode = ((OtpErlangLong) info.elementAt(7)).intValue();
				return (access.equals("read") || access.equals("read_write"))
						&& (mode & 4) == 4;
			} else {
				return false;
			}

		} catch (final RpcException e) {
			ErlLogger.error(e.getMessage());
		} catch (final BackendException e) {
			ErlLogger.error(e.getMessage());
		} catch (final OtpErlangRangeException e) {
			ErlLogger.error(e);
		}
		return false;
	}

	public static void unpackBeamFiles(final ICodeBundle p) {
		final String location = p.getEbinDir();
		if (location == null) {
			ErlLogger.warn("Could not find 'ebin' in bundle %s.", p.getBundle()
					.getSymbolicName());
			return;
		}
		final File ebinDir = new File(location + "/ebin");
		ebinDir.mkdirs();
		for (final String fn : ebinDir.list()) {
			if (fn.charAt(0) == '.') {
				continue;
			}
			final File b = new File(fn);
			b.delete();
		}

		final Bundle b = p.getBundle();
		ErlLogger.debug("unpacking plugin " + b.getSymbolicName() + " in "
				+ location);

		// TODO Do we have to also check any fragments?
		// see FindSupport.findInFragments

		final IExtensionRegistry reg = RegistryFactory.getRegistry();
		final IConfigurationElement[] els = reg.getConfigurationElementsFor(
				InterfacePlugin.PLUGIN_ID, "codepath");
		for (final IConfigurationElement el : els) {
			final IContributor c = el.getContributor();
			if (c.getName().equals(b.getSymbolicName())) {
				final String dir_path = el.getAttribute("path");
				final Enumeration<?> e = b.getEntryPaths(dir_path);
				if (e == null) {
					ErlLogger.debug("* !!! error loading plugin "
							+ b.getSymbolicName());
					return;
				}
				while (e.hasMoreElements()) {
					final String s = (String) e.nextElement();
					final Path path = new Path(s);
					if (path.getFileExtension() != null
							&& "beam".compareTo(path.getFileExtension()) == 0) {
						final String m = path.removeFileExtension()
								.lastSegment();
						final URL url = b.getEntry(s);
						ErlLogger.debug(" unpack: " + m);
						final File beam = new File(ebinDir, m + ".erl");
						try {
							beam.createNewFile();
							final FileOutputStream fs = new FileOutputStream(
									beam);
							try {
								final OtpErlangBinary bin = getBeamBinary(m,
										url);
								fs.write(bin.binaryValue());
							} finally {
								fs.close();
							}
						} catch (final IOException e1) {
							ErlLogger.warn(e1);
						}
					}
				}
			}
		}

	}

	public static OtpErlangBinary getBeamBinary(final String moduleName,
			final URL beamPath) {
		try {
			final FileInputStream s = (FileInputStream) beamPath.openStream();
			final int sz = (int) s.getChannel().size();
			final byte buf[] = new byte[sz];
			try {
				s.read(buf);
				return new OtpErlangBinary(buf);
			} finally {
				s.close();
			}
		} catch (final IOException e) {
			ErlLogger.warn(e);
			return null;
		}
	}

	public static String getPath(final String name, final Bundle b) {
		final URL entry = b.getEntry(name.replace(" ", "%20"));
		if (entry != null) {
			URLConnection connection;
			try {
				connection = entry.openConnection();
				if (connection instanceof BundleURLConnection) {
					final URL fileURL = ((BundleURLConnection) connection)
							.getFileURL();
					final URI uri = new URI(fileURL.toString());
					final String path = new File(uri).getAbsolutePath();
					return path;
				}
			} catch (final IOException e) {
				ErlLogger.warn(e.getMessage());
			} catch (final URISyntaxException e) {
				ErlLogger.warn(e.getMessage());
			}
		}
		return null;
	}

	public static String getEbinDir(final Bundle bundle) {
		return getPath("ebin", bundle);
	}

	public static boolean isDeveloper() {
		final String dev = System.getProperty("erlide.devel");
		return dev != null && "true".equals(dev);
	}

	public static boolean isTest() {
		final String test = System.getProperty("erlide.test");
		return test != null && "true".equals(test);
	}

	public static boolean isEricssonUser() {
		final String dev = System.getProperty("erlide.ericsson.user");
		if (dev == null || !"true".equals(dev)) {
			return false;
		}
		String s;
		if (System.getProperty("os.name").toLowerCase().contains("windows")) {
			s = "\\\\projhost\\tecsas\\shade\\erlide";
		} else {
			s = "/proj/tecsas/SHADE/erlide";
		}
		return new File(s).exists();
	}

	public static boolean isModuleExtension(final String ext) {
		return extensionToModuleKind(ext) != ModuleKind.BAD;
	}

	public static ModuleKind extensionToModuleKind(final String ext) {
		if (ext == null) {
			return ModuleKind.BAD;
		}
		if (ext.equalsIgnoreCase("hrl")) {
			return ModuleKind.HRL;
		}
		if (ext.equalsIgnoreCase("erl")) {
			return ModuleKind.ERL;
		}
		if (ext.equalsIgnoreCase("yrl")) {
			return ModuleKind.YRL;
		}
		return ModuleKind.BAD;
	}

	public static ModuleKind nameToModuleKind(final String name) {
		final IPath p = new Path(name);
		return extensionToModuleKind(p.getFileExtension());
	}

	public static boolean hasModuleExtension(final String name) {
		return nameToModuleKind(name) != ModuleKind.BAD;
	}

	public static boolean hasExtension(final String name) {
		final int i = name.lastIndexOf('.');
		return i != -1;
	}

	public static String withoutExtension(final String name) {
		final int i = name.lastIndexOf('.');
		if (i == -1) {
			return name;
		}
		return name.substring(0, i);
	}

	public static boolean hasERLExtension(final String name) {
		return nameToModuleKind(name) == ModuleKind.ERL;
	}

	/**
	 * Returns true if the given project is accessible and it has a Erlang
	 * nature, otherwise false.
	 * 
	 * @param project
	 *            IProject
	 * @return boolean
	 */
	public static boolean hasErlangNature(final IProject project) {
		if (project != null) {
			try {
				return project.hasNature(ErlangPlugin.NATURE_ID);
			} catch (final CoreException e) {
				// project does not exist or is not open
			}
		}
		return false;
	}

	public static boolean isOnSourcePathOrParentToFolderOnSourcePath(
			final IFolder folder) {
		final IProject project = folder.getProject();
		final IPath folderPath = folder.getFullPath();
		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				project);
		final List<String> sourcePaths = PreferencesUtils.unpackList(prefs
				.getSourceDirsString());
		for (final String p : sourcePaths) {
			final IPath path = project.getFolder(p).getFullPath();
			if (folderPath.isPrefixOf(path)) {
				return true;
			}
		}
		return false;
	}

	public static String basenameWithoutExtension(final String m) {
		final IPath p = new Path(m);
		return withoutExtension(p.lastSegment());
	}
}
