/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
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

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.RegistryFactory;
import org.eclipse.osgi.framework.internal.core.BundleURLConnection;
import org.erlide.jinterface.ICodeBundle;
import org.erlide.jinterface.InterfacePlugin;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideUtil {

	public static boolean isAccessible(IBackend backend, String localDir) {
		try {
			OtpErlangObject r = backend.rpcx("file", "read_file_info", "s",
					localDir);
			OtpErlangTuple result = (OtpErlangTuple) r;
			String tag = ((OtpErlangAtom) result.elementAt(0)).atomValue();
			if ("ok".equals(tag)) {
				OtpErlangTuple info = (OtpErlangTuple) result.elementAt(1);
				String access = info.elementAt(3).toString();
				int mode = ((OtpErlangLong) info.elementAt(7)).intValue();
				return (access.equals("read") || access.equals("read_write"))
						&& ((mode & 4) == 4);
			} else {
				return false;
			}

		} catch (final RpcException e) {
			ErlLogger.error(e);
		} catch (final BackendException e) {
			ErlLogger.error(e);
		} catch (OtpErlangRangeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return false;
	}

	public static void unpackBeamFiles(ICodeBundle p) {
		String location = p.getEbinDir();
		File ebinDir = new File(location + "/ebin");
		ebinDir.mkdirs();
		for (String fn : ebinDir.list()) {
			if (fn.charAt(0) == '.') {
				continue;
			}
			File b = new File(fn);
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
				Enumeration e = b.getEntryPaths(dir_path);
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
						URL url = b.getEntry(s);
						ErlLogger.debug(" unpack: " + m);
						File beam = new File(ebinDir, m + ".erl");
						try {
							beam.createNewFile();
							FileOutputStream fs = new FileOutputStream(beam);
							try {
								OtpErlangBinary bin = getBeamBinary(m, url);
								fs.write(bin.binaryValue());
							} finally {
								fs.close();
							}
						} catch (IOException e1) {
							// TODO Auto-generated catch block
							e1.printStackTrace();
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
			e.printStackTrace();
			return null;
		}
	}

	public static String getEbinDir(Bundle b) {
		URL entry = b.getEntry("ebin");
		if (entry != null) {
			URLConnection connection;
			try {
				connection = entry.openConnection();
				if (connection instanceof BundleURLConnection) {
					URL fileURL = ((BundleURLConnection) connection)
							.getFileURL();
					URI uri = new URI(fileURL.toString());
					String path = new File(uri).getAbsolutePath();
					return path;
				}
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return null;
	}

}
