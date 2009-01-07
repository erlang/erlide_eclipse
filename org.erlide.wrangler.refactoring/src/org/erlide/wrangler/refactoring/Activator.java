package org.erlide.wrangler.refactoring;

import java.io.IOException;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IdeBackend;
import org.erlide.runtime.backend.RpcResult;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import erlang.ErlangCode;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	/**
	 * The plug-in ID.
	 */
	public static final String PLUGIN_ID = "org.erlide.wranglerrefactoring";

	// The shared instance
	private static Activator plugin;

	/**
	 * Returns the shared instance
	 * 
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

	/**
	 * The constructor
	 */
	public Activator() {
	}

	/**
	 * Loads the necessary *.ebin files to the Erlang node for the plug-in.
	 * 
	 * @throws CoreException
	 *             detailed exception about the loading process errors
	 */
	private void initWrangler() throws CoreException {
		URL url;
		try {
			Bundle b = getDefault().getBundle();
			url = FileLocator.find(b, new Path(""), null);
			url = FileLocator.resolve(url);

			ErlLogger.debug("Wrangler installation found at: " + url);

			String wranglerPath = new Path(url.getPath()).append("wrangler")
					.append("ebin").toOSString();

			ErlLogger.debug("Wrangler beam files found at: " + wranglerPath);

			IdeBackend mb = BackendManager.getDefault().getIdeBackend();

			ErlLogger.debug("Managed backend found:" + mb.getJavaNodeName());

			ErlangCode.addPathA(mb, wranglerPath);
			RpcResult res = mb.rpc("code", "load_file", "a", "wrangler");
			ErlLogger.debug("Wrangler's path is added to Erlang with result:"
					+ res.isOk() + "\t raw:" + res);
		} catch (IOException ioe) {
			ioe.printStackTrace();
			throw new CoreException(new Status(IStatus.ERROR, PLUGIN_ID,
					"Could not load the ebin files!"));
		} catch (RpcException e) {
			e.printStackTrace();
			throw new CoreException(new Status(IStatus.ERROR, PLUGIN_ID,
					"Could not reach the erlang node!"));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext
	 * )
	 */
	@Override
	public void start(final BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		initWrangler();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext
	 * )
	 */
	@Override
	public void stop(final BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

}
