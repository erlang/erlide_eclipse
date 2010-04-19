package org.erlide.wrangler.refactoring;

import java.io.IOException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.ErlideBackend;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	/**
	 * The plug-in ID.
	 */
	public static final String PLUGIN_ID = "org.erlide.wrangler.refactoring";

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

	private void initWrangler() throws CoreException { /*
														 * try { Path pluginPath
														 * = getPluginPath();
														 * IPath
														 * wranglerRootPath =
														 * pluginPath
														 * .append("wrangler");
														 * String
														 * wranglerEbinPath =
														 * wranglerRootPath
														 * .append("ebin")
														 * .toOSString(); String
														 * wranglerAppPath =
														 * wranglerRootPath
														 * .append("app")
														 * .toOSString();
														 * 
														 * String
														 * wranglerSrcPath =
														 * wranglerRootPath
														 * .append("erl")
														 * .toOSString();
														 * 
														 * ErlLogger.debug(
														 * "Wrangler beam files found at: "
														 * + wranglerEbinPath);
														 */
		ErlideBackend mb = ErlangCore.getBackendManager().getIdeBackend();

		ErlLogger.debug("Managed backend found:" + mb.getJavaNodeName());

		/*
		 * ErlangCode.addPathA(mb , wranglerEbinPath); ErlangCode .addPathA(mb,
		 * wranglerSrcPath); ErlangCode .addPathA(mb, wranglerAppPath);
		 * ErlLogger.debug( "Wrangler path has been added." );
		 * 
		 * RpcResult res = mb.call_noexception ("code", "load_file", "a",
		 * "wrangler"); res = mb.call_noexception( "code", "load_file", "a",
		 * "refac_util"); ErlLogger.debug(
		 * "Wrangler's path is added to Erlang with result:" + res.isOk() +
		 * "\t raw:" + res);
		 */
		RpcResult res = mb.call_noexception("wrangler", "init_eclipse", "",
				new Object[0]);
		/*
		 * application :start(wrangler_app) res = mb.call_noexception
		 * ("application", "start", "a", "wrangler_app");
		 */
		ErlLogger.debug("Wrangler app started:\n" + res);
		/*
		 * } catch (IOException ioe) { ioe.printStackTrace (); throw new
		 * CoreException(new Status(IStatus.ERROR, PLUGIN_ID,
		 * "Could not load the ebin files!" )); } catch (Exception e) {
		 * e.printStackTrace(); } }
		 */
	}

	/**
	 * Returns with the Wrangler integration plugin path
	 * 
	 * @return path of the plugin
	 * @throws IOException
	 */
	/*
	 * public static Path getPluginPath() throws IOException { URL url; Bundle b
	 * = getDefault().getBundle(); url = FileLocator.find(b, new Path(""),
	 * null); url = FileLocator.resolve(url);
	 * 
	 * ErlLogger.debug("Wrangler installation found at: " + url);
	 * 
	 * Path pluginPath = new Path(url.getPath()); return pluginPath; }
	 */

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
