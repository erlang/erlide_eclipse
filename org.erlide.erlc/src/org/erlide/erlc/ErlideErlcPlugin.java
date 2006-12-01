package org.erlide.erlc;

import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Preferences;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.erlide.core.ErlangPlugin;
import org.erlide.erlc.core.IMakeBuilderInfo;
import org.erlide.erlc.core.resources.IConsole;
import org.erlide.erlc.errorparsing.ErlcErrorParser;
import org.erlide.erlc.errorparsing.IErrorParser;
import org.erlide.erlc.internal.core.BuildInfoFactory;
import org.erlide.erlc.internal.ui.buildconsole.BuildConsoleManager;
import org.erlide.erlc.internal.ui.buildconsole.ErlcBuildConsole;
import org.erlide.erlc.ui.IBuildConsoleManager;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class ErlideErlcPlugin extends AbstractUIPlugin {

	public static final String PLUGIN_ID = "org.erlide.erlc";

	// The shared instance.
	private static ErlideErlcPlugin plugin;

	private ResourceBundle resourceBundle;

	private BuildConsoleManager fBuildConsoleManager = null;

	private IConsole fConsole;

	/**
	 * The constructor.
	 */
	public ErlideErlcPlugin() {
		try {
			resourceBundle = ResourceBundle
					.getBundle("org.erlide.erlc.internal.ui.MakeResources"); //$NON-NLS-1$
		} catch (final MissingResourceException x) {
			resourceBundle = null;
		}
		plugin = this;
		fConsole = null;
	}

	/**
	 * This method is called upon plug-in activation
	 */
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
	}

	/**
	 * This method is called when the plug-in is stopped
	 */
	@Override
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		plugin = null;
	}

	public static String getUniqueIdentifier() {
		if (getDefault() == null) {
			// If the default instance is not yet initialized,
			// return a static identifier. This identifier must
			// match the plugin id defined in plugin.xml
			return PLUGIN_ID;
		}
		return getDefault().getBundle().getSymbolicName();
	}

	public static IMakeBuilderInfo createBuildInfo(IProject project,
			String builderID) throws CoreException {
		return BuildInfoFactory.create(project, builderID);
	}

	public static IMakeBuilderInfo createBuildInfo(Map<String,String> args, String builderID) {
		return BuildInfoFactory.create(args, builderID);
	}

	public static IMakeBuilderInfo createBuildInfo(Preferences prefs,
			String builderID, boolean useDefaults) {
		return BuildInfoFactory.create(prefs, builderID, useDefaults);
	}

	/**
	 * Returns the shared instance.
	 */
	public static ErlideErlcPlugin getDefault() {
		return plugin;
	}

	/**
	 * Returns the image descriptor for the given image PLUGIN_ID. Returns null
	 * if there is no such image.
	 * 
	 * @param id
	 *            The image id
	 * 
	 * @return The image descriptor
	 */
	public ImageDescriptor getImageDescriptor(String id) {
		final ImageDescriptor returnImageDescriptor = getImageRegistry()
				.getDescriptor(id);
		return returnImageDescriptor;
	}

	public IErrorParser[] getErrorParser(String string) {
		if (string.equals((ErlcErrorParser.ID))) {
			return new IErrorParser[] { new ErlcErrorParser() };
		}
		// FIXME eller ska vi kr?ngla till det som i CDT?
		return null;
	}

	public String[] getAllErrorParsersIDs() {
		return new String[] { ErlcErrorParser.ID };
		// FIXME kanske fler? l?nka till CDT p? n?t s?tt?
	}

	// /**
	// * @param parserId
	// * @return parser - parser object identified by the parserId
	// */
	// public IScannerInfoConsoleParser getScannerInfoConsoleParser(String
	// parserId) {
	// try {
	// IExtensionPoint extension =
	// Platform.getExtensionRegistry().getExtensionPoint(PLUGIN_ID,
	// SI_CONSOLE_PARSER_SIMPLE_ID);
	// if (extension != null) {
	// IExtension[] extensions = extension.getExtensions();
	// for (int i = 0; i < extensions.length; i++) {
	// String id = extensions[i].getUniqueIdentifier();
	// if (id != null && id.equals(parserId)) {
	// IConfigurationElement[] configElements =
	// extensions[i].getConfigurationElements();
	// IScannerInfoConsoleParser parser =
	// (IScannerInfoConsoleParser)configElements[0].createExecutableExtension("class");//$NON-NLS-1$
	// return parser;
	// }
	// }
	// }
	// }
	// catch (CoreException e) {
	// ErlangPlugin.log(e);
	// }
	// return null;
	// }

	/**
	 * Returns the plugin's resource bundle,
	 * 
	 * @return The requested bundle
	 */
	public ResourceBundle getResourceBundle() {
		return resourceBundle;
	}

	public IBuildConsoleManager getConsoleManager() {
		if (fBuildConsoleManager == null) {
			fBuildConsoleManager = new BuildConsoleManager();
			fBuildConsoleManager.startup();
		}
		return fBuildConsoleManager;
	}

	/**
	 * Returns the standard display to be used. The method first checks, if the
	 * thread calling this method has an associated display. If so, this display
	 * is returned. Otherwise the method returns the default display.
	 * 
	 * @return the standard display
	 */
	public static Display getStandardDisplay() {
		Display display = Display.getCurrent();
		if (display == null) {
			display = Display.getDefault();
		}

		return display;
	}

	/**
	 * Returns the string from the plugin's resource bundle, or 'key' if not
	 * found.
	 */
	public static String getResourceString(String key) {
		final ResourceBundle bundle = getDefault().getResourceBundle();
		try {
			return bundle.getString(key);
		} catch (final MissingResourceException e) {
			return key;
		}
	}

	/**
	 * @return
	 */
	public static IWorkbenchPage getActivePage() {
		final IWorkbenchWindow w = getActiveWorkbenchWindow();
		if (w != null) {
			return w.getActivePage();
		}
		return null;
	}

	/**
	 * Returns the active workbench shell or <code>null</code> if none
	 * 
	 * @return the active workbench shell or <code>null</code> if none
	 */
	public static Shell getActiveWorkbenchShell() {
		final IWorkbenchWindow window = getActiveWorkbenchWindow();
		if (window != null) {
			return window.getShell();
		}
		return null;
	}

	public static IWorkbenchWindow getActiveWorkbenchWindow() {
		return getDefault().getWorkbench().getActiveWorkbenchWindow();
	}

	public IConsole getConsole() {
		if (fConsole == null) {
			fConsole = new ErlcBuildConsole();
		}
		return fConsole;
	}

	/**
	 * Utility method with conventions
	 * 
	 * @param logError
	 *            TODO
	 */
	public static void errorDialog(Shell shell, String title, String message,
			IStatus s, boolean logError) {
		if (logError) {
			ErlangPlugin.log(s);
		}

		// if the 'message' resource string and the IStatus' message are the
		// same,
		// don't show both in the dialog
		if (s != null && message.equals(s.getMessage())) {
			message = null;
		}
		ErrorDialog.openError(shell, title, message, s);
	}

	/**
	 * Utility method with conventions
	 * 
	 * @param logError
	 *            TODO
	 */
	public static void errorDialog(Shell shell, String title, String message,
			Throwable t, boolean logError) {
		if (logError) {
			ErlangPlugin.log(t);
		}

		IStatus status;
		if (t instanceof CoreException) {
			status = ((CoreException) t).getStatus();
			// if the 'message' resource string and the IStatus' message are the
			// same,
			// don't show both in the dialog
			if (status != null && message.equals(status.getMessage())) {
				message = null;
			}
		} else {
			status = new Status(IStatus.ERROR, PLUGIN_ID, -1,
					"Internal Error: ", t); //$NON-NLS-1$	
		}
		ErrorDialog.openError(shell, title, message, status);
	}
}
