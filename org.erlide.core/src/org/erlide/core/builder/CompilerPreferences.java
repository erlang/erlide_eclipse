package org.erlide.core.builder;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.erlide.core.ErlangPlugin;
import org.osgi.service.prefs.BackingStoreException;

@SuppressWarnings("unused")
public class CompilerPreferences {

	private boolean debugInfo;
	private boolean warnModuleNotOnSourcePath;
	private boolean warnFormat;
	private boolean warnBifClash;
	private boolean warnExportAll;
	private boolean warnExportVars;
	private boolean warnShadowVars;
	private boolean warnUnusedFunction;
	private boolean warnDeprecatedFunction;
	private boolean warnObsoleteGuard;
	private boolean warnUnusedImport;
	private boolean warnUnusedVars;
	private boolean warnUnusedRecord;

	public void store() {
		IEclipsePreferences root = getRootPreferenceNode();
		root.putBoolean(CompilerPreferencesConstants.DEBUG_INFO, debugInfo);
	}

	public void load() throws BackingStoreException {
		IPreferencesService service = Platform.getPreferencesService();
		String s;
	}

	private IEclipsePreferences getRootPreferenceNode() {
		return new InstanceScope()
				.getNode(ErlangPlugin.PLUGIN_ID + "/compiler");
	}

}
