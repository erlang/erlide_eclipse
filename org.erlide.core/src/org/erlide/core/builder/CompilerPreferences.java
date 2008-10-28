package org.erlide.core.builder;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.preferences.PreferencesHelper;
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
		// FIXME
		PreferencesHelper helper = new PreferencesHelper(ErlangPlugin.PLUGIN_ID);
		helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO, debugInfo);
		helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
				warnModuleNotOnSourcePath);
		helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO, warnFormat);
		helper
				.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
						warnBifClash);
		helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
				warnExportAll);
		helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
				warnExportVars);
		helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
				warnShadowVars);
		helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
				warnUnusedFunction);
		helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
				warnDeprecatedFunction);
		helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
				warnObsoleteGuard);
		helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
				warnUnusedImport);
		helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
				warnUnusedVars);
		helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
				warnUnusedRecord);
	}

	public void load() throws BackingStoreException {

		// FIXME

		PreferencesHelper helper = new PreferencesHelper(ErlangPlugin.PLUGIN_ID);
		debugInfo = helper.getBoolean(CompilerPreferencesConstants.DEBUG_INFO,
				true);
		warnModuleNotOnSourcePath = helper.getBoolean(
				CompilerPreferencesConstants.DEBUG_INFO, true);
		warnFormat = helper.getBoolean(CompilerPreferencesConstants.DEBUG_INFO,
				true);
		warnBifClash = helper.getBoolean(
				CompilerPreferencesConstants.DEBUG_INFO, true);
		warnExportAll = helper.getBoolean(
				CompilerPreferencesConstants.DEBUG_INFO, true);
		warnExportVars = helper.getBoolean(
				CompilerPreferencesConstants.DEBUG_INFO, true);
		warnShadowVars = helper.getBoolean(
				CompilerPreferencesConstants.DEBUG_INFO, true);
		warnUnusedFunction = helper.getBoolean(
				CompilerPreferencesConstants.DEBUG_INFO, true);
		warnDeprecatedFunction = helper.getBoolean(
				CompilerPreferencesConstants.DEBUG_INFO, true);
		warnObsoleteGuard = helper.getBoolean(
				CompilerPreferencesConstants.DEBUG_INFO, true);
		warnUnusedImport = helper.getBoolean(
				CompilerPreferencesConstants.DEBUG_INFO, true);
		warnUnusedVars = helper.getBoolean(
				CompilerPreferencesConstants.DEBUG_INFO, true);
		warnUnusedRecord = helper.getBoolean(
				CompilerPreferencesConstants.DEBUG_INFO, true);
	}

	private IEclipsePreferences getRootPreferenceNode() {
		return new InstanceScope()
				.getNode(ErlangPlugin.PLUGIN_ID + "/compiler");
	}

}
