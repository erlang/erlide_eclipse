package org.erlide.core.builder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.erlide.core.ErlangPlugin;
import org.erlide.core.preferences.PreferencesHelper;
import org.osgi.service.prefs.BackingStoreException;

public class CompilerPreferences {

	private static final String QUALIFIER = ErlangPlugin.PLUGIN_ID
			+ "/compiler";
	private boolean debugInfo;
	private boolean useExportAll;
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

	public void store() throws BackingStoreException {
		// FIXME
		final PreferencesHelper helper = new PreferencesHelper(QUALIFIER);
		helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
				hasDebugInfo());
		helper.putBoolean(CompilerPreferencesConstants.USE_EXPORT_ALL,
				useExportAll());
		helper.putBoolean(
				CompilerPreferencesConstants.WARN_MODULE_NOT_ON_SOURCE_PATH,
				doWarnModuleNotOnSourcePath());
		// helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
		// warnFormat);
		// helper
		// .putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
		// warnBifClash);
		// helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
		// warnExportAll);
		// helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
		// warnExportVars);
		// helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
		// warnShadowVars);
		// helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
		// warnUnusedFunction);
		// helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
		// warnDeprecatedFunction);
		// helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
		// warnObsoleteGuard);
		// helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
		// warnUnusedImport);
		// helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
		// warnUnusedVars);
		// helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
		// warnUnusedRecord);

		helper.flush();
	}

	public void load() throws BackingStoreException {

		// FIXME

		final PreferencesHelper helper = new PreferencesHelper(QUALIFIER);
		setDebugInfo(helper.getBoolean(CompilerPreferencesConstants.DEBUG_INFO,
				true));
		setUseExportAll(helper.getBoolean(
				CompilerPreferencesConstants.USE_EXPORT_ALL, false));
		setWarnModuleNotOnSourcePath(helper.getBoolean(
				CompilerPreferencesConstants.WARN_MODULE_NOT_ON_SOURCE_PATH,
				true));
		// warnFormat =
		// helper.getBoolean(CompilerPreferencesConstants.DEBUG_INFO,
		// true);
		// warnBifClash = helper.getBoolean(
		// CompilerPreferencesConstants.DEBUG_INFO, true);
		// warnExportAll = helper.getBoolean(
		// CompilerPreferencesConstants.DEBUG_INFO, true);
		// warnExportVars = helper.getBoolean(
		// CompilerPreferencesConstants.DEBUG_INFO, true);
		// warnShadowVars = helper.getBoolean(
		// CompilerPreferencesConstants.DEBUG_INFO, true);
		// warnUnusedFunction = helper.getBoolean(
		// CompilerPreferencesConstants.DEBUG_INFO, true);
		// warnDeprecatedFunction = helper.getBoolean(
		// CompilerPreferencesConstants.DEBUG_INFO, true);
		// warnObsoleteGuard = helper.getBoolean(
		// CompilerPreferencesConstants.DEBUG_INFO, true);
		// warnUnusedImport = helper.getBoolean(
		// CompilerPreferencesConstants.DEBUG_INFO, true);
		// warnUnusedVars = helper.getBoolean(
		// CompilerPreferencesConstants.DEBUG_INFO, true);
		// warnUnusedRecord = helper.getBoolean(
		// CompilerPreferencesConstants.DEBUG_INFO, true);
	}

	public void setUseExportAll(boolean useExportAll) {
		this.useExportAll = useExportAll;
	}

	public boolean useExportAll() {
		return useExportAll;
	}

	public void setDebugInfo(boolean debugInfo) {
		this.debugInfo = debugInfo;
	}

	public boolean hasDebugInfo() {
		return debugInfo;
	}

	public String[] export() {
		List<String> result = new ArrayList<String>();
		if (debugInfo) {
			result.add("debug_info");
		}
		if (useExportAll) {
			result.add("export_all");
		}
		return result.toArray(new String[result.size()]);
	}

	@Override
	public String toString() {
		return Arrays.toString(export());
	}

	public void setWarnModuleNotOnSourcePath(boolean warnModuleNotOnSourcePath) {
		this.warnModuleNotOnSourcePath = warnModuleNotOnSourcePath;
	}

	public boolean doWarnModuleNotOnSourcePath() {
		return warnModuleNotOnSourcePath;
	}

}
