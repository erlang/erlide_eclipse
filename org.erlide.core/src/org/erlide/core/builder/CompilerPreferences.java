package org.erlide.core.builder;

import org.eclipse.core.resources.IProject;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.preferences.PreferencesHelper;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.ErlBackend;
import org.osgi.service.prefs.BackingStoreException;

import com.ericsson.otp.erlang.JInterfaceFactory;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class CompilerPreferences {

	private static final String QUALIFIER = ErlangPlugin.PLUGIN_ID
			+ "/compiler";
	// private boolean debugInfo;
	// private boolean useExportAll;
	// private boolean warnModuleNotOnSourcePath;
	// private boolean warnFormat;
	// private boolean warnBifClash;
	// private boolean warnExportAll;
	// private boolean warnExportVars;
	// private boolean warnShadowVars;
	// private boolean warnUnusedFunction;
	// private boolean warnDeprecatedFunction;
	// private boolean warnObsoleteGuard;
	// private boolean warnUnusedImport;
	// private boolean warnUnusedVars;
	// private boolean warnUnusedRecord;
	private String allOptions = "";
	private final PreferencesHelper helper;

	public CompilerPreferences() {
		helper = new PreferencesHelper(QUALIFIER);
	}

	public CompilerPreferences(IProject project) {
		helper = new PreferencesHelper(QUALIFIER, project);
	}

	public void store() throws BackingStoreException {
		// FIXME
		helper.putString(CompilerPreferencesConstants.ALL_OPTIONS, allOptions);

		// helper.putBoolean(CompilerPreferencesConstants.DEBUG_INFO,
		// hasDebugInfo());
		// helper.putBoolean(CompilerPreferencesConstants.USE_EXPORT_ALL,
		// useExportAll());
		// helper.putBoolean(
		// CompilerPreferencesConstants.WARN_MODULE_NOT_ON_SOURCE_PATH,
		// doWarnModuleNotOnSourcePath());
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
		String string = helper.getString(
				CompilerPreferencesConstants.ALL_OPTIONS, "");
		setAllOptions(string);

		// setDebugInfo(helper.getBoolean(CompilerPreferencesConstants.DEBUG_INFO,
		// true));
		// setUseExportAll(helper.getBoolean(
		// CompilerPreferencesConstants.USE_EXPORT_ALL, false));
		// setWarnModuleNotOnSourcePath(helper.getBoolean(
		// CompilerPreferencesConstants.WARN_MODULE_NOT_ON_SOURCE_PATH,
		// true));
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

	public void setAllOptions(String string) {
		allOptions = string;
	}

	public String getAllOptions() {
		return allOptions;
	}

	// public void setUseExportAll(boolean useExportAll) {
	// this.useExportAll = useExportAll;
	// }
	//
	// public boolean useExportAll() {
	// return useExportAll;
	// }
	//
	// public void setDebugInfo(boolean debugInfo) {
	// this.debugInfo = debugInfo;
	// }
	//
	// public boolean hasDebugInfo() {
	// return debugInfo;
	// }

	public OtpErlangList export() {
		// if (debugInfo) {
		// result.add("debug_info");
		// }
		// if (useExportAll) {
		// result.add("export_all");
		// }

		OtpErlangObject term = null;
		Backend b = ErlangCore.getBackendManager().getIdeBackend();
		if (!allOptions.equals("")) {
			try {
				term = ErlBackend.parseTerm(b, allOptions + ".");
			} catch (BackendException e) {
				try {
					term = ErlBackend.parseTerm(b, "[" + allOptions + "].");
				} catch (BackendException e1) {
				}
			}
		}

		OtpErlangList list;
		if (term instanceof OtpErlangList) {
			list = (OtpErlangList) term;
		} else if (term == null) {
			list = new OtpErlangList();
		} else {
			list = JInterfaceFactory.mkList(term);
		}

		return list;
	}

	@Override
	public String toString() {
		return export().toString();
	}

	// public void setWarnModuleNotOnSourcePath(boolean
	// warnModuleNotOnSourcePath) {
	// this.warnModuleNotOnSourcePath = warnModuleNotOnSourcePath;
	// }
	//
	// public boolean doWarnModuleNotOnSourcePath() {
	// return warnModuleNotOnSourcePath;
	// }

}
