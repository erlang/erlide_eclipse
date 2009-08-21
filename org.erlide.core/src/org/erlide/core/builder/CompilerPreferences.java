package org.erlide.core.builder;

import java.util.HashMap;
import java.util.Map;

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
	private final Map<String, Boolean> booleanOptions = new HashMap<String, Boolean>();
	private Boolean warnFormat;

	public CompilerPreferences() {
		helper = new PreferencesHelper(QUALIFIER);
	}

	public CompilerPreferences(final IProject project) {
		helper = new PreferencesHelper(QUALIFIER, project);
	}

	public boolean hasOptionsAtLowestScope() {
		return helper.hasAnyAtLowestScope();
	}

	public void store() throws BackingStoreException {
		helper.putString(CompilerPreferencesConstants.ALL_OPTIONS, allOptions);
		for (final Map.Entry<String, Boolean> i : booleanOptions.entrySet()) {
			helper.putBoolean(i.getKey(), i.getValue());
		}
		helper.putBoolean(CompilerPreferencesConstants.WARN_FORMAT_STRINGS,
				warnFormat);
		// FIXME
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
		final String string = helper.getString(
				CompilerPreferencesConstants.ALL_OPTIONS, "");
		setAllOptions(string);
		getBooleanFromHelper(CompilerPreferencesConstants.DEBUG_INFO);
		getBooleanFromHelper(CompilerPreferencesConstants.EXPORT_ALL);
		warnFormat = helper.getBoolean(
				CompilerPreferencesConstants.WARN_FORMAT_STRINGS, true);
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

	private void getBooleanFromHelper(final String optionKey) {
		booleanOptions.put(optionKey, helper.getBoolean(optionKey, false));
	}

	public void setAllOptions(final String string) {
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
		final StringBuilder sb = new StringBuilder();
		for (final Map.Entry<String, Boolean> i : booleanOptions.entrySet()) {
			if (i.getValue()) {
				sb.append(i.getKey());
				sb.append(", ");
			}
		}
		sb.append("{warn_format, ").append(warnFormat ? "1" : "0").append("}");
		final Backend b = ErlangCore.getBackendManager().getIdeBackend();
		if (!allOptions.equals("")) {
			sb.append("|").append(allOptions);
		}
		OtpErlangObject term = null;
		final String s = sb.toString();
		try {
			term = ErlBackend.parseTerm(b, "[" + s + "].");
		} catch (final BackendException e1) {
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

	public boolean getBooleanOption(final String optionKey) {
		if (optionKey.equals(CompilerPreferencesConstants.WARN_FORMAT_STRINGS)) {
			return warnFormat;
		} else if (optionKey
				.equals(CompilerPreferencesConstants.WARN_DEPRECATED_FUNCTIONS)) {
			final Boolean b = booleanOptions.get("nowarn_deprecated_function");
			return b == null ? true : !b;
		} else {
			final Boolean b = booleanOptions.get(optionKey);
			return b == null ? false : b;
		}
	}

	public void setBooleanOption(final String optionKey, final boolean b) {
		if (optionKey.equals(CompilerPreferencesConstants.WARN_FORMAT_STRINGS)) {
			warnFormat = b;
		} else if (optionKey
				.equals(CompilerPreferencesConstants.WARN_DEPRECATED_FUNCTIONS)) {
			booleanOptions.put("nowarn_deprecated_function", !b);
		} else {
			booleanOptions.put(optionKey, b);
		}
	}

	// public void setWarnModuleNotOnSourcePath(boolean
	// warnModuleNotOnSourcePath) {
	// this.warnModuleNotOnSourcePath = warnModuleNotOnSourcePath;
	// }
	//
	// public boolean doWarnModuleNotOnSourcePath() {
	// return warnModuleNotOnSourcePath;
	// }

	public void removeAllAtLowestScope() {
		helper.removeAllAtLowestScope();
	}
}
