package org.erlide.core.builder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.preferences.PreferencesHelper;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.ErlBackend;
import org.osgi.service.prefs.BackingStoreException;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
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

    public static OtpErlangList get(final IProject project)
            throws CoreException {
        final CompilerPreferences prefs = new CompilerPreferences(project);
        try {
            prefs.load();
        } catch (final BackingStoreException e1) {
            e1.printStackTrace();
            throw new CoreException(new Status(IStatus.ERROR,
                    ErlangPlugin.PLUGIN_ID,
                    "could not retrieve compiler options"));
        }
        final OtpErlangList compilerOptions = prefs.export();
        return compilerOptions;
    }

    public CompilerPreferences() {
        helper = PreferencesHelper.getHelper(QUALIFIER);
    }

    public CompilerPreferences(final IProject project) {
        helper = PreferencesHelper.getHelper(QUALIFIER, project);
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
        // FIXME handle all compiler options
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

    @SuppressWarnings("boxing")
    public void load() throws BackingStoreException {

        // FIXME
        final String string = helper.getString(
                CompilerPreferencesConstants.ALL_OPTIONS, "");
        setAllOptions(string);
        final String[] booleanKeys = { CompilerPreferencesConstants.DEBUG_INFO,
                CompilerPreferencesConstants.EXPORT_ALL,
                CompilerPreferencesConstants.ENCRYPT_DEBUG_INFO,
                CompilerPreferencesConstants.WARN_OBSOLETE_GUARD,
                CompilerPreferencesConstants.NOWARN_DEPRECATED_FUNCTIONS,
                CompilerPreferencesConstants.WARN_EXPORT_ALL,
                CompilerPreferencesConstants.WARN_UNUSED_IMPORT,
                CompilerPreferencesConstants.NOWARN_SHADOW_VARS,
                CompilerPreferencesConstants.NOWARN_BIF_CLASH };
        for (final String key : booleanKeys) {
            getBooleanFromHelper(key);
        }
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

    @SuppressWarnings("boxing")
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
        final List<OtpErlangObject> result = new ArrayList<OtpErlangObject>();
        for (final Map.Entry<String, Boolean> i : booleanOptions.entrySet()) {
            if (i.getValue()) {
                result.add(new OtpErlangAtom(i.getKey()));
            }
        }
        result.add(OtpErlang.mkTuple(new OtpErlangAtom("warn_format"),
                new OtpErlangLong(warnFormat ? 1 : 0)));
        final Backend b = ErlangCore.getBackendManager().getIdeBackend();
        if (!allOptions.equals("")) {
            try {
                final OtpErlangList term = (OtpErlangList) ErlBackend
                        .parseTerm(b, "[" + allOptions + "].");
                result.addAll(Arrays.asList(term.elements()));
            } catch (final BackendException e1) {
            }
        }

        result.add(new OtpErlangAtom("debug_info"));
        final OtpErlangList list = OtpErlang.mkList(result);
        return list;
    }

    @Override
    public String toString() {
        return export().toString();
    }

    public boolean getBooleanOption(final String optionKey) {
        if (optionKey.equals(CompilerPreferencesConstants.WARN_FORMAT_STRINGS)) {
            return warnFormat;
        } else {
            final Boolean b = booleanOptions.get(optionKey);
            return b == null ? false : b;
        }
    }

    public void setBooleanOption(final String optionKey, final boolean b) {
        if (optionKey.equals(CompilerPreferencesConstants.WARN_FORMAT_STRINGS)) {
            warnFormat = b;
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

    public void removeAllProjectSpecificSettings() {
        helper.removeAllAtLowestScope();
    }
}
