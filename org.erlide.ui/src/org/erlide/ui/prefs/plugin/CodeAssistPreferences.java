package org.erlide.ui.prefs.plugin;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.erlide.core.ErlangCore;
import org.erlide.core.internal.model.erlang.PreferencesHelper;
import org.osgi.service.prefs.BackingStoreException;

public class CodeAssistPreferences {

    private static final String QUALIFIER = ErlangCore.PLUGIN_ID
            + "/codeassist";

    private final PreferencesHelper helper;

    private boolean autoActivate;
    private int delayInMS;
    private String erlangTriggers;
    private String eDocTriggers;

    public static IEclipsePreferences getNode() {
        final IScopeContext context = new InstanceScope();
        final IEclipsePreferences eclipsePreferences = context
                .getNode(QUALIFIER);
        return eclipsePreferences;
    }

    public static CodeAssistPreferences get() throws CoreException {
        final CodeAssistPreferences prefs = new CodeAssistPreferences();
        try {
            prefs.load();
        } catch (final BackingStoreException e1) {
            e1.printStackTrace();
            throw new CoreException(
                    new Status(IStatus.ERROR, ErlangCore.PLUGIN_ID,
                            "could not retrieve compiler options"));
        }
        return prefs;
    }

    public CodeAssistPreferences() {
        helper = PreferencesHelper.getHelper(QUALIFIER);
    }

    public boolean hasOptionsAtLowestScope() {
        return helper.hasAnyAtLowestScope();
    }

    public void store() throws BackingStoreException {
        helper.putBoolean(CodeAssistPreferencesConstants.AUTO_ACTIVATE,
                autoActivate);
        helper.putInt(CodeAssistPreferencesConstants.DELAY_IN_MS, delayInMS);
        helper.putString(CodeAssistPreferencesConstants.ERLANG_TRIGGERS,
                erlangTriggers);
        helper.putString(CodeAssistPreferencesConstants.EDOC_TRIGGERS,
                eDocTriggers);
        helper.flush();
    }

    @SuppressWarnings("boxing")
    public void load() throws BackingStoreException {
        autoActivate = helper.getBoolean(
                CodeAssistPreferencesConstants.AUTO_ACTIVATE, true);
        delayInMS = helper.getInt(CodeAssistPreferencesConstants.DELAY_IN_MS,
                100);
        erlangTriggers = helper.getString(
                CodeAssistPreferencesConstants.ERLANG_TRIGGERS, ":#?");
        eDocTriggers = helper.getString(
                CodeAssistPreferencesConstants.EDOC_TRIGGERS, "");
    }

    @Override
    public String toString() {
        return "auto " + (autoActivate ? "on" : "off") + " " + delayInMS + " "
                + erlangTriggers + " " + eDocTriggers;
    }

    public boolean isAutoActivate() {
        return autoActivate;
    }

    public void setAutoActivate(final boolean autoActivate) {
        this.autoActivate = autoActivate;
    }

    public int getDelayInMS() {
        return delayInMS;
    }

    public void setDelayInMS(final int delayInMS) {
        this.delayInMS = delayInMS;
    }

    public String getErlangTriggers() {
        return erlangTriggers;
    }

    public void setErlangTriggers(final String erlangTriggers) {
        this.erlangTriggers = erlangTriggers;
    }

    public String geteDocTriggers() {
        return eDocTriggers;
    }

    public void seteDocTriggers(final String eDocTriggers) {
        this.eDocTriggers = eDocTriggers;
    }

}
