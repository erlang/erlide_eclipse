package org.erlide.ui.util;

import org.erlide.core.ErlangCore;
import org.erlide.core.internal.model.erlang.PreferencesHelper;

public class PerformanceTuning {

    private static final String QUALIFIER = ErlangCore.PLUGIN_ID
            + "/performance";
    private final PreferencesHelper helper;

    /**
     * Disable folding for files with more lines than this.
     */
    private static final int DEFAULT_FOLDING_LIMIT = 2000;
    private static final String FOLDING_DISABLE_LIMIT_KEY = "perf.limit.folding";

    private int foldingLimit = DEFAULT_FOLDING_LIMIT;

    public static PerformanceTuning get() {
        final PerformanceTuning prefs = new PerformanceTuning();
        prefs.load();
        return prefs;
    }

    public static PerformanceTuning getDefault() {
        final PerformanceTuning result = new PerformanceTuning();
        result.setFoldingLimit(DEFAULT_FOLDING_LIMIT);
        return result;
    }

    public PerformanceTuning() {
        helper = PreferencesHelper.getHelper(QUALIFIER);
    }

    public void load() {
        foldingLimit = helper.getInt(FOLDING_DISABLE_LIMIT_KEY,
                DEFAULT_FOLDING_LIMIT);
    }

    public void store() {
        helper.putInt(FOLDING_DISABLE_LIMIT_KEY, foldingLimit);
    }

    public int getFoldingLimit() {
        return foldingLimit;
    }

    public void setFoldingLimit(final int foldingLimit) {
        this.foldingLimit = foldingLimit;
    }

}
