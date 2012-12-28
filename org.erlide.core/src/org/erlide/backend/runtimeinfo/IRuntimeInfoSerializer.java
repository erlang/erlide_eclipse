package org.erlide.backend.runtimeinfo;

import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;

public interface IRuntimeInfoSerializer extends IPreferenceChangeListener {

    public abstract void setOwner(final RuntimeInfoManager runtimeInfoManager);

    public abstract void store();

    public abstract void load();

}
