package org.erlide.ui.prefs;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.preferences.PreferencesHelper;
import org.osgi.service.prefs.BackingStoreException;

public class DialyzerPreferences {

	private static final String QUALIFIER = ErlangPlugin.PLUGIN_ID
			+ "/dialyzer";

	private final PreferencesHelper helper;

	private String pltPath;

	public static DialyzerPreferences get(final IProject project)
			throws CoreException {
		final DialyzerPreferences prefs = new DialyzerPreferences(project);
		try {
			prefs.load();
		} catch (final BackingStoreException e1) {
			e1.printStackTrace();
			throw new CoreException(new Status(IStatus.ERROR,
					ErlangPlugin.PLUGIN_ID,
					"could not retrieve compiler options"));
		}
		return prefs;
	}

	public DialyzerPreferences() {
		helper = PreferencesHelper.getHelper(QUALIFIER);
	}

	public DialyzerPreferences(final IProject project) {
		helper = PreferencesHelper.getHelper(QUALIFIER, project);
	}

	public boolean hasOptionsAtLowestScope() {
		return helper.hasAnyAtLowestScope();
	}

	public void store() throws BackingStoreException {
		helper.putString(DialyzerPreferencesConstants.PLT_PATH, pltPath);
		helper.flush();
	}

	@SuppressWarnings("boxing")
	public void load() throws BackingStoreException {
		pltPath = helper.getString(DialyzerPreferencesConstants.PLT_PATH, "");
	}

	@Override
	public String toString() {
		return getPltPath().toString();
	}

	public String getPltPath() {
		return pltPath;
	}

	public void removeAllProjectSpecificSettings() {
		helper.removeAllAtLowestScope();
	}

	public void setPltPath(final String text) {
		pltPath = text;
	}
}
