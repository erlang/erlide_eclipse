package org.erlide.runtime.backend.internal;

import org.eclipse.debug.core.ILaunch;

public final class RuntimeLauncherFactory {

	private RuntimeLauncherFactory() {
	}

	public static RuntimeLauncher createManagedLauncher(final ILaunch launch) {
		ManagedLauncher launcher = new ManagedLauncher();
		launcher.initializeRuntime(launch);
		return launcher;
	}

	public static RuntimeLauncher createStandaloneLauncher(final ILaunch launch) {
		StandaloneLauncher launcher = new StandaloneLauncher();
		launcher.initializeRuntime(launch);
		return launcher;
	}

}
