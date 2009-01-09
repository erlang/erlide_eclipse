package org.erlide.runtime.backend.internal;

public final class RuntimeLauncherFactory {

	private RuntimeLauncherFactory() {
	}

	public static RuntimeLauncher createManagedLauncher() {
		return new ManagedLauncher();
	}

	public static RuntimeLauncher createStandaloneLauncher() {
		return new StandaloneLauncher();
	}

}
